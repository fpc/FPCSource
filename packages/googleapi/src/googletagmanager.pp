unit googletagmanager;
{
  This is the file COPYING.FPC, it applies to the Free Pascal Run-Time Library 
  (RTL) and packages (packages) distributed by members of the Free Pascal 
  Development Team.
  
  The source code of the Free Pascal Runtime Libraries and packages are 
  distributed under the Library GNU General Public License 
  (see the file COPYING) with the following modification:
  
  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,
  and to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a module
  which is not derived from or based on this library. If you modify this
  library, you may extend this exception to your version of the library, but you are
  not obligated to do so. If you do not wish to do so, delete this exception
  statement from your version.
  
  If you didn't receive a copy of the file COPYING, contact:
        Free Software Foundation
        675 Mass Ave
        Cambridge, MA  02139
        USA
  
}
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  //
  TAccount = class;
  TAccountArray = Array of TAccount;
  TAccountAccess = class;
  TAccountAccessArray = Array of TAccountAccess;
  TAccountAccesspermission = class;
  TAccountAccesspermissionArray = Array of TAccountAccesspermission;
  TCondition = class;
  TConditionArray = Array of TCondition;
  TConditionparameter = class;
  TConditionparameterArray = Array of TConditionparameter;
  TContainer = class;
  TContainerArray = Array of TContainer;
  TContainerdomainName = class;
  TContainerdomainNameArray = Array of TContainerdomainName;
  TContainerenabledBuiltInVariable = class;
  TContainerenabledBuiltInVariableArray = Array of TContainerenabledBuiltInVariable;
  TContainerusageContext = class;
  TContainerusageContextArray = Array of TContainerusageContext;
  TContainerAccess = class;
  TContainerAccessArray = Array of TContainerAccess;
  TContainerAccesspermission = class;
  TContainerAccesspermissionArray = Array of TContainerAccesspermission;
  TContainerVersion = class;
  TContainerVersionArray = Array of TContainerVersion;
  TContainerVersionmacro = class;
  TContainerVersionmacroArray = Array of TContainerVersionmacro;
  TContainerVersionrule = class;
  TContainerVersionruleArray = Array of TContainerVersionrule;
  TContainerVersiontag = class;
  TContainerVersiontagArray = Array of TContainerVersiontag;
  TContainerVersiontrigger = class;
  TContainerVersiontriggerArray = Array of TContainerVersiontrigger;
  TContainerVersionvariable = class;
  TContainerVersionvariableArray = Array of TContainerVersionvariable;
  TContainerVersionHeader = class;
  TContainerVersionHeaderArray = Array of TContainerVersionHeader;
  TCreateContainerVersionRequestVersionOptions = class;
  TCreateContainerVersionRequestVersionOptionsArray = Array of TCreateContainerVersionRequestVersionOptions;
  TCreateContainerVersionResponse = class;
  TCreateContainerVersionResponseArray = Array of TCreateContainerVersionResponse;
  TListAccountUsersResponse = class;
  TListAccountUsersResponseArray = Array of TListAccountUsersResponse;
  TListAccountUsersResponseuserAccess = class;
  TListAccountUsersResponseuserAccessArray = Array of TListAccountUsersResponseuserAccess;
  TListAccountsResponse = class;
  TListAccountsResponseArray = Array of TListAccountsResponse;
  TListAccountsResponseaccounts = class;
  TListAccountsResponseaccountsArray = Array of TListAccountsResponseaccounts;
  TListContainerVersionsResponse = class;
  TListContainerVersionsResponseArray = Array of TListContainerVersionsResponse;
  TListContainerVersionsResponsecontainerVersion = class;
  TListContainerVersionsResponsecontainerVersionArray = Array of TListContainerVersionsResponsecontainerVersion;
  TListContainerVersionsResponsecontainerVersionHeader = class;
  TListContainerVersionsResponsecontainerVersionHeaderArray = Array of TListContainerVersionsResponsecontainerVersionHeader;
  TListContainersResponse = class;
  TListContainersResponseArray = Array of TListContainersResponse;
  TListContainersResponsecontainers = class;
  TListContainersResponsecontainersArray = Array of TListContainersResponsecontainers;
  TListMacrosResponse = class;
  TListMacrosResponseArray = Array of TListMacrosResponse;
  TListMacrosResponsemacros = class;
  TListMacrosResponsemacrosArray = Array of TListMacrosResponsemacros;
  TListRulesResponse = class;
  TListRulesResponseArray = Array of TListRulesResponse;
  TListRulesResponserules = class;
  TListRulesResponserulesArray = Array of TListRulesResponserules;
  TListTagsResponse = class;
  TListTagsResponseArray = Array of TListTagsResponse;
  TListTagsResponsetags = class;
  TListTagsResponsetagsArray = Array of TListTagsResponsetags;
  TListTriggersResponse = class;
  TListTriggersResponseArray = Array of TListTriggersResponse;
  TListTriggersResponsetriggers = class;
  TListTriggersResponsetriggersArray = Array of TListTriggersResponsetriggers;
  TListVariablesResponse = class;
  TListVariablesResponseArray = Array of TListVariablesResponse;
  TListVariablesResponsevariables = class;
  TListVariablesResponsevariablesArray = Array of TListVariablesResponsevariables;
  TMacro = class;
  TMacroArray = Array of TMacro;
  TMacrodisablingRuleId = class;
  TMacrodisablingRuleIdArray = Array of TMacrodisablingRuleId;
  TMacroenablingRuleId = class;
  TMacroenablingRuleIdArray = Array of TMacroenablingRuleId;
  TMacroparameter = class;
  TMacroparameterArray = Array of TMacroparameter;
  TParameter = class;
  TParameterArray = Array of TParameter;
  TParameterlist = class;
  TParameterlistArray = Array of TParameterlist;
  TParametermap = class;
  TParametermapArray = Array of TParametermap;
  TPublishContainerVersionResponse = class;
  TPublishContainerVersionResponseArray = Array of TPublishContainerVersionResponse;
  TRule = class;
  TRuleArray = Array of TRule;
  TRulecondition = class;
  TRuleconditionArray = Array of TRulecondition;
  TTag = class;
  TTagArray = Array of TTag;
  TTagblockingRuleId = class;
  TTagblockingRuleIdArray = Array of TTagblockingRuleId;
  TTagblockingTriggerId = class;
  TTagblockingTriggerIdArray = Array of TTagblockingTriggerId;
  TTagfiringRuleId = class;
  TTagfiringRuleIdArray = Array of TTagfiringRuleId;
  TTagfiringTriggerId = class;
  TTagfiringTriggerIdArray = Array of TTagfiringTriggerId;
  TTagparameter = class;
  TTagparameterArray = Array of TTagparameter;
  TTrigger = class;
  TTriggerArray = Array of TTrigger;
  TTriggerautoEventFilter = class;
  TTriggerautoEventFilterArray = Array of TTriggerautoEventFilter;
  TTriggercustomEventFilter = class;
  TTriggercustomEventFilterArray = Array of TTriggercustomEventFilter;
  TTriggerfilter = class;
  TTriggerfilterArray = Array of TTriggerfilter;
  TUserAccess = class;
  TUserAccessArray = Array of TUserAccess;
  TUserAccesscontainerAccess = class;
  TUserAccesscontainerAccessArray = Array of TUserAccesscontainerAccess;
  TVariable = class;
  TVariableArray = Array of TVariable;
  TVariabledisablingTriggerId = class;
  TVariabledisablingTriggerIdArray = Array of TVariabledisablingTriggerId;
  TVariableenablingTriggerId = class;
  TVariableenablingTriggerIdArray = Array of TVariableenablingTriggerId;
  TVariableparameter = class;
  TVariableparameterArray = Array of TVariableparameter;
  
  { --------------------------------------------------------------------
    TAccount
    --------------------------------------------------------------------}
  
  TAccount = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    Ffingerprint : string;
    Fname : string;
    FshareData : boolean;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetshareData(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property fingerprint : string Index 8 Read Ffingerprint Write Setfingerprint;
    Property name : string Index 16 Read Fname Write Setname;
    Property shareData : boolean Index 24 Read FshareData Write SetshareData;
  end;
  TAccountClass = Class of TAccount;
  
  { --------------------------------------------------------------------
    TAccountAccess
    --------------------------------------------------------------------}
  
  TAccountAccess = Class(TGoogleBaseObject)
  Private
    Fpermission : TAccountAccesspermission;
  Protected
    //Property setters
    Procedure Setpermission(AIndex : Integer; AValue : TAccountAccesspermission); virtual;
  Public
  Published
    Property permission : TAccountAccesspermission Index 0 Read Fpermission Write Setpermission;
  end;
  TAccountAccessClass = Class of TAccountAccess;
  
  { --------------------------------------------------------------------
    TAccountAccesspermission
    --------------------------------------------------------------------}
  
  TAccountAccesspermission = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountAccesspermissionClass = Class of TAccountAccesspermission;
  
  { --------------------------------------------------------------------
    TCondition
    --------------------------------------------------------------------}
  
  TCondition = Class(TGoogleBaseObject)
  Private
    Fparameter : TConditionparameter;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setparameter(AIndex : Integer; AValue : TConditionparameter); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property parameter : TConditionparameter Index 0 Read Fparameter Write Setparameter;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TConditionClass = Class of TCondition;
  
  { --------------------------------------------------------------------
    TConditionparameter
    --------------------------------------------------------------------}
  
  TConditionparameter = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TConditionparameterClass = Class of TConditionparameter;
  
  { --------------------------------------------------------------------
    TContainer
    --------------------------------------------------------------------}
  
  TContainer = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    FcontainerId : string;
    FdomainName : TContainerdomainName;
    FenabledBuiltInVariable : TContainerenabledBuiltInVariable;
    Ffingerprint : string;
    Fname : string;
    Fnotes : string;
    FpublicId : string;
    FtimeZoneCountryId : string;
    FtimeZoneId : string;
    FusageContext : TContainerusageContext;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure SetcontainerId(AIndex : Integer; AValue : string); virtual;
    Procedure SetdomainName(AIndex : Integer; AValue : TContainerdomainName); virtual;
    Procedure SetenabledBuiltInVariable(AIndex : Integer; AValue : TContainerenabledBuiltInVariable); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setnotes(AIndex : Integer; AValue : string); virtual;
    Procedure SetpublicId(AIndex : Integer; AValue : string); virtual;
    Procedure SettimeZoneCountryId(AIndex : Integer; AValue : string); virtual;
    Procedure SettimeZoneId(AIndex : Integer; AValue : string); virtual;
    Procedure SetusageContext(AIndex : Integer; AValue : TContainerusageContext); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property containerId : string Index 8 Read FcontainerId Write SetcontainerId;
    Property domainName : TContainerdomainName Index 16 Read FdomainName Write SetdomainName;
    Property enabledBuiltInVariable : TContainerenabledBuiltInVariable Index 24 Read FenabledBuiltInVariable Write SetenabledBuiltInVariable;
    Property fingerprint : string Index 32 Read Ffingerprint Write Setfingerprint;
    Property name : string Index 40 Read Fname Write Setname;
    Property notes : string Index 48 Read Fnotes Write Setnotes;
    Property publicId : string Index 56 Read FpublicId Write SetpublicId;
    Property timeZoneCountryId : string Index 64 Read FtimeZoneCountryId Write SettimeZoneCountryId;
    Property timeZoneId : string Index 72 Read FtimeZoneId Write SettimeZoneId;
    Property usageContext : TContainerusageContext Index 80 Read FusageContext Write SetusageContext;
  end;
  TContainerClass = Class of TContainer;
  
  { --------------------------------------------------------------------
    TContainerdomainName
    --------------------------------------------------------------------}
  
  TContainerdomainName = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TContainerdomainNameClass = Class of TContainerdomainName;
  
  { --------------------------------------------------------------------
    TContainerenabledBuiltInVariable
    --------------------------------------------------------------------}
  
  TContainerenabledBuiltInVariable = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TContainerenabledBuiltInVariableClass = Class of TContainerenabledBuiltInVariable;
  
  { --------------------------------------------------------------------
    TContainerusageContext
    --------------------------------------------------------------------}
  
  TContainerusageContext = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TContainerusageContextClass = Class of TContainerusageContext;
  
  { --------------------------------------------------------------------
    TContainerAccess
    --------------------------------------------------------------------}
  
  TContainerAccess = Class(TGoogleBaseObject)
  Private
    FcontainerId : string;
    Fpermission : TContainerAccesspermission;
  Protected
    //Property setters
    Procedure SetcontainerId(AIndex : Integer; AValue : string); virtual;
    Procedure Setpermission(AIndex : Integer; AValue : TContainerAccesspermission); virtual;
  Public
  Published
    Property containerId : string Index 0 Read FcontainerId Write SetcontainerId;
    Property permission : TContainerAccesspermission Index 8 Read Fpermission Write Setpermission;
  end;
  TContainerAccessClass = Class of TContainerAccess;
  
  { --------------------------------------------------------------------
    TContainerAccesspermission
    --------------------------------------------------------------------}
  
  TContainerAccesspermission = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TContainerAccesspermissionClass = Class of TContainerAccesspermission;
  
  { --------------------------------------------------------------------
    TContainerVersion
    --------------------------------------------------------------------}
  
  TContainerVersion = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    Fcontainer : TContainer;
    FcontainerId : string;
    FcontainerVersionId : string;
    Fdeleted : boolean;
    Ffingerprint : string;
    Fmacro : TContainerVersionmacro;
    Fname : string;
    Fnotes : string;
    Frule : TContainerVersionrule;
    Ftag : TContainerVersiontag;
    Ftrigger : TContainerVersiontrigger;
    Fvariable : TContainerVersionvariable;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure Setcontainer(AIndex : Integer; AValue : TContainer); virtual;
    Procedure SetcontainerId(AIndex : Integer; AValue : string); virtual;
    Procedure SetcontainerVersionId(AIndex : Integer; AValue : string); virtual;
    Procedure Setdeleted(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : string); virtual;
    Procedure Setmacro(AIndex : Integer; AValue : TContainerVersionmacro); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setnotes(AIndex : Integer; AValue : string); virtual;
    Procedure Setrule(AIndex : Integer; AValue : TContainerVersionrule); virtual;
    Procedure Settag(AIndex : Integer; AValue : TContainerVersiontag); virtual;
    Procedure Settrigger(AIndex : Integer; AValue : TContainerVersiontrigger); virtual;
    Procedure Setvariable(AIndex : Integer; AValue : TContainerVersionvariable); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property container : TContainer Index 8 Read Fcontainer Write Setcontainer;
    Property containerId : string Index 16 Read FcontainerId Write SetcontainerId;
    Property containerVersionId : string Index 24 Read FcontainerVersionId Write SetcontainerVersionId;
    Property deleted : boolean Index 32 Read Fdeleted Write Setdeleted;
    Property fingerprint : string Index 40 Read Ffingerprint Write Setfingerprint;
    Property macro : TContainerVersionmacro Index 48 Read Fmacro Write Setmacro;
    Property name : string Index 56 Read Fname Write Setname;
    Property notes : string Index 64 Read Fnotes Write Setnotes;
    Property rule : TContainerVersionrule Index 72 Read Frule Write Setrule;
    Property tag : TContainerVersiontag Index 80 Read Ftag Write Settag;
    Property trigger : TContainerVersiontrigger Index 88 Read Ftrigger Write Settrigger;
    Property variable : TContainerVersionvariable Index 96 Read Fvariable Write Setvariable;
  end;
  TContainerVersionClass = Class of TContainerVersion;
  
  { --------------------------------------------------------------------
    TContainerVersionmacro
    --------------------------------------------------------------------}
  
  TContainerVersionmacro = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TContainerVersionmacroClass = Class of TContainerVersionmacro;
  
  { --------------------------------------------------------------------
    TContainerVersionrule
    --------------------------------------------------------------------}
  
  TContainerVersionrule = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TContainerVersionruleClass = Class of TContainerVersionrule;
  
  { --------------------------------------------------------------------
    TContainerVersiontag
    --------------------------------------------------------------------}
  
  TContainerVersiontag = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TContainerVersiontagClass = Class of TContainerVersiontag;
  
  { --------------------------------------------------------------------
    TContainerVersiontrigger
    --------------------------------------------------------------------}
  
  TContainerVersiontrigger = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TContainerVersiontriggerClass = Class of TContainerVersiontrigger;
  
  { --------------------------------------------------------------------
    TContainerVersionvariable
    --------------------------------------------------------------------}
  
  TContainerVersionvariable = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TContainerVersionvariableClass = Class of TContainerVersionvariable;
  
  { --------------------------------------------------------------------
    TContainerVersionHeader
    --------------------------------------------------------------------}
  
  TContainerVersionHeader = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    FcontainerId : string;
    FcontainerVersionId : string;
    Fdeleted : boolean;
    Fname : string;
    FnumMacros : string;
    FnumRules : string;
    FnumTags : string;
    FnumTriggers : string;
    FnumVariables : string;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure SetcontainerId(AIndex : Integer; AValue : string); virtual;
    Procedure SetcontainerVersionId(AIndex : Integer; AValue : string); virtual;
    Procedure Setdeleted(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetnumMacros(AIndex : Integer; AValue : string); virtual;
    Procedure SetnumRules(AIndex : Integer; AValue : string); virtual;
    Procedure SetnumTags(AIndex : Integer; AValue : string); virtual;
    Procedure SetnumTriggers(AIndex : Integer; AValue : string); virtual;
    Procedure SetnumVariables(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property containerId : string Index 8 Read FcontainerId Write SetcontainerId;
    Property containerVersionId : string Index 16 Read FcontainerVersionId Write SetcontainerVersionId;
    Property deleted : boolean Index 24 Read Fdeleted Write Setdeleted;
    Property name : string Index 32 Read Fname Write Setname;
    Property numMacros : string Index 40 Read FnumMacros Write SetnumMacros;
    Property numRules : string Index 48 Read FnumRules Write SetnumRules;
    Property numTags : string Index 56 Read FnumTags Write SetnumTags;
    Property numTriggers : string Index 64 Read FnumTriggers Write SetnumTriggers;
    Property numVariables : string Index 72 Read FnumVariables Write SetnumVariables;
  end;
  TContainerVersionHeaderClass = Class of TContainerVersionHeader;
  
  { --------------------------------------------------------------------
    TCreateContainerVersionRequestVersionOptions
    --------------------------------------------------------------------}
  
  TCreateContainerVersionRequestVersionOptions = Class(TGoogleBaseObject)
  Private
    Fname : string;
    Fnotes : string;
    FquickPreview : boolean;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setnotes(AIndex : Integer; AValue : string); virtual;
    Procedure SetquickPreview(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property name : string Index 0 Read Fname Write Setname;
    Property notes : string Index 8 Read Fnotes Write Setnotes;
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
    FuserAccess : TListAccountUsersResponseuserAccess;
  Protected
    //Property setters
    Procedure SetuserAccess(AIndex : Integer; AValue : TListAccountUsersResponseuserAccess); virtual;
  Public
  Published
    Property userAccess : TListAccountUsersResponseuserAccess Index 0 Read FuserAccess Write SetuserAccess;
  end;
  TListAccountUsersResponseClass = Class of TListAccountUsersResponse;
  
  { --------------------------------------------------------------------
    TListAccountUsersResponseuserAccess
    --------------------------------------------------------------------}
  
  TListAccountUsersResponseuserAccess = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListAccountUsersResponseuserAccessClass = Class of TListAccountUsersResponseuserAccess;
  
  { --------------------------------------------------------------------
    TListAccountsResponse
    --------------------------------------------------------------------}
  
  TListAccountsResponse = Class(TGoogleBaseObject)
  Private
    Faccounts : TListAccountsResponseaccounts;
  Protected
    //Property setters
    Procedure Setaccounts(AIndex : Integer; AValue : TListAccountsResponseaccounts); virtual;
  Public
  Published
    Property accounts : TListAccountsResponseaccounts Index 0 Read Faccounts Write Setaccounts;
  end;
  TListAccountsResponseClass = Class of TListAccountsResponse;
  
  { --------------------------------------------------------------------
    TListAccountsResponseaccounts
    --------------------------------------------------------------------}
  
  TListAccountsResponseaccounts = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListAccountsResponseaccountsClass = Class of TListAccountsResponseaccounts;
  
  { --------------------------------------------------------------------
    TListContainerVersionsResponse
    --------------------------------------------------------------------}
  
  TListContainerVersionsResponse = Class(TGoogleBaseObject)
  Private
    FcontainerVersion : TListContainerVersionsResponsecontainerVersion;
    FcontainerVersionHeader : TListContainerVersionsResponsecontainerVersionHeader;
  Protected
    //Property setters
    Procedure SetcontainerVersion(AIndex : Integer; AValue : TListContainerVersionsResponsecontainerVersion); virtual;
    Procedure SetcontainerVersionHeader(AIndex : Integer; AValue : TListContainerVersionsResponsecontainerVersionHeader); virtual;
  Public
  Published
    Property containerVersion : TListContainerVersionsResponsecontainerVersion Index 0 Read FcontainerVersion Write SetcontainerVersion;
    Property containerVersionHeader : TListContainerVersionsResponsecontainerVersionHeader Index 8 Read FcontainerVersionHeader Write SetcontainerVersionHeader;
  end;
  TListContainerVersionsResponseClass = Class of TListContainerVersionsResponse;
  
  { --------------------------------------------------------------------
    TListContainerVersionsResponsecontainerVersion
    --------------------------------------------------------------------}
  
  TListContainerVersionsResponsecontainerVersion = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListContainerVersionsResponsecontainerVersionClass = Class of TListContainerVersionsResponsecontainerVersion;
  
  { --------------------------------------------------------------------
    TListContainerVersionsResponsecontainerVersionHeader
    --------------------------------------------------------------------}
  
  TListContainerVersionsResponsecontainerVersionHeader = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListContainerVersionsResponsecontainerVersionHeaderClass = Class of TListContainerVersionsResponsecontainerVersionHeader;
  
  { --------------------------------------------------------------------
    TListContainersResponse
    --------------------------------------------------------------------}
  
  TListContainersResponse = Class(TGoogleBaseObject)
  Private
    Fcontainers : TListContainersResponsecontainers;
  Protected
    //Property setters
    Procedure Setcontainers(AIndex : Integer; AValue : TListContainersResponsecontainers); virtual;
  Public
  Published
    Property containers : TListContainersResponsecontainers Index 0 Read Fcontainers Write Setcontainers;
  end;
  TListContainersResponseClass = Class of TListContainersResponse;
  
  { --------------------------------------------------------------------
    TListContainersResponsecontainers
    --------------------------------------------------------------------}
  
  TListContainersResponsecontainers = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListContainersResponsecontainersClass = Class of TListContainersResponsecontainers;
  
  { --------------------------------------------------------------------
    TListMacrosResponse
    --------------------------------------------------------------------}
  
  TListMacrosResponse = Class(TGoogleBaseObject)
  Private
    Fmacros : TListMacrosResponsemacros;
  Protected
    //Property setters
    Procedure Setmacros(AIndex : Integer; AValue : TListMacrosResponsemacros); virtual;
  Public
  Published
    Property macros : TListMacrosResponsemacros Index 0 Read Fmacros Write Setmacros;
  end;
  TListMacrosResponseClass = Class of TListMacrosResponse;
  
  { --------------------------------------------------------------------
    TListMacrosResponsemacros
    --------------------------------------------------------------------}
  
  TListMacrosResponsemacros = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListMacrosResponsemacrosClass = Class of TListMacrosResponsemacros;
  
  { --------------------------------------------------------------------
    TListRulesResponse
    --------------------------------------------------------------------}
  
  TListRulesResponse = Class(TGoogleBaseObject)
  Private
    Frules : TListRulesResponserules;
  Protected
    //Property setters
    Procedure Setrules(AIndex : Integer; AValue : TListRulesResponserules); virtual;
  Public
  Published
    Property rules : TListRulesResponserules Index 0 Read Frules Write Setrules;
  end;
  TListRulesResponseClass = Class of TListRulesResponse;
  
  { --------------------------------------------------------------------
    TListRulesResponserules
    --------------------------------------------------------------------}
  
  TListRulesResponserules = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListRulesResponserulesClass = Class of TListRulesResponserules;
  
  { --------------------------------------------------------------------
    TListTagsResponse
    --------------------------------------------------------------------}
  
  TListTagsResponse = Class(TGoogleBaseObject)
  Private
    Ftags : TListTagsResponsetags;
  Protected
    //Property setters
    Procedure Settags(AIndex : Integer; AValue : TListTagsResponsetags); virtual;
  Public
  Published
    Property tags : TListTagsResponsetags Index 0 Read Ftags Write Settags;
  end;
  TListTagsResponseClass = Class of TListTagsResponse;
  
  { --------------------------------------------------------------------
    TListTagsResponsetags
    --------------------------------------------------------------------}
  
  TListTagsResponsetags = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListTagsResponsetagsClass = Class of TListTagsResponsetags;
  
  { --------------------------------------------------------------------
    TListTriggersResponse
    --------------------------------------------------------------------}
  
  TListTriggersResponse = Class(TGoogleBaseObject)
  Private
    Ftriggers : TListTriggersResponsetriggers;
  Protected
    //Property setters
    Procedure Settriggers(AIndex : Integer; AValue : TListTriggersResponsetriggers); virtual;
  Public
  Published
    Property triggers : TListTriggersResponsetriggers Index 0 Read Ftriggers Write Settriggers;
  end;
  TListTriggersResponseClass = Class of TListTriggersResponse;
  
  { --------------------------------------------------------------------
    TListTriggersResponsetriggers
    --------------------------------------------------------------------}
  
  TListTriggersResponsetriggers = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListTriggersResponsetriggersClass = Class of TListTriggersResponsetriggers;
  
  { --------------------------------------------------------------------
    TListVariablesResponse
    --------------------------------------------------------------------}
  
  TListVariablesResponse = Class(TGoogleBaseObject)
  Private
    Fvariables : TListVariablesResponsevariables;
  Protected
    //Property setters
    Procedure Setvariables(AIndex : Integer; AValue : TListVariablesResponsevariables); virtual;
  Public
  Published
    Property variables : TListVariablesResponsevariables Index 0 Read Fvariables Write Setvariables;
  end;
  TListVariablesResponseClass = Class of TListVariablesResponse;
  
  { --------------------------------------------------------------------
    TListVariablesResponsevariables
    --------------------------------------------------------------------}
  
  TListVariablesResponsevariables = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListVariablesResponsevariablesClass = Class of TListVariablesResponsevariables;
  
  { --------------------------------------------------------------------
    TMacro
    --------------------------------------------------------------------}
  
  TMacro = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    FcontainerId : string;
    FdisablingRuleId : TMacrodisablingRuleId;
    FenablingRuleId : TMacroenablingRuleId;
    Ffingerprint : string;
    FmacroId : string;
    Fname : string;
    Fnotes : string;
    Fparameter : TMacroparameter;
    FscheduleEndMs : string;
    FscheduleStartMs : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure SetcontainerId(AIndex : Integer; AValue : string); virtual;
    Procedure SetdisablingRuleId(AIndex : Integer; AValue : TMacrodisablingRuleId); virtual;
    Procedure SetenablingRuleId(AIndex : Integer; AValue : TMacroenablingRuleId); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : string); virtual;
    Procedure SetmacroId(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setnotes(AIndex : Integer; AValue : string); virtual;
    Procedure Setparameter(AIndex : Integer; AValue : TMacroparameter); virtual;
    Procedure SetscheduleEndMs(AIndex : Integer; AValue : string); virtual;
    Procedure SetscheduleStartMs(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property containerId : string Index 8 Read FcontainerId Write SetcontainerId;
    Property disablingRuleId : TMacrodisablingRuleId Index 16 Read FdisablingRuleId Write SetdisablingRuleId;
    Property enablingRuleId : TMacroenablingRuleId Index 24 Read FenablingRuleId Write SetenablingRuleId;
    Property fingerprint : string Index 32 Read Ffingerprint Write Setfingerprint;
    Property macroId : string Index 40 Read FmacroId Write SetmacroId;
    Property name : string Index 48 Read Fname Write Setname;
    Property notes : string Index 56 Read Fnotes Write Setnotes;
    Property parameter : TMacroparameter Index 64 Read Fparameter Write Setparameter;
    Property scheduleEndMs : string Index 72 Read FscheduleEndMs Write SetscheduleEndMs;
    Property scheduleStartMs : string Index 80 Read FscheduleStartMs Write SetscheduleStartMs;
    Property _type : string Index 88 Read F_type Write Set_type;
  end;
  TMacroClass = Class of TMacro;
  
  { --------------------------------------------------------------------
    TMacrodisablingRuleId
    --------------------------------------------------------------------}
  
  TMacrodisablingRuleId = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMacrodisablingRuleIdClass = Class of TMacrodisablingRuleId;
  
  { --------------------------------------------------------------------
    TMacroenablingRuleId
    --------------------------------------------------------------------}
  
  TMacroenablingRuleId = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMacroenablingRuleIdClass = Class of TMacroenablingRuleId;
  
  { --------------------------------------------------------------------
    TMacroparameter
    --------------------------------------------------------------------}
  
  TMacroparameter = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMacroparameterClass = Class of TMacroparameter;
  
  { --------------------------------------------------------------------
    TParameter
    --------------------------------------------------------------------}
  
  TParameter = Class(TGoogleBaseObject)
  Private
    Fkey : string;
    Flist : TParameterlist;
    Fmap : TParametermap;
    F_type : string;
    Fvalue : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure Setlist(AIndex : Integer; AValue : TParameterlist); virtual;
    Procedure Setmap(AIndex : Integer; AValue : TParametermap); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property key : string Index 0 Read Fkey Write Setkey;
    Property list : TParameterlist Index 8 Read Flist Write Setlist;
    Property map : TParametermap Index 16 Read Fmap Write Setmap;
    Property _type : string Index 24 Read F_type Write Set_type;
    Property value : string Index 32 Read Fvalue Write Setvalue;
  end;
  TParameterClass = Class of TParameter;
  
  { --------------------------------------------------------------------
    TParameterlist
    --------------------------------------------------------------------}
  
  TParameterlist = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TParameterlistClass = Class of TParameterlist;
  
  { --------------------------------------------------------------------
    TParametermap
    --------------------------------------------------------------------}
  
  TParametermap = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TParametermapClass = Class of TParametermap;
  
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
    FaccountId : string;
    Fcondition : TRulecondition;
    FcontainerId : string;
    Ffingerprint : string;
    Fname : string;
    Fnotes : string;
    FruleId : string;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure Setcondition(AIndex : Integer; AValue : TRulecondition); virtual;
    Procedure SetcontainerId(AIndex : Integer; AValue : string); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setnotes(AIndex : Integer; AValue : string); virtual;
    Procedure SetruleId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property condition : TRulecondition Index 8 Read Fcondition Write Setcondition;
    Property containerId : string Index 16 Read FcontainerId Write SetcontainerId;
    Property fingerprint : string Index 24 Read Ffingerprint Write Setfingerprint;
    Property name : string Index 32 Read Fname Write Setname;
    Property notes : string Index 40 Read Fnotes Write Setnotes;
    Property ruleId : string Index 48 Read FruleId Write SetruleId;
  end;
  TRuleClass = Class of TRule;
  
  { --------------------------------------------------------------------
    TRulecondition
    --------------------------------------------------------------------}
  
  TRulecondition = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRuleconditionClass = Class of TRulecondition;
  
  { --------------------------------------------------------------------
    TTag
    --------------------------------------------------------------------}
  
  TTag = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    FblockingRuleId : TTagblockingRuleId;
    FblockingTriggerId : TTagblockingTriggerId;
    FcontainerId : string;
    Ffingerprint : string;
    FfiringRuleId : TTagfiringRuleId;
    FfiringTriggerId : TTagfiringTriggerId;
    FliveOnly : boolean;
    Fname : string;
    Fnotes : string;
    Fparameter : TTagparameter;
    Fpriority : TParameter;
    FscheduleEndMs : string;
    FscheduleStartMs : string;
    FtagId : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure SetblockingRuleId(AIndex : Integer; AValue : TTagblockingRuleId); virtual;
    Procedure SetblockingTriggerId(AIndex : Integer; AValue : TTagblockingTriggerId); virtual;
    Procedure SetcontainerId(AIndex : Integer; AValue : string); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : string); virtual;
    Procedure SetfiringRuleId(AIndex : Integer; AValue : TTagfiringRuleId); virtual;
    Procedure SetfiringTriggerId(AIndex : Integer; AValue : TTagfiringTriggerId); virtual;
    Procedure SetliveOnly(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setnotes(AIndex : Integer; AValue : string); virtual;
    Procedure Setparameter(AIndex : Integer; AValue : TTagparameter); virtual;
    Procedure Setpriority(AIndex : Integer; AValue : TParameter); virtual;
    Procedure SetscheduleEndMs(AIndex : Integer; AValue : string); virtual;
    Procedure SetscheduleStartMs(AIndex : Integer; AValue : string); virtual;
    Procedure SettagId(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property blockingRuleId : TTagblockingRuleId Index 8 Read FblockingRuleId Write SetblockingRuleId;
    Property blockingTriggerId : TTagblockingTriggerId Index 16 Read FblockingTriggerId Write SetblockingTriggerId;
    Property containerId : string Index 24 Read FcontainerId Write SetcontainerId;
    Property fingerprint : string Index 32 Read Ffingerprint Write Setfingerprint;
    Property firingRuleId : TTagfiringRuleId Index 40 Read FfiringRuleId Write SetfiringRuleId;
    Property firingTriggerId : TTagfiringTriggerId Index 48 Read FfiringTriggerId Write SetfiringTriggerId;
    Property liveOnly : boolean Index 56 Read FliveOnly Write SetliveOnly;
    Property name : string Index 64 Read Fname Write Setname;
    Property notes : string Index 72 Read Fnotes Write Setnotes;
    Property parameter : TTagparameter Index 80 Read Fparameter Write Setparameter;
    Property priority : TParameter Index 88 Read Fpriority Write Setpriority;
    Property scheduleEndMs : string Index 96 Read FscheduleEndMs Write SetscheduleEndMs;
    Property scheduleStartMs : string Index 104 Read FscheduleStartMs Write SetscheduleStartMs;
    Property tagId : string Index 112 Read FtagId Write SettagId;
    Property _type : string Index 120 Read F_type Write Set_type;
  end;
  TTagClass = Class of TTag;
  
  { --------------------------------------------------------------------
    TTagblockingRuleId
    --------------------------------------------------------------------}
  
  TTagblockingRuleId = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTagblockingRuleIdClass = Class of TTagblockingRuleId;
  
  { --------------------------------------------------------------------
    TTagblockingTriggerId
    --------------------------------------------------------------------}
  
  TTagblockingTriggerId = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTagblockingTriggerIdClass = Class of TTagblockingTriggerId;
  
  { --------------------------------------------------------------------
    TTagfiringRuleId
    --------------------------------------------------------------------}
  
  TTagfiringRuleId = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTagfiringRuleIdClass = Class of TTagfiringRuleId;
  
  { --------------------------------------------------------------------
    TTagfiringTriggerId
    --------------------------------------------------------------------}
  
  TTagfiringTriggerId = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTagfiringTriggerIdClass = Class of TTagfiringTriggerId;
  
  { --------------------------------------------------------------------
    TTagparameter
    --------------------------------------------------------------------}
  
  TTagparameter = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTagparameterClass = Class of TTagparameter;
  
  { --------------------------------------------------------------------
    TTrigger
    --------------------------------------------------------------------}
  
  TTrigger = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    FautoEventFilter : TTriggerautoEventFilter;
    FcheckValidation : TParameter;
    FcontainerId : string;
    FcustomEventFilter : TTriggercustomEventFilter;
    FenableAllVideos : TParameter;
    FeventName : TParameter;
    Ffilter : TTriggerfilter;
    Ffingerprint : string;
    Finterval : TParameter;
    Flimit : TParameter;
    Fname : string;
    FtriggerId : string;
    F_type : string;
    FuniqueTriggerId : TParameter;
    FvideoPercentageList : TParameter;
    FwaitForTags : TParameter;
    FwaitForTagsTimeout : TParameter;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure SetautoEventFilter(AIndex : Integer; AValue : TTriggerautoEventFilter); virtual;
    Procedure SetcheckValidation(AIndex : Integer; AValue : TParameter); virtual;
    Procedure SetcontainerId(AIndex : Integer; AValue : string); virtual;
    Procedure SetcustomEventFilter(AIndex : Integer; AValue : TTriggercustomEventFilter); virtual;
    Procedure SetenableAllVideos(AIndex : Integer; AValue : TParameter); virtual;
    Procedure SeteventName(AIndex : Integer; AValue : TParameter); virtual;
    Procedure Setfilter(AIndex : Integer; AValue : TTriggerfilter); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : string); virtual;
    Procedure Setinterval(AIndex : Integer; AValue : TParameter); virtual;
    Procedure Setlimit(AIndex : Integer; AValue : TParameter); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SettriggerId(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure SetuniqueTriggerId(AIndex : Integer; AValue : TParameter); virtual;
    Procedure SetvideoPercentageList(AIndex : Integer; AValue : TParameter); virtual;
    Procedure SetwaitForTags(AIndex : Integer; AValue : TParameter); virtual;
    Procedure SetwaitForTagsTimeout(AIndex : Integer; AValue : TParameter); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property autoEventFilter : TTriggerautoEventFilter Index 8 Read FautoEventFilter Write SetautoEventFilter;
    Property checkValidation : TParameter Index 16 Read FcheckValidation Write SetcheckValidation;
    Property containerId : string Index 24 Read FcontainerId Write SetcontainerId;
    Property customEventFilter : TTriggercustomEventFilter Index 32 Read FcustomEventFilter Write SetcustomEventFilter;
    Property enableAllVideos : TParameter Index 40 Read FenableAllVideos Write SetenableAllVideos;
    Property eventName : TParameter Index 48 Read FeventName Write SeteventName;
    Property filter : TTriggerfilter Index 56 Read Ffilter Write Setfilter;
    Property fingerprint : string Index 64 Read Ffingerprint Write Setfingerprint;
    Property interval : TParameter Index 72 Read Finterval Write Setinterval;
    Property limit : TParameter Index 80 Read Flimit Write Setlimit;
    Property name : string Index 88 Read Fname Write Setname;
    Property triggerId : string Index 96 Read FtriggerId Write SettriggerId;
    Property _type : string Index 104 Read F_type Write Set_type;
    Property uniqueTriggerId : TParameter Index 112 Read FuniqueTriggerId Write SetuniqueTriggerId;
    Property videoPercentageList : TParameter Index 120 Read FvideoPercentageList Write SetvideoPercentageList;
    Property waitForTags : TParameter Index 128 Read FwaitForTags Write SetwaitForTags;
    Property waitForTagsTimeout : TParameter Index 136 Read FwaitForTagsTimeout Write SetwaitForTagsTimeout;
  end;
  TTriggerClass = Class of TTrigger;
  
  { --------------------------------------------------------------------
    TTriggerautoEventFilter
    --------------------------------------------------------------------}
  
  TTriggerautoEventFilter = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTriggerautoEventFilterClass = Class of TTriggerautoEventFilter;
  
  { --------------------------------------------------------------------
    TTriggercustomEventFilter
    --------------------------------------------------------------------}
  
  TTriggercustomEventFilter = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTriggercustomEventFilterClass = Class of TTriggercustomEventFilter;
  
  { --------------------------------------------------------------------
    TTriggerfilter
    --------------------------------------------------------------------}
  
  TTriggerfilter = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTriggerfilterClass = Class of TTriggerfilter;
  
  { --------------------------------------------------------------------
    TUserAccess
    --------------------------------------------------------------------}
  
  TUserAccess = Class(TGoogleBaseObject)
  Private
    FaccountAccess : TAccountAccess;
    FaccountId : string;
    FcontainerAccess : TUserAccesscontainerAccess;
    FemailAddress : string;
    FpermissionId : string;
  Protected
    //Property setters
    Procedure SetaccountAccess(AIndex : Integer; AValue : TAccountAccess); virtual;
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure SetcontainerAccess(AIndex : Integer; AValue : TUserAccesscontainerAccess); virtual;
    Procedure SetemailAddress(AIndex : Integer; AValue : string); virtual;
    Procedure SetpermissionId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountAccess : TAccountAccess Index 0 Read FaccountAccess Write SetaccountAccess;
    Property accountId : string Index 8 Read FaccountId Write SetaccountId;
    Property containerAccess : TUserAccesscontainerAccess Index 16 Read FcontainerAccess Write SetcontainerAccess;
    Property emailAddress : string Index 24 Read FemailAddress Write SetemailAddress;
    Property permissionId : string Index 32 Read FpermissionId Write SetpermissionId;
  end;
  TUserAccessClass = Class of TUserAccess;
  
  { --------------------------------------------------------------------
    TUserAccesscontainerAccess
    --------------------------------------------------------------------}
  
  TUserAccesscontainerAccess = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TUserAccesscontainerAccessClass = Class of TUserAccesscontainerAccess;
  
  { --------------------------------------------------------------------
    TVariable
    --------------------------------------------------------------------}
  
  TVariable = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    FcontainerId : string;
    FdisablingTriggerId : TVariabledisablingTriggerId;
    FenablingTriggerId : TVariableenablingTriggerId;
    Ffingerprint : string;
    Fname : string;
    Fnotes : string;
    Fparameter : TVariableparameter;
    FscheduleEndMs : string;
    FscheduleStartMs : string;
    F_type : string;
    FvariableId : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure SetcontainerId(AIndex : Integer; AValue : string); virtual;
    Procedure SetdisablingTriggerId(AIndex : Integer; AValue : TVariabledisablingTriggerId); virtual;
    Procedure SetenablingTriggerId(AIndex : Integer; AValue : TVariableenablingTriggerId); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setnotes(AIndex : Integer; AValue : string); virtual;
    Procedure Setparameter(AIndex : Integer; AValue : TVariableparameter); virtual;
    Procedure SetscheduleEndMs(AIndex : Integer; AValue : string); virtual;
    Procedure SetscheduleStartMs(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure SetvariableId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property containerId : string Index 8 Read FcontainerId Write SetcontainerId;
    Property disablingTriggerId : TVariabledisablingTriggerId Index 16 Read FdisablingTriggerId Write SetdisablingTriggerId;
    Property enablingTriggerId : TVariableenablingTriggerId Index 24 Read FenablingTriggerId Write SetenablingTriggerId;
    Property fingerprint : string Index 32 Read Ffingerprint Write Setfingerprint;
    Property name : string Index 40 Read Fname Write Setname;
    Property notes : string Index 48 Read Fnotes Write Setnotes;
    Property parameter : TVariableparameter Index 56 Read Fparameter Write Setparameter;
    Property scheduleEndMs : string Index 64 Read FscheduleEndMs Write SetscheduleEndMs;
    Property scheduleStartMs : string Index 72 Read FscheduleStartMs Write SetscheduleStartMs;
    Property _type : string Index 80 Read F_type Write Set_type;
    Property variableId : string Index 88 Read FvariableId Write SetvariableId;
  end;
  TVariableClass = Class of TVariable;
  
  { --------------------------------------------------------------------
    TVariabledisablingTriggerId
    --------------------------------------------------------------------}
  
  TVariabledisablingTriggerId = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVariabledisablingTriggerIdClass = Class of TVariabledisablingTriggerId;
  
  { --------------------------------------------------------------------
    TVariableenablingTriggerId
    --------------------------------------------------------------------}
  
  TVariableenablingTriggerId = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVariableenablingTriggerIdClass = Class of TVariableenablingTriggerId;
  
  { --------------------------------------------------------------------
    TVariableparameter
    --------------------------------------------------------------------}
  
  TVariableparameter = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVariableparameterClass = Class of TVariableparameter;
  
  { --------------------------------------------------------------------
    TAccountsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsResource, method Update
  
  TAccountsUpdateOptions = Record
    fingerprint : string;
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


Procedure TAccount.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setfingerprint(AIndex : Integer; AValue : string); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setname(AIndex : Integer; AValue : string); 

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


Procedure TAccountAccess.Setpermission(AIndex : Integer; AValue : TAccountAccesspermission); 

begin
  If (Fpermission=AValue) then exit;
  Fpermission:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountAccesspermission
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCondition
  --------------------------------------------------------------------}


Procedure TCondition.Setparameter(AIndex : Integer; AValue : TConditionparameter); 

begin
  If (Fparameter=AValue) then exit;
  Fparameter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCondition.Set_type(AIndex : Integer; AValue : string); 

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
  TConditionparameter
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TContainer
  --------------------------------------------------------------------}


Procedure TContainer.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.SetcontainerId(AIndex : Integer; AValue : string); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.SetdomainName(AIndex : Integer; AValue : TContainerdomainName); 

begin
  If (FdomainName=AValue) then exit;
  FdomainName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.SetenabledBuiltInVariable(AIndex : Integer; AValue : TContainerenabledBuiltInVariable); 

begin
  If (FenabledBuiltInVariable=AValue) then exit;
  FenabledBuiltInVariable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.Setfingerprint(AIndex : Integer; AValue : string); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.Setnotes(AIndex : Integer; AValue : string); 

begin
  If (Fnotes=AValue) then exit;
  Fnotes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.SetpublicId(AIndex : Integer; AValue : string); 

begin
  If (FpublicId=AValue) then exit;
  FpublicId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.SettimeZoneCountryId(AIndex : Integer; AValue : string); 

begin
  If (FtimeZoneCountryId=AValue) then exit;
  FtimeZoneCountryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.SettimeZoneId(AIndex : Integer; AValue : string); 

begin
  If (FtimeZoneId=AValue) then exit;
  FtimeZoneId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.SetusageContext(AIndex : Integer; AValue : TContainerusageContext); 

begin
  If (FusageContext=AValue) then exit;
  FusageContext:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TContainerdomainName
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TContainerenabledBuiltInVariable
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TContainerusageContext
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TContainerAccess
  --------------------------------------------------------------------}


Procedure TContainerAccess.SetcontainerId(AIndex : Integer; AValue : string); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerAccess.Setpermission(AIndex : Integer; AValue : TContainerAccesspermission); 

begin
  If (Fpermission=AValue) then exit;
  Fpermission:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TContainerAccesspermission
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TContainerVersion
  --------------------------------------------------------------------}


Procedure TContainerVersion.SetaccountId(AIndex : Integer; AValue : string); 

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



Procedure TContainerVersion.SetcontainerId(AIndex : Integer; AValue : string); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.SetcontainerVersionId(AIndex : Integer; AValue : string); 

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



Procedure TContainerVersion.Setfingerprint(AIndex : Integer; AValue : string); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.Setmacro(AIndex : Integer; AValue : TContainerVersionmacro); 

begin
  If (Fmacro=AValue) then exit;
  Fmacro:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.Setnotes(AIndex : Integer; AValue : string); 

begin
  If (Fnotes=AValue) then exit;
  Fnotes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.Setrule(AIndex : Integer; AValue : TContainerVersionrule); 

begin
  If (Frule=AValue) then exit;
  Frule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.Settag(AIndex : Integer; AValue : TContainerVersiontag); 

begin
  If (Ftag=AValue) then exit;
  Ftag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.Settrigger(AIndex : Integer; AValue : TContainerVersiontrigger); 

begin
  If (Ftrigger=AValue) then exit;
  Ftrigger:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.Setvariable(AIndex : Integer; AValue : TContainerVersionvariable); 

begin
  If (Fvariable=AValue) then exit;
  Fvariable:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TContainerVersionmacro
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TContainerVersionrule
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TContainerVersiontag
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TContainerVersiontrigger
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TContainerVersionvariable
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TContainerVersionHeader
  --------------------------------------------------------------------}


Procedure TContainerVersionHeader.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersionHeader.SetcontainerId(AIndex : Integer; AValue : string); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersionHeader.SetcontainerVersionId(AIndex : Integer; AValue : string); 

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



Procedure TContainerVersionHeader.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersionHeader.SetnumMacros(AIndex : Integer; AValue : string); 

begin
  If (FnumMacros=AValue) then exit;
  FnumMacros:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersionHeader.SetnumRules(AIndex : Integer; AValue : string); 

begin
  If (FnumRules=AValue) then exit;
  FnumRules:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersionHeader.SetnumTags(AIndex : Integer; AValue : string); 

begin
  If (FnumTags=AValue) then exit;
  FnumTags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersionHeader.SetnumTriggers(AIndex : Integer; AValue : string); 

begin
  If (FnumTriggers=AValue) then exit;
  FnumTriggers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersionHeader.SetnumVariables(AIndex : Integer; AValue : string); 

begin
  If (FnumVariables=AValue) then exit;
  FnumVariables:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreateContainerVersionRequestVersionOptions
  --------------------------------------------------------------------}


Procedure TCreateContainerVersionRequestVersionOptions.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreateContainerVersionRequestVersionOptions.Setnotes(AIndex : Integer; AValue : string); 

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


Procedure TListAccountUsersResponse.SetuserAccess(AIndex : Integer; AValue : TListAccountUsersResponseuserAccess); 

begin
  If (FuserAccess=AValue) then exit;
  FuserAccess:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListAccountUsersResponseuserAccess
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListAccountsResponse
  --------------------------------------------------------------------}


Procedure TListAccountsResponse.Setaccounts(AIndex : Integer; AValue : TListAccountsResponseaccounts); 

begin
  If (Faccounts=AValue) then exit;
  Faccounts:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListAccountsResponseaccounts
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListContainerVersionsResponse
  --------------------------------------------------------------------}


Procedure TListContainerVersionsResponse.SetcontainerVersion(AIndex : Integer; AValue : TListContainerVersionsResponsecontainerVersion); 

begin
  If (FcontainerVersion=AValue) then exit;
  FcontainerVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListContainerVersionsResponse.SetcontainerVersionHeader(AIndex : Integer; AValue : TListContainerVersionsResponsecontainerVersionHeader); 

begin
  If (FcontainerVersionHeader=AValue) then exit;
  FcontainerVersionHeader:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListContainerVersionsResponsecontainerVersion
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListContainerVersionsResponsecontainerVersionHeader
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListContainersResponse
  --------------------------------------------------------------------}


Procedure TListContainersResponse.Setcontainers(AIndex : Integer; AValue : TListContainersResponsecontainers); 

begin
  If (Fcontainers=AValue) then exit;
  Fcontainers:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListContainersResponsecontainers
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListMacrosResponse
  --------------------------------------------------------------------}


Procedure TListMacrosResponse.Setmacros(AIndex : Integer; AValue : TListMacrosResponsemacros); 

begin
  If (Fmacros=AValue) then exit;
  Fmacros:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListMacrosResponsemacros
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListRulesResponse
  --------------------------------------------------------------------}


Procedure TListRulesResponse.Setrules(AIndex : Integer; AValue : TListRulesResponserules); 

begin
  If (Frules=AValue) then exit;
  Frules:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListRulesResponserules
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListTagsResponse
  --------------------------------------------------------------------}


Procedure TListTagsResponse.Settags(AIndex : Integer; AValue : TListTagsResponsetags); 

begin
  If (Ftags=AValue) then exit;
  Ftags:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListTagsResponsetags
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListTriggersResponse
  --------------------------------------------------------------------}


Procedure TListTriggersResponse.Settriggers(AIndex : Integer; AValue : TListTriggersResponsetriggers); 

begin
  If (Ftriggers=AValue) then exit;
  Ftriggers:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListTriggersResponsetriggers
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListVariablesResponse
  --------------------------------------------------------------------}


Procedure TListVariablesResponse.Setvariables(AIndex : Integer; AValue : TListVariablesResponsevariables); 

begin
  If (Fvariables=AValue) then exit;
  Fvariables:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListVariablesResponsevariables
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMacro
  --------------------------------------------------------------------}


Procedure TMacro.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.SetcontainerId(AIndex : Integer; AValue : string); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.SetdisablingRuleId(AIndex : Integer; AValue : TMacrodisablingRuleId); 

begin
  If (FdisablingRuleId=AValue) then exit;
  FdisablingRuleId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.SetenablingRuleId(AIndex : Integer; AValue : TMacroenablingRuleId); 

begin
  If (FenablingRuleId=AValue) then exit;
  FenablingRuleId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.Setfingerprint(AIndex : Integer; AValue : string); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.SetmacroId(AIndex : Integer; AValue : string); 

begin
  If (FmacroId=AValue) then exit;
  FmacroId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.Setnotes(AIndex : Integer; AValue : string); 

begin
  If (Fnotes=AValue) then exit;
  Fnotes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.Setparameter(AIndex : Integer; AValue : TMacroparameter); 

begin
  If (Fparameter=AValue) then exit;
  Fparameter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.SetscheduleEndMs(AIndex : Integer; AValue : string); 

begin
  If (FscheduleEndMs=AValue) then exit;
  FscheduleEndMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.SetscheduleStartMs(AIndex : Integer; AValue : string); 

begin
  If (FscheduleStartMs=AValue) then exit;
  FscheduleStartMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.Set_type(AIndex : Integer; AValue : string); 

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
  TMacrodisablingRuleId
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMacroenablingRuleId
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMacroparameter
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TParameter
  --------------------------------------------------------------------}


Procedure TParameter.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParameter.Setlist(AIndex : Integer; AValue : TParameterlist); 

begin
  If (Flist=AValue) then exit;
  Flist:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParameter.Setmap(AIndex : Integer; AValue : TParametermap); 

begin
  If (Fmap=AValue) then exit;
  Fmap:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParameter.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParameter.Setvalue(AIndex : Integer; AValue : string); 

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
  TParameterlist
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TParametermap
  --------------------------------------------------------------------}




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


Procedure TRule.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.Setcondition(AIndex : Integer; AValue : TRulecondition); 

begin
  If (Fcondition=AValue) then exit;
  Fcondition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.SetcontainerId(AIndex : Integer; AValue : string); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.Setfingerprint(AIndex : Integer; AValue : string); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.Setnotes(AIndex : Integer; AValue : string); 

begin
  If (Fnotes=AValue) then exit;
  Fnotes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.SetruleId(AIndex : Integer; AValue : string); 

begin
  If (FruleId=AValue) then exit;
  FruleId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRulecondition
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTag
  --------------------------------------------------------------------}


Procedure TTag.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SetblockingRuleId(AIndex : Integer; AValue : TTagblockingRuleId); 

begin
  If (FblockingRuleId=AValue) then exit;
  FblockingRuleId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SetblockingTriggerId(AIndex : Integer; AValue : TTagblockingTriggerId); 

begin
  If (FblockingTriggerId=AValue) then exit;
  FblockingTriggerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SetcontainerId(AIndex : Integer; AValue : string); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.Setfingerprint(AIndex : Integer; AValue : string); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SetfiringRuleId(AIndex : Integer; AValue : TTagfiringRuleId); 

begin
  If (FfiringRuleId=AValue) then exit;
  FfiringRuleId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SetfiringTriggerId(AIndex : Integer; AValue : TTagfiringTriggerId); 

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



Procedure TTag.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.Setnotes(AIndex : Integer; AValue : string); 

begin
  If (Fnotes=AValue) then exit;
  Fnotes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.Setparameter(AIndex : Integer; AValue : TTagparameter); 

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



Procedure TTag.SetscheduleEndMs(AIndex : Integer; AValue : string); 

begin
  If (FscheduleEndMs=AValue) then exit;
  FscheduleEndMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SetscheduleStartMs(AIndex : Integer; AValue : string); 

begin
  If (FscheduleStartMs=AValue) then exit;
  FscheduleStartMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SettagId(AIndex : Integer; AValue : string); 

begin
  If (FtagId=AValue) then exit;
  FtagId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.Set_type(AIndex : Integer; AValue : string); 

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
  TTagblockingRuleId
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTagblockingTriggerId
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTagfiringRuleId
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTagfiringTriggerId
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTagparameter
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTrigger
  --------------------------------------------------------------------}


Procedure TTrigger.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.SetautoEventFilter(AIndex : Integer; AValue : TTriggerautoEventFilter); 

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



Procedure TTrigger.SetcontainerId(AIndex : Integer; AValue : string); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.SetcustomEventFilter(AIndex : Integer; AValue : TTriggercustomEventFilter); 

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



Procedure TTrigger.Setfilter(AIndex : Integer; AValue : TTriggerfilter); 

begin
  If (Ffilter=AValue) then exit;
  Ffilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.Setfingerprint(AIndex : Integer; AValue : string); 

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



Procedure TTrigger.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.SettriggerId(AIndex : Integer; AValue : string); 

begin
  If (FtriggerId=AValue) then exit;
  FtriggerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.Set_type(AIndex : Integer; AValue : string); 

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
  TTriggerautoEventFilter
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTriggercustomEventFilter
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTriggerfilter
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TUserAccess
  --------------------------------------------------------------------}


Procedure TUserAccess.SetaccountAccess(AIndex : Integer; AValue : TAccountAccess); 

begin
  If (FaccountAccess=AValue) then exit;
  FaccountAccess:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserAccess.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserAccess.SetcontainerAccess(AIndex : Integer; AValue : TUserAccesscontainerAccess); 

begin
  If (FcontainerAccess=AValue) then exit;
  FcontainerAccess:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserAccess.SetemailAddress(AIndex : Integer; AValue : string); 

begin
  If (FemailAddress=AValue) then exit;
  FemailAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserAccess.SetpermissionId(AIndex : Integer; AValue : string); 

begin
  If (FpermissionId=AValue) then exit;
  FpermissionId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUserAccesscontainerAccess
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TVariable
  --------------------------------------------------------------------}


Procedure TVariable.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.SetcontainerId(AIndex : Integer; AValue : string); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.SetdisablingTriggerId(AIndex : Integer; AValue : TVariabledisablingTriggerId); 

begin
  If (FdisablingTriggerId=AValue) then exit;
  FdisablingTriggerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.SetenablingTriggerId(AIndex : Integer; AValue : TVariableenablingTriggerId); 

begin
  If (FenablingTriggerId=AValue) then exit;
  FenablingTriggerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.Setfingerprint(AIndex : Integer; AValue : string); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.Setnotes(AIndex : Integer; AValue : string); 

begin
  If (Fnotes=AValue) then exit;
  Fnotes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.Setparameter(AIndex : Integer; AValue : TVariableparameter); 

begin
  If (Fparameter=AValue) then exit;
  Fparameter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.SetscheduleEndMs(AIndex : Integer; AValue : string); 

begin
  If (FscheduleEndMs=AValue) then exit;
  FscheduleEndMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.SetscheduleStartMs(AIndex : Integer; AValue : string); 

begin
  If (FscheduleStartMs=AValue) then exit;
  FscheduleStartMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.SetvariableId(AIndex : Integer; AValue : string); 

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
  TVariabledisablingTriggerId
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TVariableenablingTriggerId
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TVariableparameter
  --------------------------------------------------------------------}




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
  TAccountAccesspermission.RegisterObject;
  TCondition.RegisterObject;
  TConditionparameter.RegisterObject;
  TContainer.RegisterObject;
  TContainerdomainName.RegisterObject;
  TContainerenabledBuiltInVariable.RegisterObject;
  TContainerusageContext.RegisterObject;
  TContainerAccess.RegisterObject;
  TContainerAccesspermission.RegisterObject;
  TContainerVersion.RegisterObject;
  TContainerVersionmacro.RegisterObject;
  TContainerVersionrule.RegisterObject;
  TContainerVersiontag.RegisterObject;
  TContainerVersiontrigger.RegisterObject;
  TContainerVersionvariable.RegisterObject;
  TContainerVersionHeader.RegisterObject;
  TCreateContainerVersionRequestVersionOptions.RegisterObject;
  TCreateContainerVersionResponse.RegisterObject;
  TListAccountUsersResponse.RegisterObject;
  TListAccountUsersResponseuserAccess.RegisterObject;
  TListAccountsResponse.RegisterObject;
  TListAccountsResponseaccounts.RegisterObject;
  TListContainerVersionsResponse.RegisterObject;
  TListContainerVersionsResponsecontainerVersion.RegisterObject;
  TListContainerVersionsResponsecontainerVersionHeader.RegisterObject;
  TListContainersResponse.RegisterObject;
  TListContainersResponsecontainers.RegisterObject;
  TListMacrosResponse.RegisterObject;
  TListMacrosResponsemacros.RegisterObject;
  TListRulesResponse.RegisterObject;
  TListRulesResponserules.RegisterObject;
  TListTagsResponse.RegisterObject;
  TListTagsResponsetags.RegisterObject;
  TListTriggersResponse.RegisterObject;
  TListTriggersResponsetriggers.RegisterObject;
  TListVariablesResponse.RegisterObject;
  TListVariablesResponsevariables.RegisterObject;
  TMacro.RegisterObject;
  TMacrodisablingRuleId.RegisterObject;
  TMacroenablingRuleId.RegisterObject;
  TMacroparameter.RegisterObject;
  TParameter.RegisterObject;
  TParameterlist.RegisterObject;
  TParametermap.RegisterObject;
  TPublishContainerVersionResponse.RegisterObject;
  TRule.RegisterObject;
  TRulecondition.RegisterObject;
  TTag.RegisterObject;
  TTagblockingRuleId.RegisterObject;
  TTagblockingTriggerId.RegisterObject;
  TTagfiringRuleId.RegisterObject;
  TTagfiringTriggerId.RegisterObject;
  TTagparameter.RegisterObject;
  TTrigger.RegisterObject;
  TTriggerautoEventFilter.RegisterObject;
  TTriggercustomEventFilter.RegisterObject;
  TTriggerfilter.RegisterObject;
  TUserAccess.RegisterObject;
  TUserAccesscontainerAccess.RegisterObject;
  TVariable.RegisterObject;
  TVariabledisablingTriggerId.RegisterObject;
  TVariableenablingTriggerId.RegisterObject;
  TVariableparameter.RegisterObject;
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
