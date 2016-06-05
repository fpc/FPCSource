unit googleclouddebugger;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TRegisterDebuggeeRequest = Class;
  TDebuggee = Class;
  TStatusMessage = Class;
  TFormatMessage = Class;
  TSourceContext = Class;
  TCloudRepoSourceContext = Class;
  TRepoId = Class;
  TProjectRepoId = Class;
  TAliasContext = Class;
  TCloudWorkspaceSourceContext = Class;
  TCloudWorkspaceId = Class;
  TGerritSourceContext = Class;
  TGitSourceContext = Class;
  TExtendedSourceContext = Class;
  TRegisterDebuggeeResponse = Class;
  TListActiveBreakpointsResponse = Class;
  TBreakpoint = Class;
  TSourceLocation = Class;
  TStackFrame = Class;
  TVariable = Class;
  TUpdateActiveBreakpointRequest = Class;
  TUpdateActiveBreakpointResponse = Class;
  TSetBreakpointResponse = Class;
  TGetBreakpointResponse = Class;
  TEmpty = Class;
  TListBreakpointsResponse = Class;
  TListDebuggeesResponse = Class;
  TRegisterDebuggeeRequestArray = Array of TRegisterDebuggeeRequest;
  TDebuggeeArray = Array of TDebuggee;
  TStatusMessageArray = Array of TStatusMessage;
  TFormatMessageArray = Array of TFormatMessage;
  TSourceContextArray = Array of TSourceContext;
  TCloudRepoSourceContextArray = Array of TCloudRepoSourceContext;
  TRepoIdArray = Array of TRepoId;
  TProjectRepoIdArray = Array of TProjectRepoId;
  TAliasContextArray = Array of TAliasContext;
  TCloudWorkspaceSourceContextArray = Array of TCloudWorkspaceSourceContext;
  TCloudWorkspaceIdArray = Array of TCloudWorkspaceId;
  TGerritSourceContextArray = Array of TGerritSourceContext;
  TGitSourceContextArray = Array of TGitSourceContext;
  TExtendedSourceContextArray = Array of TExtendedSourceContext;
  TRegisterDebuggeeResponseArray = Array of TRegisterDebuggeeResponse;
  TListActiveBreakpointsResponseArray = Array of TListActiveBreakpointsResponse;
  TBreakpointArray = Array of TBreakpoint;
  TSourceLocationArray = Array of TSourceLocation;
  TStackFrameArray = Array of TStackFrame;
  TVariableArray = Array of TVariable;
  TUpdateActiveBreakpointRequestArray = Array of TUpdateActiveBreakpointRequest;
  TUpdateActiveBreakpointResponseArray = Array of TUpdateActiveBreakpointResponse;
  TSetBreakpointResponseArray = Array of TSetBreakpointResponse;
  TGetBreakpointResponseArray = Array of TGetBreakpointResponse;
  TEmptyArray = Array of TEmpty;
  TListBreakpointsResponseArray = Array of TListBreakpointsResponse;
  TListDebuggeesResponseArray = Array of TListDebuggeesResponse;
  //Anonymous types, using auto-generated names
  TDebuggeeTypelabels = Class;
  TExtendedSourceContextTypelabels = Class;
  TBreakpointTypelabels = Class;
  TDebuggeeTypesourceContextsArray = Array of TSourceContext;
  TDebuggeeTypeextSourceContextsArray = Array of TExtendedSourceContext;
  TListActiveBreakpointsResponseTypebreakpointsArray = Array of TBreakpoint;
  TBreakpointTypestackFramesArray = Array of TStackFrame;
  TBreakpointTypeevaluatedExpressionsArray = Array of TVariable;
  TBreakpointTypevariableTableArray = Array of TVariable;
  TStackFrameTypeargumentsArray = Array of TVariable;
  TStackFrameTypelocalsArray = Array of TVariable;
  TVariableTypemembersArray = Array of TVariable;
  TListBreakpointsResponseTypebreakpointsArray = Array of TBreakpoint;
  TListDebuggeesResponseTypedebuggeesArray = Array of TDebuggee;
  
  { --------------------------------------------------------------------
    TRegisterDebuggeeRequest
    --------------------------------------------------------------------}
  
  TRegisterDebuggeeRequest = Class(TGoogleBaseObject)
  Private
    Fdebuggee : TDebuggee;
  Protected
    //Property setters
    Procedure Setdebuggee(AIndex : Integer; const AValue : TDebuggee); virtual;
  Public
  Published
    Property debuggee : TDebuggee Index 0 Read Fdebuggee Write Setdebuggee;
  end;
  TRegisterDebuggeeRequestClass = Class of TRegisterDebuggeeRequest;
  
  { --------------------------------------------------------------------
    TDebuggeeTypelabels
    --------------------------------------------------------------------}
  
  TDebuggeeTypelabels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TDebuggeeTypelabelsClass = Class of TDebuggeeTypelabels;
  
  { --------------------------------------------------------------------
    TDebuggee
    --------------------------------------------------------------------}
  
  TDebuggee = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fproject : String;
    Funiquifier : String;
    Fdescription : String;
    FisInactive : boolean;
    FagentVersion : String;
    FisDisabled : boolean;
    Fstatus : TStatusMessage;
    FsourceContexts : TDebuggeeTypesourceContextsArray;
    FextSourceContexts : TDebuggeeTypeextSourceContextsArray;
    Flabels : TDebuggeeTypelabels;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setproject(AIndex : Integer; const AValue : String); virtual;
    Procedure Setuniquifier(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure SetisInactive(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetagentVersion(AIndex : Integer; const AValue : String); virtual;
    Procedure SetisDisabled(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setstatus(AIndex : Integer; const AValue : TStatusMessage); virtual;
    Procedure SetsourceContexts(AIndex : Integer; const AValue : TDebuggeeTypesourceContextsArray); virtual;
    Procedure SetextSourceContexts(AIndex : Integer; const AValue : TDebuggeeTypeextSourceContextsArray); virtual;
    Procedure Setlabels(AIndex : Integer; const AValue : TDebuggeeTypelabels); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property project : String Index 8 Read Fproject Write Setproject;
    Property uniquifier : String Index 16 Read Funiquifier Write Setuniquifier;
    Property description : String Index 24 Read Fdescription Write Setdescription;
    Property isInactive : boolean Index 32 Read FisInactive Write SetisInactive;
    Property agentVersion : String Index 40 Read FagentVersion Write SetagentVersion;
    Property isDisabled : boolean Index 48 Read FisDisabled Write SetisDisabled;
    Property status : TStatusMessage Index 56 Read Fstatus Write Setstatus;
    Property sourceContexts : TDebuggeeTypesourceContextsArray Index 64 Read FsourceContexts Write SetsourceContexts;
    Property extSourceContexts : TDebuggeeTypeextSourceContextsArray Index 72 Read FextSourceContexts Write SetextSourceContexts;
    Property labels : TDebuggeeTypelabels Index 80 Read Flabels Write Setlabels;
  end;
  TDebuggeeClass = Class of TDebuggee;
  
  { --------------------------------------------------------------------
    TStatusMessage
    --------------------------------------------------------------------}
  
  TStatusMessage = Class(TGoogleBaseObject)
  Private
    FisError : boolean;
    FrefersTo : String;
    Fdescription : TFormatMessage;
  Protected
    //Property setters
    Procedure SetisError(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetrefersTo(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : TFormatMessage); virtual;
  Public
  Published
    Property isError : boolean Index 0 Read FisError Write SetisError;
    Property refersTo : String Index 8 Read FrefersTo Write SetrefersTo;
    Property description : TFormatMessage Index 16 Read Fdescription Write Setdescription;
  end;
  TStatusMessageClass = Class of TStatusMessage;
  
  { --------------------------------------------------------------------
    TFormatMessage
    --------------------------------------------------------------------}
  
  TFormatMessage = Class(TGoogleBaseObject)
  Private
    Fformat : String;
    Fparameters : TStringArray;
  Protected
    //Property setters
    Procedure Setformat(AIndex : Integer; const AValue : String); virtual;
    Procedure Setparameters(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property format : String Index 0 Read Fformat Write Setformat;
    Property parameters : TStringArray Index 8 Read Fparameters Write Setparameters;
  end;
  TFormatMessageClass = Class of TFormatMessage;
  
  { --------------------------------------------------------------------
    TSourceContext
    --------------------------------------------------------------------}
  
  TSourceContext = Class(TGoogleBaseObject)
  Private
    FcloudRepo : TCloudRepoSourceContext;
    FcloudWorkspace : TCloudWorkspaceSourceContext;
    Fgerrit : TGerritSourceContext;
    Fgit : TGitSourceContext;
  Protected
    //Property setters
    Procedure SetcloudRepo(AIndex : Integer; const AValue : TCloudRepoSourceContext); virtual;
    Procedure SetcloudWorkspace(AIndex : Integer; const AValue : TCloudWorkspaceSourceContext); virtual;
    Procedure Setgerrit(AIndex : Integer; const AValue : TGerritSourceContext); virtual;
    Procedure Setgit(AIndex : Integer; const AValue : TGitSourceContext); virtual;
  Public
  Published
    Property cloudRepo : TCloudRepoSourceContext Index 0 Read FcloudRepo Write SetcloudRepo;
    Property cloudWorkspace : TCloudWorkspaceSourceContext Index 8 Read FcloudWorkspace Write SetcloudWorkspace;
    Property gerrit : TGerritSourceContext Index 16 Read Fgerrit Write Setgerrit;
    Property git : TGitSourceContext Index 24 Read Fgit Write Setgit;
  end;
  TSourceContextClass = Class of TSourceContext;
  
  { --------------------------------------------------------------------
    TCloudRepoSourceContext
    --------------------------------------------------------------------}
  
  TCloudRepoSourceContext = Class(TGoogleBaseObject)
  Private
    FrepoId : TRepoId;
    FrevisionId : String;
    FaliasName : String;
    FaliasContext : TAliasContext;
  Protected
    //Property setters
    Procedure SetrepoId(AIndex : Integer; const AValue : TRepoId); virtual;
    Procedure SetrevisionId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetaliasName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetaliasContext(AIndex : Integer; const AValue : TAliasContext); virtual;
  Public
  Published
    Property repoId : TRepoId Index 0 Read FrepoId Write SetrepoId;
    Property revisionId : String Index 8 Read FrevisionId Write SetrevisionId;
    Property aliasName : String Index 16 Read FaliasName Write SetaliasName;
    Property aliasContext : TAliasContext Index 24 Read FaliasContext Write SetaliasContext;
  end;
  TCloudRepoSourceContextClass = Class of TCloudRepoSourceContext;
  
  { --------------------------------------------------------------------
    TRepoId
    --------------------------------------------------------------------}
  
  TRepoId = Class(TGoogleBaseObject)
  Private
    FprojectRepoId : TProjectRepoId;
    Fuid : String;
  Protected
    //Property setters
    Procedure SetprojectRepoId(AIndex : Integer; const AValue : TProjectRepoId); virtual;
    Procedure Setuid(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property projectRepoId : TProjectRepoId Index 0 Read FprojectRepoId Write SetprojectRepoId;
    Property uid : String Index 8 Read Fuid Write Setuid;
  end;
  TRepoIdClass = Class of TRepoId;
  
  { --------------------------------------------------------------------
    TProjectRepoId
    --------------------------------------------------------------------}
  
  TProjectRepoId = Class(TGoogleBaseObject)
  Private
    FprojectId : String;
    FrepoName : String;
  Protected
    //Property setters
    Procedure SetprojectId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrepoName(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property projectId : String Index 0 Read FprojectId Write SetprojectId;
    Property repoName : String Index 8 Read FrepoName Write SetrepoName;
  end;
  TProjectRepoIdClass = Class of TProjectRepoId;
  
  { --------------------------------------------------------------------
    TAliasContext
    --------------------------------------------------------------------}
  
  TAliasContext = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property name : String Index 8 Read Fname Write Setname;
  end;
  TAliasContextClass = Class of TAliasContext;
  
  { --------------------------------------------------------------------
    TCloudWorkspaceSourceContext
    --------------------------------------------------------------------}
  
  TCloudWorkspaceSourceContext = Class(TGoogleBaseObject)
  Private
    FworkspaceId : TCloudWorkspaceId;
    FsnapshotId : String;
  Protected
    //Property setters
    Procedure SetworkspaceId(AIndex : Integer; const AValue : TCloudWorkspaceId); virtual;
    Procedure SetsnapshotId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property workspaceId : TCloudWorkspaceId Index 0 Read FworkspaceId Write SetworkspaceId;
    Property snapshotId : String Index 8 Read FsnapshotId Write SetsnapshotId;
  end;
  TCloudWorkspaceSourceContextClass = Class of TCloudWorkspaceSourceContext;
  
  { --------------------------------------------------------------------
    TCloudWorkspaceId
    --------------------------------------------------------------------}
  
  TCloudWorkspaceId = Class(TGoogleBaseObject)
  Private
    FrepoId : TRepoId;
    Fname : String;
  Protected
    //Property setters
    Procedure SetrepoId(AIndex : Integer; const AValue : TRepoId); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property repoId : TRepoId Index 0 Read FrepoId Write SetrepoId;
    Property name : String Index 8 Read Fname Write Setname;
  end;
  TCloudWorkspaceIdClass = Class of TCloudWorkspaceId;
  
  { --------------------------------------------------------------------
    TGerritSourceContext
    --------------------------------------------------------------------}
  
  TGerritSourceContext = Class(TGoogleBaseObject)
  Private
    FhostUri : String;
    FgerritProject : String;
    FrevisionId : String;
    FaliasName : String;
    FaliasContext : TAliasContext;
  Protected
    //Property setters
    Procedure SethostUri(AIndex : Integer; const AValue : String); virtual;
    Procedure SetgerritProject(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrevisionId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetaliasName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetaliasContext(AIndex : Integer; const AValue : TAliasContext); virtual;
  Public
  Published
    Property hostUri : String Index 0 Read FhostUri Write SethostUri;
    Property gerritProject : String Index 8 Read FgerritProject Write SetgerritProject;
    Property revisionId : String Index 16 Read FrevisionId Write SetrevisionId;
    Property aliasName : String Index 24 Read FaliasName Write SetaliasName;
    Property aliasContext : TAliasContext Index 32 Read FaliasContext Write SetaliasContext;
  end;
  TGerritSourceContextClass = Class of TGerritSourceContext;
  
  { --------------------------------------------------------------------
    TGitSourceContext
    --------------------------------------------------------------------}
  
  TGitSourceContext = Class(TGoogleBaseObject)
  Private
    Furl : String;
    FrevisionId : String;
  Protected
    //Property setters
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrevisionId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property url : String Index 0 Read Furl Write Seturl;
    Property revisionId : String Index 8 Read FrevisionId Write SetrevisionId;
  end;
  TGitSourceContextClass = Class of TGitSourceContext;
  
  { --------------------------------------------------------------------
    TExtendedSourceContextTypelabels
    --------------------------------------------------------------------}
  
  TExtendedSourceContextTypelabels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TExtendedSourceContextTypelabelsClass = Class of TExtendedSourceContextTypelabels;
  
  { --------------------------------------------------------------------
    TExtendedSourceContext
    --------------------------------------------------------------------}
  
  TExtendedSourceContext = Class(TGoogleBaseObject)
  Private
    Fcontext : TSourceContext;
    Flabels : TExtendedSourceContextTypelabels;
  Protected
    //Property setters
    Procedure Setcontext(AIndex : Integer; const AValue : TSourceContext); virtual;
    Procedure Setlabels(AIndex : Integer; const AValue : TExtendedSourceContextTypelabels); virtual;
  Public
  Published
    Property context : TSourceContext Index 0 Read Fcontext Write Setcontext;
    Property labels : TExtendedSourceContextTypelabels Index 8 Read Flabels Write Setlabels;
  end;
  TExtendedSourceContextClass = Class of TExtendedSourceContext;
  
  { --------------------------------------------------------------------
    TRegisterDebuggeeResponse
    --------------------------------------------------------------------}
  
  TRegisterDebuggeeResponse = Class(TGoogleBaseObject)
  Private
    Fdebuggee : TDebuggee;
  Protected
    //Property setters
    Procedure Setdebuggee(AIndex : Integer; const AValue : TDebuggee); virtual;
  Public
  Published
    Property debuggee : TDebuggee Index 0 Read Fdebuggee Write Setdebuggee;
  end;
  TRegisterDebuggeeResponseClass = Class of TRegisterDebuggeeResponse;
  
  { --------------------------------------------------------------------
    TListActiveBreakpointsResponse
    --------------------------------------------------------------------}
  
  TListActiveBreakpointsResponse = Class(TGoogleBaseObject)
  Private
    Fbreakpoints : TListActiveBreakpointsResponseTypebreakpointsArray;
    FnextWaitToken : String;
    FwaitExpired : boolean;
  Protected
    //Property setters
    Procedure Setbreakpoints(AIndex : Integer; const AValue : TListActiveBreakpointsResponseTypebreakpointsArray); virtual;
    Procedure SetnextWaitToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetwaitExpired(AIndex : Integer; const AValue : boolean); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property breakpoints : TListActiveBreakpointsResponseTypebreakpointsArray Index 0 Read Fbreakpoints Write Setbreakpoints;
    Property nextWaitToken : String Index 8 Read FnextWaitToken Write SetnextWaitToken;
    Property waitExpired : boolean Index 16 Read FwaitExpired Write SetwaitExpired;
  end;
  TListActiveBreakpointsResponseClass = Class of TListActiveBreakpointsResponse;
  
  { --------------------------------------------------------------------
    TBreakpointTypelabels
    --------------------------------------------------------------------}
  
  TBreakpointTypelabels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TBreakpointTypelabelsClass = Class of TBreakpointTypelabels;
  
  { --------------------------------------------------------------------
    TBreakpoint
    --------------------------------------------------------------------}
  
  TBreakpoint = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Faction : String;
    Flocation : TSourceLocation;
    Fcondition : String;
    Fexpressions : TStringArray;
    FlogMessageFormat : String;
    FlogLevel : String;
    FisFinalState : boolean;
    FcreateTime : String;
    FfinalTime : String;
    FuserEmail : String;
    Fstatus : TStatusMessage;
    FstackFrames : TBreakpointTypestackFramesArray;
    FevaluatedExpressions : TBreakpointTypeevaluatedExpressionsArray;
    FvariableTable : TBreakpointTypevariableTableArray;
    Flabels : TBreakpointTypelabels;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setaction(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlocation(AIndex : Integer; const AValue : TSourceLocation); virtual;
    Procedure Setcondition(AIndex : Integer; const AValue : String); virtual;
    Procedure Setexpressions(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetlogMessageFormat(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlogLevel(AIndex : Integer; const AValue : String); virtual;
    Procedure SetisFinalState(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetcreateTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfinalTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetuserEmail(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; const AValue : TStatusMessage); virtual;
    Procedure SetstackFrames(AIndex : Integer; const AValue : TBreakpointTypestackFramesArray); virtual;
    Procedure SetevaluatedExpressions(AIndex : Integer; const AValue : TBreakpointTypeevaluatedExpressionsArray); virtual;
    Procedure SetvariableTable(AIndex : Integer; const AValue : TBreakpointTypevariableTableArray); virtual;
    Procedure Setlabels(AIndex : Integer; const AValue : TBreakpointTypelabels); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property action : String Index 8 Read Faction Write Setaction;
    Property location : TSourceLocation Index 16 Read Flocation Write Setlocation;
    Property condition : String Index 24 Read Fcondition Write Setcondition;
    Property expressions : TStringArray Index 32 Read Fexpressions Write Setexpressions;
    Property logMessageFormat : String Index 40 Read FlogMessageFormat Write SetlogMessageFormat;
    Property logLevel : String Index 48 Read FlogLevel Write SetlogLevel;
    Property isFinalState : boolean Index 56 Read FisFinalState Write SetisFinalState;
    Property createTime : String Index 64 Read FcreateTime Write SetcreateTime;
    Property finalTime : String Index 72 Read FfinalTime Write SetfinalTime;
    Property userEmail : String Index 80 Read FuserEmail Write SetuserEmail;
    Property status : TStatusMessage Index 88 Read Fstatus Write Setstatus;
    Property stackFrames : TBreakpointTypestackFramesArray Index 96 Read FstackFrames Write SetstackFrames;
    Property evaluatedExpressions : TBreakpointTypeevaluatedExpressionsArray Index 104 Read FevaluatedExpressions Write SetevaluatedExpressions;
    Property variableTable : TBreakpointTypevariableTableArray Index 112 Read FvariableTable Write SetvariableTable;
    Property labels : TBreakpointTypelabels Index 120 Read Flabels Write Setlabels;
  end;
  TBreakpointClass = Class of TBreakpoint;
  
  { --------------------------------------------------------------------
    TSourceLocation
    --------------------------------------------------------------------}
  
  TSourceLocation = Class(TGoogleBaseObject)
  Private
    Fpath : String;
    Fline : integer;
  Protected
    //Property setters
    Procedure Setpath(AIndex : Integer; const AValue : String); virtual;
    Procedure Setline(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property path : String Index 0 Read Fpath Write Setpath;
    Property line : integer Index 8 Read Fline Write Setline;
  end;
  TSourceLocationClass = Class of TSourceLocation;
  
  { --------------------------------------------------------------------
    TStackFrame
    --------------------------------------------------------------------}
  
  TStackFrame = Class(TGoogleBaseObject)
  Private
    F_function : String;
    Flocation : TSourceLocation;
    Farguments : TStackFrameTypeargumentsArray;
    Flocals : TStackFrameTypelocalsArray;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_function(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlocation(AIndex : Integer; const AValue : TSourceLocation); virtual;
    Procedure Setarguments(AIndex : Integer; const AValue : TStackFrameTypeargumentsArray); virtual;
    Procedure Setlocals(AIndex : Integer; const AValue : TStackFrameTypelocalsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property _function : String Index 0 Read F_function Write Set_function;
    Property location : TSourceLocation Index 8 Read Flocation Write Setlocation;
    Property arguments : TStackFrameTypeargumentsArray Index 16 Read Farguments Write Setarguments;
    Property locals : TStackFrameTypelocalsArray Index 24 Read Flocals Write Setlocals;
  end;
  TStackFrameClass = Class of TStackFrame;
  
  { --------------------------------------------------------------------
    TVariable
    --------------------------------------------------------------------}
  
  TVariable = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Fvalue : String;
    F_type : String;
    Fmembers : TVariableTypemembersArray;
    FvarTableIndex : integer;
    Fstatus : TStatusMessage;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmembers(AIndex : Integer; const AValue : TVariableTypemembersArray); virtual;
    Procedure SetvarTableIndex(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setstatus(AIndex : Integer; const AValue : TStatusMessage); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property value : String Index 8 Read Fvalue Write Setvalue;
    Property _type : String Index 16 Read F_type Write Set_type;
    Property members : TVariableTypemembersArray Index 24 Read Fmembers Write Setmembers;
    Property varTableIndex : integer Index 32 Read FvarTableIndex Write SetvarTableIndex;
    Property status : TStatusMessage Index 40 Read Fstatus Write Setstatus;
  end;
  TVariableClass = Class of TVariable;
  
  { --------------------------------------------------------------------
    TUpdateActiveBreakpointRequest
    --------------------------------------------------------------------}
  
  TUpdateActiveBreakpointRequest = Class(TGoogleBaseObject)
  Private
    Fbreakpoint : TBreakpoint;
  Protected
    //Property setters
    Procedure Setbreakpoint(AIndex : Integer; const AValue : TBreakpoint); virtual;
  Public
  Published
    Property breakpoint : TBreakpoint Index 0 Read Fbreakpoint Write Setbreakpoint;
  end;
  TUpdateActiveBreakpointRequestClass = Class of TUpdateActiveBreakpointRequest;
  
  { --------------------------------------------------------------------
    TUpdateActiveBreakpointResponse
    --------------------------------------------------------------------}
  
  TUpdateActiveBreakpointResponse = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TUpdateActiveBreakpointResponseClass = Class of TUpdateActiveBreakpointResponse;
  
  { --------------------------------------------------------------------
    TSetBreakpointResponse
    --------------------------------------------------------------------}
  
  TSetBreakpointResponse = Class(TGoogleBaseObject)
  Private
    Fbreakpoint : TBreakpoint;
  Protected
    //Property setters
    Procedure Setbreakpoint(AIndex : Integer; const AValue : TBreakpoint); virtual;
  Public
  Published
    Property breakpoint : TBreakpoint Index 0 Read Fbreakpoint Write Setbreakpoint;
  end;
  TSetBreakpointResponseClass = Class of TSetBreakpointResponse;
  
  { --------------------------------------------------------------------
    TGetBreakpointResponse
    --------------------------------------------------------------------}
  
  TGetBreakpointResponse = Class(TGoogleBaseObject)
  Private
    Fbreakpoint : TBreakpoint;
  Protected
    //Property setters
    Procedure Setbreakpoint(AIndex : Integer; const AValue : TBreakpoint); virtual;
  Public
  Published
    Property breakpoint : TBreakpoint Index 0 Read Fbreakpoint Write Setbreakpoint;
  end;
  TGetBreakpointResponseClass = Class of TGetBreakpointResponse;
  
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
    TListBreakpointsResponse
    --------------------------------------------------------------------}
  
  TListBreakpointsResponse = Class(TGoogleBaseObject)
  Private
    Fbreakpoints : TListBreakpointsResponseTypebreakpointsArray;
    FnextWaitToken : String;
  Protected
    //Property setters
    Procedure Setbreakpoints(AIndex : Integer; const AValue : TListBreakpointsResponseTypebreakpointsArray); virtual;
    Procedure SetnextWaitToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property breakpoints : TListBreakpointsResponseTypebreakpointsArray Index 0 Read Fbreakpoints Write Setbreakpoints;
    Property nextWaitToken : String Index 8 Read FnextWaitToken Write SetnextWaitToken;
  end;
  TListBreakpointsResponseClass = Class of TListBreakpointsResponse;
  
  { --------------------------------------------------------------------
    TListDebuggeesResponse
    --------------------------------------------------------------------}
  
  TListDebuggeesResponse = Class(TGoogleBaseObject)
  Private
    Fdebuggees : TListDebuggeesResponseTypedebuggeesArray;
  Protected
    //Property setters
    Procedure Setdebuggees(AIndex : Integer; const AValue : TListDebuggeesResponseTypedebuggeesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property debuggees : TListDebuggeesResponseTypedebuggeesArray Index 0 Read Fdebuggees Write Setdebuggees;
  end;
  TListDebuggeesResponseClass = Class of TListDebuggeesResponse;
  
  { --------------------------------------------------------------------
    TControllerDebuggeesBreakpointsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TControllerDebuggeesBreakpointsResource, method List
  
  TControllerDebuggeesBreakpointsListOptions = Record
    waitToken : String;
    successOnTimeout : boolean;
  end;
  
  TControllerDebuggeesBreakpointsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(debuggeeId: string; AQuery : string  = '') : TListActiveBreakpointsResponse;
    Function List(debuggeeId: string; AQuery : TControllerDebuggeesBreakpointslistOptions) : TListActiveBreakpointsResponse;
    Function Update(debuggeeId: string; id: string; aUpdateActiveBreakpointRequest : TUpdateActiveBreakpointRequest) : TUpdateActiveBreakpointResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TControllerDebuggeesResource
    --------------------------------------------------------------------}
  
  TControllerDebuggeesResource = Class(TGoogleResource)
  Private
    FBreakpointsInstance : TControllerDebuggeesBreakpointsResource;
    Function GetBreakpointsInstance : TControllerDebuggeesBreakpointsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Register(aRegisterDebuggeeRequest : TRegisterDebuggeeRequest) : TRegisterDebuggeeResponse;
    Function CreateBreakpointsResource(AOwner : TComponent) : TControllerDebuggeesBreakpointsResource;virtual;overload;
    Function CreateBreakpointsResource : TControllerDebuggeesBreakpointsResource;virtual;overload;
    Property BreakpointsResource : TControllerDebuggeesBreakpointsResource Read GetBreakpointsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TControllerResource
    --------------------------------------------------------------------}
  
  TControllerResource = Class(TGoogleResource)
  Private
    FDebuggeesBreakpointsInstance : TControllerDebuggeesBreakpointsResource;
    FDebuggeesInstance : TControllerDebuggeesResource;
    Function GetDebuggeesBreakpointsInstance : TControllerDebuggeesBreakpointsResource;virtual;
    Function GetDebuggeesInstance : TControllerDebuggeesResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CreateDebuggeesBreakpointsResource(AOwner : TComponent) : TControllerDebuggeesBreakpointsResource;virtual;overload;
    Function CreateDebuggeesBreakpointsResource : TControllerDebuggeesBreakpointsResource;virtual;overload;
    Function CreateDebuggeesResource(AOwner : TComponent) : TControllerDebuggeesResource;virtual;overload;
    Function CreateDebuggeesResource : TControllerDebuggeesResource;virtual;overload;
    Property DebuggeesBreakpointsResource : TControllerDebuggeesBreakpointsResource Read GetDebuggeesBreakpointsInstance;
    Property DebuggeesResource : TControllerDebuggeesResource Read GetDebuggeesInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TDebuggerDebuggeesBreakpointsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TDebuggerDebuggeesBreakpointsResource, method Set
  
  TDebuggerDebuggeesBreakpointsSetOptions = Record
    clientVersion : String;
  end;
  
  
  //Optional query Options for TDebuggerDebuggeesBreakpointsResource, method Get
  
  TDebuggerDebuggeesBreakpointsGetOptions = Record
    clientVersion : String;
  end;
  
  
  //Optional query Options for TDebuggerDebuggeesBreakpointsResource, method Delete
  
  TDebuggerDebuggeesBreakpointsDeleteOptions = Record
    clientVersion : String;
  end;
  
  
  //Optional query Options for TDebuggerDebuggeesBreakpointsResource, method List
  
  TDebuggerDebuggeesBreakpointsListOptions = Record
    includeAllUsers : boolean;
    includeInactive : boolean;
    actionvalue : String;
    stripResults : boolean;
    waitToken : String;
    clientVersion : String;
  end;
  
  TDebuggerDebuggeesBreakpointsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function _set(debuggeeId: string; aBreakpoint : TBreakpoint; AQuery : string  = '') : TSetBreakpointResponse;
    Function _set(debuggeeId: string; aBreakpoint : TBreakpoint; AQuery : TDebuggerDebuggeesBreakpointssetOptions) : TSetBreakpointResponse;
    Function Get(debuggeeId: string; breakpointId: string; AQuery : string  = '') : TGetBreakpointResponse;
    Function Get(debuggeeId: string; breakpointId: string; AQuery : TDebuggerDebuggeesBreakpointsgetOptions) : TGetBreakpointResponse;
    Function Delete(debuggeeId: string; breakpointId: string; AQuery : string  = '') : TEmpty;
    Function Delete(debuggeeId: string; breakpointId: string; AQuery : TDebuggerDebuggeesBreakpointsdeleteOptions) : TEmpty;
    Function List(debuggeeId: string; AQuery : string  = '') : TListBreakpointsResponse;
    Function List(debuggeeId: string; AQuery : TDebuggerDebuggeesBreakpointslistOptions) : TListBreakpointsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TDebuggerDebuggeesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TDebuggerDebuggeesResource, method List
  
  TDebuggerDebuggeesListOptions = Record
    project : String;
    includeInactive : boolean;
    clientVersion : String;
  end;
  
  TDebuggerDebuggeesResource = Class(TGoogleResource)
  Private
    FBreakpointsInstance : TDebuggerDebuggeesBreakpointsResource;
    Function GetBreakpointsInstance : TDebuggerDebuggeesBreakpointsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TListDebuggeesResponse;
    Function List(AQuery : TDebuggerDebuggeeslistOptions) : TListDebuggeesResponse;
    Function CreateBreakpointsResource(AOwner : TComponent) : TDebuggerDebuggeesBreakpointsResource;virtual;overload;
    Function CreateBreakpointsResource : TDebuggerDebuggeesBreakpointsResource;virtual;overload;
    Property BreakpointsResource : TDebuggerDebuggeesBreakpointsResource Read GetBreakpointsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TDebuggerResource
    --------------------------------------------------------------------}
  
  TDebuggerResource = Class(TGoogleResource)
  Private
    FDebuggeesBreakpointsInstance : TDebuggerDebuggeesBreakpointsResource;
    FDebuggeesInstance : TDebuggerDebuggeesResource;
    Function GetDebuggeesBreakpointsInstance : TDebuggerDebuggeesBreakpointsResource;virtual;
    Function GetDebuggeesInstance : TDebuggerDebuggeesResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CreateDebuggeesBreakpointsResource(AOwner : TComponent) : TDebuggerDebuggeesBreakpointsResource;virtual;overload;
    Function CreateDebuggeesBreakpointsResource : TDebuggerDebuggeesBreakpointsResource;virtual;overload;
    Function CreateDebuggeesResource(AOwner : TComponent) : TDebuggerDebuggeesResource;virtual;overload;
    Function CreateDebuggeesResource : TDebuggerDebuggeesResource;virtual;overload;
    Property DebuggeesBreakpointsResource : TDebuggerDebuggeesBreakpointsResource Read GetDebuggeesBreakpointsInstance;
    Property DebuggeesResource : TDebuggerDebuggeesResource Read GetDebuggeesInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TClouddebuggerAPI
    --------------------------------------------------------------------}
  
  TClouddebuggerAPI = Class(TGoogleAPI)
  Private
    FControllerDebuggeesBreakpointsInstance : TControllerDebuggeesBreakpointsResource;
    FControllerDebuggeesInstance : TControllerDebuggeesResource;
    FControllerInstance : TControllerResource;
    FDebuggerDebuggeesBreakpointsInstance : TDebuggerDebuggeesBreakpointsResource;
    FDebuggerDebuggeesInstance : TDebuggerDebuggeesResource;
    FDebuggerInstance : TDebuggerResource;
    Function GetControllerDebuggeesBreakpointsInstance : TControllerDebuggeesBreakpointsResource;virtual;
    Function GetControllerDebuggeesInstance : TControllerDebuggeesResource;virtual;
    Function GetControllerInstance : TControllerResource;virtual;
    Function GetDebuggerDebuggeesBreakpointsInstance : TDebuggerDebuggeesBreakpointsResource;virtual;
    Function GetDebuggerDebuggeesInstance : TDebuggerDebuggeesResource;virtual;
    Function GetDebuggerInstance : TDebuggerResource;virtual;
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
    Function CreateControllerDebuggeesBreakpointsResource(AOwner : TComponent) : TControllerDebuggeesBreakpointsResource;virtual;overload;
    Function CreateControllerDebuggeesBreakpointsResource : TControllerDebuggeesBreakpointsResource;virtual;overload;
    Function CreateControllerDebuggeesResource(AOwner : TComponent) : TControllerDebuggeesResource;virtual;overload;
    Function CreateControllerDebuggeesResource : TControllerDebuggeesResource;virtual;overload;
    Function CreateControllerResource(AOwner : TComponent) : TControllerResource;virtual;overload;
    Function CreateControllerResource : TControllerResource;virtual;overload;
    Function CreateDebuggerDebuggeesBreakpointsResource(AOwner : TComponent) : TDebuggerDebuggeesBreakpointsResource;virtual;overload;
    Function CreateDebuggerDebuggeesBreakpointsResource : TDebuggerDebuggeesBreakpointsResource;virtual;overload;
    Function CreateDebuggerDebuggeesResource(AOwner : TComponent) : TDebuggerDebuggeesResource;virtual;overload;
    Function CreateDebuggerDebuggeesResource : TDebuggerDebuggeesResource;virtual;overload;
    Function CreateDebuggerResource(AOwner : TComponent) : TDebuggerResource;virtual;overload;
    Function CreateDebuggerResource : TDebuggerResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ControllerDebuggeesBreakpointsResource : TControllerDebuggeesBreakpointsResource Read GetControllerDebuggeesBreakpointsInstance;
    Property ControllerDebuggeesResource : TControllerDebuggeesResource Read GetControllerDebuggeesInstance;
    Property ControllerResource : TControllerResource Read GetControllerInstance;
    Property DebuggerDebuggeesBreakpointsResource : TDebuggerDebuggeesBreakpointsResource Read GetDebuggerDebuggeesBreakpointsInstance;
    Property DebuggerDebuggeesResource : TDebuggerDebuggeesResource Read GetDebuggerDebuggeesInstance;
    Property DebuggerResource : TDebuggerResource Read GetDebuggerInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TRegisterDebuggeeRequest
  --------------------------------------------------------------------}


Procedure TRegisterDebuggeeRequest.Setdebuggee(AIndex : Integer; const AValue : TDebuggee); 

begin
  If (Fdebuggee=AValue) then exit;
  Fdebuggee:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDebuggeeTypelabels
  --------------------------------------------------------------------}


Class Function TDebuggeeTypelabels.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TDebuggee
  --------------------------------------------------------------------}


Procedure TDebuggee.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDebuggee.Setproject(AIndex : Integer; const AValue : String); 

begin
  If (Fproject=AValue) then exit;
  Fproject:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDebuggee.Setuniquifier(AIndex : Integer; const AValue : String); 

begin
  If (Funiquifier=AValue) then exit;
  Funiquifier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDebuggee.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDebuggee.SetisInactive(AIndex : Integer; const AValue : boolean); 

begin
  If (FisInactive=AValue) then exit;
  FisInactive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDebuggee.SetagentVersion(AIndex : Integer; const AValue : String); 

begin
  If (FagentVersion=AValue) then exit;
  FagentVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDebuggee.SetisDisabled(AIndex : Integer; const AValue : boolean); 

begin
  If (FisDisabled=AValue) then exit;
  FisDisabled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDebuggee.Setstatus(AIndex : Integer; const AValue : TStatusMessage); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDebuggee.SetsourceContexts(AIndex : Integer; const AValue : TDebuggeeTypesourceContextsArray); 

begin
  If (FsourceContexts=AValue) then exit;
  FsourceContexts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDebuggee.SetextSourceContexts(AIndex : Integer; const AValue : TDebuggeeTypeextSourceContextsArray); 

begin
  If (FextSourceContexts=AValue) then exit;
  FextSourceContexts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDebuggee.Setlabels(AIndex : Integer; const AValue : TDebuggeeTypelabels); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDebuggee.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'sourcecontexts' : SetLength(FsourceContexts,ALength);
  'extsourcecontexts' : SetLength(FextSourceContexts,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TStatusMessage
  --------------------------------------------------------------------}


Procedure TStatusMessage.SetisError(AIndex : Integer; const AValue : boolean); 

begin
  If (FisError=AValue) then exit;
  FisError:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStatusMessage.SetrefersTo(AIndex : Integer; const AValue : String); 

begin
  If (FrefersTo=AValue) then exit;
  FrefersTo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStatusMessage.Setdescription(AIndex : Integer; const AValue : TFormatMessage); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFormatMessage
  --------------------------------------------------------------------}


Procedure TFormatMessage.Setformat(AIndex : Integer; const AValue : String); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFormatMessage.Setparameters(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fparameters=AValue) then exit;
  Fparameters:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TFormatMessage.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'parameters' : SetLength(Fparameters,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSourceContext
  --------------------------------------------------------------------}


Procedure TSourceContext.SetcloudRepo(AIndex : Integer; const AValue : TCloudRepoSourceContext); 

begin
  If (FcloudRepo=AValue) then exit;
  FcloudRepo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceContext.SetcloudWorkspace(AIndex : Integer; const AValue : TCloudWorkspaceSourceContext); 

begin
  If (FcloudWorkspace=AValue) then exit;
  FcloudWorkspace:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceContext.Setgerrit(AIndex : Integer; const AValue : TGerritSourceContext); 

begin
  If (Fgerrit=AValue) then exit;
  Fgerrit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceContext.Setgit(AIndex : Integer; const AValue : TGitSourceContext); 

begin
  If (Fgit=AValue) then exit;
  Fgit:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCloudRepoSourceContext
  --------------------------------------------------------------------}


Procedure TCloudRepoSourceContext.SetrepoId(AIndex : Integer; const AValue : TRepoId); 

begin
  If (FrepoId=AValue) then exit;
  FrepoId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCloudRepoSourceContext.SetrevisionId(AIndex : Integer; const AValue : String); 

begin
  If (FrevisionId=AValue) then exit;
  FrevisionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCloudRepoSourceContext.SetaliasName(AIndex : Integer; const AValue : String); 

begin
  If (FaliasName=AValue) then exit;
  FaliasName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCloudRepoSourceContext.SetaliasContext(AIndex : Integer; const AValue : TAliasContext); 

begin
  If (FaliasContext=AValue) then exit;
  FaliasContext:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRepoId
  --------------------------------------------------------------------}


Procedure TRepoId.SetprojectRepoId(AIndex : Integer; const AValue : TProjectRepoId); 

begin
  If (FprojectRepoId=AValue) then exit;
  FprojectRepoId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRepoId.Setuid(AIndex : Integer; const AValue : String); 

begin
  If (Fuid=AValue) then exit;
  Fuid:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProjectRepoId
  --------------------------------------------------------------------}


Procedure TProjectRepoId.SetprojectId(AIndex : Integer; const AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProjectRepoId.SetrepoName(AIndex : Integer; const AValue : String); 

begin
  If (FrepoName=AValue) then exit;
  FrepoName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAliasContext
  --------------------------------------------------------------------}


Procedure TAliasContext.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAliasContext.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCloudWorkspaceSourceContext
  --------------------------------------------------------------------}


Procedure TCloudWorkspaceSourceContext.SetworkspaceId(AIndex : Integer; const AValue : TCloudWorkspaceId); 

begin
  If (FworkspaceId=AValue) then exit;
  FworkspaceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCloudWorkspaceSourceContext.SetsnapshotId(AIndex : Integer; const AValue : String); 

begin
  If (FsnapshotId=AValue) then exit;
  FsnapshotId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCloudWorkspaceId
  --------------------------------------------------------------------}


Procedure TCloudWorkspaceId.SetrepoId(AIndex : Integer; const AValue : TRepoId); 

begin
  If (FrepoId=AValue) then exit;
  FrepoId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCloudWorkspaceId.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGerritSourceContext
  --------------------------------------------------------------------}


Procedure TGerritSourceContext.SethostUri(AIndex : Integer; const AValue : String); 

begin
  If (FhostUri=AValue) then exit;
  FhostUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGerritSourceContext.SetgerritProject(AIndex : Integer; const AValue : String); 

begin
  If (FgerritProject=AValue) then exit;
  FgerritProject:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGerritSourceContext.SetrevisionId(AIndex : Integer; const AValue : String); 

begin
  If (FrevisionId=AValue) then exit;
  FrevisionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGerritSourceContext.SetaliasName(AIndex : Integer; const AValue : String); 

begin
  If (FaliasName=AValue) then exit;
  FaliasName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGerritSourceContext.SetaliasContext(AIndex : Integer; const AValue : TAliasContext); 

begin
  If (FaliasContext=AValue) then exit;
  FaliasContext:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGitSourceContext
  --------------------------------------------------------------------}


Procedure TGitSourceContext.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGitSourceContext.SetrevisionId(AIndex : Integer; const AValue : String); 

begin
  If (FrevisionId=AValue) then exit;
  FrevisionId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TExtendedSourceContextTypelabels
  --------------------------------------------------------------------}


Class Function TExtendedSourceContextTypelabels.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TExtendedSourceContext
  --------------------------------------------------------------------}


Procedure TExtendedSourceContext.Setcontext(AIndex : Integer; const AValue : TSourceContext); 

begin
  If (Fcontext=AValue) then exit;
  Fcontext:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExtendedSourceContext.Setlabels(AIndex : Integer; const AValue : TExtendedSourceContextTypelabels); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRegisterDebuggeeResponse
  --------------------------------------------------------------------}


Procedure TRegisterDebuggeeResponse.Setdebuggee(AIndex : Integer; const AValue : TDebuggee); 

begin
  If (Fdebuggee=AValue) then exit;
  Fdebuggee:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListActiveBreakpointsResponse
  --------------------------------------------------------------------}


Procedure TListActiveBreakpointsResponse.Setbreakpoints(AIndex : Integer; const AValue : TListActiveBreakpointsResponseTypebreakpointsArray); 

begin
  If (Fbreakpoints=AValue) then exit;
  Fbreakpoints:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListActiveBreakpointsResponse.SetnextWaitToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextWaitToken=AValue) then exit;
  FnextWaitToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListActiveBreakpointsResponse.SetwaitExpired(AIndex : Integer; const AValue : boolean); 

begin
  If (FwaitExpired=AValue) then exit;
  FwaitExpired:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListActiveBreakpointsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'breakpoints' : SetLength(Fbreakpoints,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBreakpointTypelabels
  --------------------------------------------------------------------}


Class Function TBreakpointTypelabels.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TBreakpoint
  --------------------------------------------------------------------}


Procedure TBreakpoint.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBreakpoint.Setaction(AIndex : Integer; const AValue : String); 

begin
  If (Faction=AValue) then exit;
  Faction:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBreakpoint.Setlocation(AIndex : Integer; const AValue : TSourceLocation); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBreakpoint.Setcondition(AIndex : Integer; const AValue : String); 

begin
  If (Fcondition=AValue) then exit;
  Fcondition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBreakpoint.Setexpressions(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fexpressions=AValue) then exit;
  Fexpressions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBreakpoint.SetlogMessageFormat(AIndex : Integer; const AValue : String); 

begin
  If (FlogMessageFormat=AValue) then exit;
  FlogMessageFormat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBreakpoint.SetlogLevel(AIndex : Integer; const AValue : String); 

begin
  If (FlogLevel=AValue) then exit;
  FlogLevel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBreakpoint.SetisFinalState(AIndex : Integer; const AValue : boolean); 

begin
  If (FisFinalState=AValue) then exit;
  FisFinalState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBreakpoint.SetcreateTime(AIndex : Integer; const AValue : String); 

begin
  If (FcreateTime=AValue) then exit;
  FcreateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBreakpoint.SetfinalTime(AIndex : Integer; const AValue : String); 

begin
  If (FfinalTime=AValue) then exit;
  FfinalTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBreakpoint.SetuserEmail(AIndex : Integer; const AValue : String); 

begin
  If (FuserEmail=AValue) then exit;
  FuserEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBreakpoint.Setstatus(AIndex : Integer; const AValue : TStatusMessage); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBreakpoint.SetstackFrames(AIndex : Integer; const AValue : TBreakpointTypestackFramesArray); 

begin
  If (FstackFrames=AValue) then exit;
  FstackFrames:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBreakpoint.SetevaluatedExpressions(AIndex : Integer; const AValue : TBreakpointTypeevaluatedExpressionsArray); 

begin
  If (FevaluatedExpressions=AValue) then exit;
  FevaluatedExpressions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBreakpoint.SetvariableTable(AIndex : Integer; const AValue : TBreakpointTypevariableTableArray); 

begin
  If (FvariableTable=AValue) then exit;
  FvariableTable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBreakpoint.Setlabels(AIndex : Integer; const AValue : TBreakpointTypelabels); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBreakpoint.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'expressions' : SetLength(Fexpressions,ALength);
  'stackframes' : SetLength(FstackFrames,ALength);
  'evaluatedexpressions' : SetLength(FevaluatedExpressions,ALength);
  'variabletable' : SetLength(FvariableTable,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSourceLocation
  --------------------------------------------------------------------}


Procedure TSourceLocation.Setpath(AIndex : Integer; const AValue : String); 

begin
  If (Fpath=AValue) then exit;
  Fpath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceLocation.Setline(AIndex : Integer; const AValue : integer); 

begin
  If (Fline=AValue) then exit;
  Fline:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStackFrame
  --------------------------------------------------------------------}


Procedure TStackFrame.Set_function(AIndex : Integer; const AValue : String); 

begin
  If (F_function=AValue) then exit;
  F_function:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStackFrame.Setlocation(AIndex : Integer; const AValue : TSourceLocation); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStackFrame.Setarguments(AIndex : Integer; const AValue : TStackFrameTypeargumentsArray); 

begin
  If (Farguments=AValue) then exit;
  Farguments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStackFrame.Setlocals(AIndex : Integer; const AValue : TStackFrameTypelocalsArray); 

begin
  If (Flocals=AValue) then exit;
  Flocals:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TStackFrame.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_function' : Result:='function';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TStackFrame.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'arguments' : SetLength(Farguments,ALength);
  'locals' : SetLength(Flocals,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TVariable
  --------------------------------------------------------------------}


Procedure TVariable.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.Setmembers(AIndex : Integer; const AValue : TVariableTypemembersArray); 

begin
  If (Fmembers=AValue) then exit;
  Fmembers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.SetvarTableIndex(AIndex : Integer; const AValue : integer); 

begin
  If (FvarTableIndex=AValue) then exit;
  FvarTableIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.Setstatus(AIndex : Integer; const AValue : TStatusMessage); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
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
  'members' : SetLength(Fmembers,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TUpdateActiveBreakpointRequest
  --------------------------------------------------------------------}


Procedure TUpdateActiveBreakpointRequest.Setbreakpoint(AIndex : Integer; const AValue : TBreakpoint); 

begin
  If (Fbreakpoint=AValue) then exit;
  Fbreakpoint:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUpdateActiveBreakpointResponse
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSetBreakpointResponse
  --------------------------------------------------------------------}


Procedure TSetBreakpointResponse.Setbreakpoint(AIndex : Integer; const AValue : TBreakpoint); 

begin
  If (Fbreakpoint=AValue) then exit;
  Fbreakpoint:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGetBreakpointResponse
  --------------------------------------------------------------------}


Procedure TGetBreakpointResponse.Setbreakpoint(AIndex : Integer; const AValue : TBreakpoint); 

begin
  If (Fbreakpoint=AValue) then exit;
  Fbreakpoint:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEmpty
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListBreakpointsResponse
  --------------------------------------------------------------------}


Procedure TListBreakpointsResponse.Setbreakpoints(AIndex : Integer; const AValue : TListBreakpointsResponseTypebreakpointsArray); 

begin
  If (Fbreakpoints=AValue) then exit;
  Fbreakpoints:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListBreakpointsResponse.SetnextWaitToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextWaitToken=AValue) then exit;
  FnextWaitToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListBreakpointsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'breakpoints' : SetLength(Fbreakpoints,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListDebuggeesResponse
  --------------------------------------------------------------------}


Procedure TListDebuggeesResponse.Setdebuggees(AIndex : Integer; const AValue : TListDebuggeesResponseTypedebuggeesArray); 

begin
  If (Fdebuggees=AValue) then exit;
  Fdebuggees:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListDebuggeesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'debuggees' : SetLength(Fdebuggees,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TControllerDebuggeesBreakpointsResource
  --------------------------------------------------------------------}


Class Function TControllerDebuggeesBreakpointsResource.ResourceName : String;

begin
  Result:='breakpoints';
end;

Class Function TControllerDebuggeesBreakpointsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TclouddebuggerAPI;
end;

Function TControllerDebuggeesBreakpointsResource.List(debuggeeId: string; AQuery : string = '') : TListActiveBreakpointsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v2/controller/debuggees/{debuggeeId}/breakpoints';
  _Methodid   = 'clouddebugger.controller.debuggees.breakpoints.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['debuggeeId',debuggeeId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListActiveBreakpointsResponse) as TListActiveBreakpointsResponse;
end;


Function TControllerDebuggeesBreakpointsResource.List(debuggeeId: string; AQuery : TControllerDebuggeesBreakpointslistOptions) : TListActiveBreakpointsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'waitToken',AQuery.waitToken);
  AddToQuery(_Q,'successOnTimeout',AQuery.successOnTimeout);
  Result:=List(debuggeeId,_Q);
end;

Function TControllerDebuggeesBreakpointsResource.Update(debuggeeId: string; id: string; aUpdateActiveBreakpointRequest : TUpdateActiveBreakpointRequest) : TUpdateActiveBreakpointResponse;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'v2/controller/debuggees/{debuggeeId}/breakpoints/{id}';
  _Methodid   = 'clouddebugger.controller.debuggees.breakpoints.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['debuggeeId',debuggeeId,'id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aUpdateActiveBreakpointRequest,TUpdateActiveBreakpointResponse) as TUpdateActiveBreakpointResponse;
end;



{ --------------------------------------------------------------------
  TControllerDebuggeesResource
  --------------------------------------------------------------------}


Class Function TControllerDebuggeesResource.ResourceName : String;

begin
  Result:='debuggees';
end;

Class Function TControllerDebuggeesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TclouddebuggerAPI;
end;

Function TControllerDebuggeesResource.Register(aRegisterDebuggeeRequest : TRegisterDebuggeeRequest) : TRegisterDebuggeeResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v2/controller/debuggees/register';
  _Methodid   = 'clouddebugger.controller.debuggees.register';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aRegisterDebuggeeRequest,TRegisterDebuggeeResponse) as TRegisterDebuggeeResponse;
end;



Function TControllerDebuggeesResource.GetBreakpointsInstance : TControllerDebuggeesBreakpointsResource;

begin
  if (FBreakpointsInstance=Nil) then
    FBreakpointsInstance:=CreateBreakpointsResource;
  Result:=FBreakpointsInstance;
end;

Function TControllerDebuggeesResource.CreateBreakpointsResource : TControllerDebuggeesBreakpointsResource;

begin
  Result:=CreateBreakpointsResource(Self);
end;


Function TControllerDebuggeesResource.CreateBreakpointsResource(AOwner : TComponent) : TControllerDebuggeesBreakpointsResource;

begin
  Result:=TControllerDebuggeesBreakpointsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TControllerResource
  --------------------------------------------------------------------}


Class Function TControllerResource.ResourceName : String;

begin
  Result:='controller';
end;

Class Function TControllerResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TclouddebuggerAPI;
end;



Function TControllerResource.GetDebuggeesBreakpointsInstance : TControllerDebuggeesBreakpointsResource;

begin
  if (FDebuggeesBreakpointsInstance=Nil) then
    FDebuggeesBreakpointsInstance:=CreateDebuggeesBreakpointsResource;
  Result:=FDebuggeesBreakpointsInstance;
end;

Function TControllerResource.CreateDebuggeesBreakpointsResource : TControllerDebuggeesBreakpointsResource;

begin
  Result:=CreateDebuggeesBreakpointsResource(Self);
end;


Function TControllerResource.CreateDebuggeesBreakpointsResource(AOwner : TComponent) : TControllerDebuggeesBreakpointsResource;

begin
  Result:=TControllerDebuggeesBreakpointsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TControllerResource.GetDebuggeesInstance : TControllerDebuggeesResource;

begin
  if (FDebuggeesInstance=Nil) then
    FDebuggeesInstance:=CreateDebuggeesResource;
  Result:=FDebuggeesInstance;
end;

Function TControllerResource.CreateDebuggeesResource : TControllerDebuggeesResource;

begin
  Result:=CreateDebuggeesResource(Self);
end;


Function TControllerResource.CreateDebuggeesResource(AOwner : TComponent) : TControllerDebuggeesResource;

begin
  Result:=TControllerDebuggeesResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TDebuggerDebuggeesBreakpointsResource
  --------------------------------------------------------------------}


Class Function TDebuggerDebuggeesBreakpointsResource.ResourceName : String;

begin
  Result:='breakpoints';
end;

Class Function TDebuggerDebuggeesBreakpointsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TclouddebuggerAPI;
end;

Function TDebuggerDebuggeesBreakpointsResource._set(debuggeeId: string; aBreakpoint : TBreakpoint; AQuery : string = '') : TSetBreakpointResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v2/debugger/debuggees/{debuggeeId}/breakpoints/set';
  _Methodid   = 'clouddebugger.debugger.debuggees.breakpoints.set';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['debuggeeId',debuggeeId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aBreakpoint,TSetBreakpointResponse) as TSetBreakpointResponse;
end;


Function TDebuggerDebuggeesBreakpointsResource._set(debuggeeId: string; aBreakpoint : TBreakpoint; AQuery : TDebuggerDebuggeesBreakpointssetOptions) : TSetBreakpointResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'clientVersion',AQuery.clientVersion);
  Result:=_set(debuggeeId,aBreakpoint,_Q);
end;

Function TDebuggerDebuggeesBreakpointsResource.Get(debuggeeId: string; breakpointId: string; AQuery : string = '') : TGetBreakpointResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v2/debugger/debuggees/{debuggeeId}/breakpoints/{breakpointId}';
  _Methodid   = 'clouddebugger.debugger.debuggees.breakpoints.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['debuggeeId',debuggeeId,'breakpointId',breakpointId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TGetBreakpointResponse) as TGetBreakpointResponse;
end;


Function TDebuggerDebuggeesBreakpointsResource.Get(debuggeeId: string; breakpointId: string; AQuery : TDebuggerDebuggeesBreakpointsgetOptions) : TGetBreakpointResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'clientVersion',AQuery.clientVersion);
  Result:=Get(debuggeeId,breakpointId,_Q);
end;

Function TDebuggerDebuggeesBreakpointsResource.Delete(debuggeeId: string; breakpointId: string; AQuery : string = '') : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v2/debugger/debuggees/{debuggeeId}/breakpoints/{breakpointId}';
  _Methodid   = 'clouddebugger.debugger.debuggees.breakpoints.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['debuggeeId',debuggeeId,'breakpointId',breakpointId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TEmpty) as TEmpty;
end;


Function TDebuggerDebuggeesBreakpointsResource.Delete(debuggeeId: string; breakpointId: string; AQuery : TDebuggerDebuggeesBreakpointsdeleteOptions) : TEmpty;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'clientVersion',AQuery.clientVersion);
  Result:=Delete(debuggeeId,breakpointId,_Q);
end;

Function TDebuggerDebuggeesBreakpointsResource.List(debuggeeId: string; AQuery : string = '') : TListBreakpointsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v2/debugger/debuggees/{debuggeeId}/breakpoints';
  _Methodid   = 'clouddebugger.debugger.debuggees.breakpoints.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['debuggeeId',debuggeeId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListBreakpointsResponse) as TListBreakpointsResponse;
end;


Function TDebuggerDebuggeesBreakpointsResource.List(debuggeeId: string; AQuery : TDebuggerDebuggeesBreakpointslistOptions) : TListBreakpointsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'includeAllUsers',AQuery.includeAllUsers);
  AddToQuery(_Q,'includeInactive',AQuery.includeInactive);
  AddToQuery(_Q,'action.value',AQuery.actionvalue);
  AddToQuery(_Q,'stripResults',AQuery.stripResults);
  AddToQuery(_Q,'waitToken',AQuery.waitToken);
  AddToQuery(_Q,'clientVersion',AQuery.clientVersion);
  Result:=List(debuggeeId,_Q);
end;



{ --------------------------------------------------------------------
  TDebuggerDebuggeesResource
  --------------------------------------------------------------------}


Class Function TDebuggerDebuggeesResource.ResourceName : String;

begin
  Result:='debuggees';
end;

Class Function TDebuggerDebuggeesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TclouddebuggerAPI;
end;

Function TDebuggerDebuggeesResource.List(AQuery : string = '') : TListDebuggeesResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v2/debugger/debuggees';
  _Methodid   = 'clouddebugger.debugger.debuggees.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TListDebuggeesResponse) as TListDebuggeesResponse;
end;


Function TDebuggerDebuggeesResource.List(AQuery : TDebuggerDebuggeeslistOptions) : TListDebuggeesResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'project',AQuery.project);
  AddToQuery(_Q,'includeInactive',AQuery.includeInactive);
  AddToQuery(_Q,'clientVersion',AQuery.clientVersion);
  Result:=List(_Q);
end;



Function TDebuggerDebuggeesResource.GetBreakpointsInstance : TDebuggerDebuggeesBreakpointsResource;

begin
  if (FBreakpointsInstance=Nil) then
    FBreakpointsInstance:=CreateBreakpointsResource;
  Result:=FBreakpointsInstance;
end;

Function TDebuggerDebuggeesResource.CreateBreakpointsResource : TDebuggerDebuggeesBreakpointsResource;

begin
  Result:=CreateBreakpointsResource(Self);
end;


Function TDebuggerDebuggeesResource.CreateBreakpointsResource(AOwner : TComponent) : TDebuggerDebuggeesBreakpointsResource;

begin
  Result:=TDebuggerDebuggeesBreakpointsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TDebuggerResource
  --------------------------------------------------------------------}


Class Function TDebuggerResource.ResourceName : String;

begin
  Result:='debugger';
end;

Class Function TDebuggerResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TclouddebuggerAPI;
end;



Function TDebuggerResource.GetDebuggeesBreakpointsInstance : TDebuggerDebuggeesBreakpointsResource;

begin
  if (FDebuggeesBreakpointsInstance=Nil) then
    FDebuggeesBreakpointsInstance:=CreateDebuggeesBreakpointsResource;
  Result:=FDebuggeesBreakpointsInstance;
end;

Function TDebuggerResource.CreateDebuggeesBreakpointsResource : TDebuggerDebuggeesBreakpointsResource;

begin
  Result:=CreateDebuggeesBreakpointsResource(Self);
end;


Function TDebuggerResource.CreateDebuggeesBreakpointsResource(AOwner : TComponent) : TDebuggerDebuggeesBreakpointsResource;

begin
  Result:=TDebuggerDebuggeesBreakpointsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TDebuggerResource.GetDebuggeesInstance : TDebuggerDebuggeesResource;

begin
  if (FDebuggeesInstance=Nil) then
    FDebuggeesInstance:=CreateDebuggeesResource;
  Result:=FDebuggeesInstance;
end;

Function TDebuggerResource.CreateDebuggeesResource : TDebuggerDebuggeesResource;

begin
  Result:=CreateDebuggeesResource(Self);
end;


Function TDebuggerResource.CreateDebuggeesResource(AOwner : TComponent) : TDebuggerDebuggeesResource;

begin
  Result:=TDebuggerDebuggeesResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TClouddebuggerAPI
  --------------------------------------------------------------------}

Class Function TClouddebuggerAPI.APIName : String;

begin
  Result:='clouddebugger';
end;

Class Function TClouddebuggerAPI.APIVersion : String;

begin
  Result:='v2';
end;

Class Function TClouddebuggerAPI.APIRevision : String;

begin
  Result:='20160309';
end;

Class Function TClouddebuggerAPI.APIID : String;

begin
  Result:='clouddebugger:v2';
end;

Class Function TClouddebuggerAPI.APITitle : String;

begin
  Result:='Google Cloud Debugger API';
end;

Class Function TClouddebuggerAPI.APIDescription : String;

begin
  Result:='Examines the call stack and variables of a running application without stopping or slowing it down.';
end;

Class Function TClouddebuggerAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TClouddebuggerAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TClouddebuggerAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TClouddebuggerAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TClouddebuggerAPI.APIdocumentationLink : String;

begin
  Result:='https://cloud.google.com/tools/cloud-debugger';
end;

Class Function TClouddebuggerAPI.APIrootUrl : string;

begin
  Result:='https://clouddebugger.googleapis.com/';
end;

Class Function TClouddebuggerAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TClouddebuggerAPI.APIbaseURL : String;

begin
  Result:='https://clouddebugger.googleapis.com/';
end;

Class Function TClouddebuggerAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TClouddebuggerAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TClouddebuggerAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TClouddebuggerAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,3);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  Result[1].Name:='https://www.googleapis.com/auth/cloud_debugger';
  Result[1].Description:='Manage cloud debugger';
  Result[2].Name:='https://www.googleapis.com/auth/cloud_debugletcontroller';
  Result[2].Description:='Manage active breakpoints in cloud debugger';
  
end;

Class Function TClouddebuggerAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TClouddebuggerAPI.RegisterAPIResources;

begin
  TRegisterDebuggeeRequest.RegisterObject;
  TDebuggeeTypelabels.RegisterObject;
  TDebuggee.RegisterObject;
  TStatusMessage.RegisterObject;
  TFormatMessage.RegisterObject;
  TSourceContext.RegisterObject;
  TCloudRepoSourceContext.RegisterObject;
  TRepoId.RegisterObject;
  TProjectRepoId.RegisterObject;
  TAliasContext.RegisterObject;
  TCloudWorkspaceSourceContext.RegisterObject;
  TCloudWorkspaceId.RegisterObject;
  TGerritSourceContext.RegisterObject;
  TGitSourceContext.RegisterObject;
  TExtendedSourceContextTypelabels.RegisterObject;
  TExtendedSourceContext.RegisterObject;
  TRegisterDebuggeeResponse.RegisterObject;
  TListActiveBreakpointsResponse.RegisterObject;
  TBreakpointTypelabels.RegisterObject;
  TBreakpoint.RegisterObject;
  TSourceLocation.RegisterObject;
  TStackFrame.RegisterObject;
  TVariable.RegisterObject;
  TUpdateActiveBreakpointRequest.RegisterObject;
  TUpdateActiveBreakpointResponse.RegisterObject;
  TSetBreakpointResponse.RegisterObject;
  TGetBreakpointResponse.RegisterObject;
  TEmpty.RegisterObject;
  TListBreakpointsResponse.RegisterObject;
  TListDebuggeesResponse.RegisterObject;
end;


Function TClouddebuggerAPI.GetControllerDebuggeesBreakpointsInstance : TControllerDebuggeesBreakpointsResource;

begin
  if (FControllerDebuggeesBreakpointsInstance=Nil) then
    FControllerDebuggeesBreakpointsInstance:=CreateControllerDebuggeesBreakpointsResource;
  Result:=FControllerDebuggeesBreakpointsInstance;
end;

Function TClouddebuggerAPI.CreateControllerDebuggeesBreakpointsResource : TControllerDebuggeesBreakpointsResource;

begin
  Result:=CreateControllerDebuggeesBreakpointsResource(Self);
end;


Function TClouddebuggerAPI.CreateControllerDebuggeesBreakpointsResource(AOwner : TComponent) : TControllerDebuggeesBreakpointsResource;

begin
  Result:=TControllerDebuggeesBreakpointsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TClouddebuggerAPI.GetControllerDebuggeesInstance : TControllerDebuggeesResource;

begin
  if (FControllerDebuggeesInstance=Nil) then
    FControllerDebuggeesInstance:=CreateControllerDebuggeesResource;
  Result:=FControllerDebuggeesInstance;
end;

Function TClouddebuggerAPI.CreateControllerDebuggeesResource : TControllerDebuggeesResource;

begin
  Result:=CreateControllerDebuggeesResource(Self);
end;


Function TClouddebuggerAPI.CreateControllerDebuggeesResource(AOwner : TComponent) : TControllerDebuggeesResource;

begin
  Result:=TControllerDebuggeesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TClouddebuggerAPI.GetControllerInstance : TControllerResource;

begin
  if (FControllerInstance=Nil) then
    FControllerInstance:=CreateControllerResource;
  Result:=FControllerInstance;
end;

Function TClouddebuggerAPI.CreateControllerResource : TControllerResource;

begin
  Result:=CreateControllerResource(Self);
end;


Function TClouddebuggerAPI.CreateControllerResource(AOwner : TComponent) : TControllerResource;

begin
  Result:=TControllerResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TClouddebuggerAPI.GetDebuggerDebuggeesBreakpointsInstance : TDebuggerDebuggeesBreakpointsResource;

begin
  if (FDebuggerDebuggeesBreakpointsInstance=Nil) then
    FDebuggerDebuggeesBreakpointsInstance:=CreateDebuggerDebuggeesBreakpointsResource;
  Result:=FDebuggerDebuggeesBreakpointsInstance;
end;

Function TClouddebuggerAPI.CreateDebuggerDebuggeesBreakpointsResource : TDebuggerDebuggeesBreakpointsResource;

begin
  Result:=CreateDebuggerDebuggeesBreakpointsResource(Self);
end;


Function TClouddebuggerAPI.CreateDebuggerDebuggeesBreakpointsResource(AOwner : TComponent) : TDebuggerDebuggeesBreakpointsResource;

begin
  Result:=TDebuggerDebuggeesBreakpointsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TClouddebuggerAPI.GetDebuggerDebuggeesInstance : TDebuggerDebuggeesResource;

begin
  if (FDebuggerDebuggeesInstance=Nil) then
    FDebuggerDebuggeesInstance:=CreateDebuggerDebuggeesResource;
  Result:=FDebuggerDebuggeesInstance;
end;

Function TClouddebuggerAPI.CreateDebuggerDebuggeesResource : TDebuggerDebuggeesResource;

begin
  Result:=CreateDebuggerDebuggeesResource(Self);
end;


Function TClouddebuggerAPI.CreateDebuggerDebuggeesResource(AOwner : TComponent) : TDebuggerDebuggeesResource;

begin
  Result:=TDebuggerDebuggeesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TClouddebuggerAPI.GetDebuggerInstance : TDebuggerResource;

begin
  if (FDebuggerInstance=Nil) then
    FDebuggerInstance:=CreateDebuggerResource;
  Result:=FDebuggerInstance;
end;

Function TClouddebuggerAPI.CreateDebuggerResource : TDebuggerResource;

begin
  Result:=CreateDebuggerResource(Self);
end;


Function TClouddebuggerAPI.CreateDebuggerResource(AOwner : TComponent) : TDebuggerResource;

begin
  Result:=TDebuggerResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TClouddebuggerAPI.RegisterAPI;
end.
