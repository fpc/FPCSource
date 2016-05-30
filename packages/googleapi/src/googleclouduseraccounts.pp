unit googleclouduseraccounts;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAuditConfig = Class;
  TAuthorizedKeysView = Class;
  TBinding = Class;
  TCondition = Class;
  TGroup = Class;
  TGroupList = Class;
  TGroupsAddMemberRequest = Class;
  TGroupsRemoveMemberRequest = Class;
  TLinuxAccountViews = Class;
  TLinuxGetAuthorizedKeysViewResponse = Class;
  TLinuxGetLinuxAccountViewsResponse = Class;
  TLinuxGroupView = Class;
  TLinuxUserView = Class;
  TLogConfig = Class;
  TLogConfigCounterOptions = Class;
  TOperation = Class;
  TOperationList = Class;
  TPolicy = Class;
  TPublicKey = Class;
  TRule = Class;
  TTestPermissionsRequest = Class;
  TTestPermissionsResponse = Class;
  TUser = Class;
  TUserList = Class;
  TAuditConfigArray = Array of TAuditConfig;
  TAuthorizedKeysViewArray = Array of TAuthorizedKeysView;
  TBindingArray = Array of TBinding;
  TConditionArray = Array of TCondition;
  TGroupArray = Array of TGroup;
  TGroupListArray = Array of TGroupList;
  TGroupsAddMemberRequestArray = Array of TGroupsAddMemberRequest;
  TGroupsRemoveMemberRequestArray = Array of TGroupsRemoveMemberRequest;
  TLinuxAccountViewsArray = Array of TLinuxAccountViews;
  TLinuxGetAuthorizedKeysViewResponseArray = Array of TLinuxGetAuthorizedKeysViewResponse;
  TLinuxGetLinuxAccountViewsResponseArray = Array of TLinuxGetLinuxAccountViewsResponse;
  TLinuxGroupViewArray = Array of TLinuxGroupView;
  TLinuxUserViewArray = Array of TLinuxUserView;
  TLogConfigArray = Array of TLogConfig;
  TLogConfigCounterOptionsArray = Array of TLogConfigCounterOptions;
  TOperationArray = Array of TOperation;
  TOperationListArray = Array of TOperationList;
  TPolicyArray = Array of TPolicy;
  TPublicKeyArray = Array of TPublicKey;
  TRuleArray = Array of TRule;
  TTestPermissionsRequestArray = Array of TTestPermissionsRequest;
  TTestPermissionsResponseArray = Array of TTestPermissionsResponse;
  TUserArray = Array of TUser;
  TUserListArray = Array of TUserList;
  //Anonymous types, using auto-generated names
  TOperationTypeerrorTypeerrorsItem = Class;
  TOperationTypeerror = Class;
  TOperationTypewarningsItemTypedataItem = Class;
  TOperationTypewarningsItem = Class;
  TGroupListTypeitemsArray = Array of TGroup;
  TLinuxAccountViewsTypegroupViewsArray = Array of TLinuxGroupView;
  TLinuxAccountViewsTypeuserViewsArray = Array of TLinuxUserView;
  TOperationTypeerrorTypeerrorsArray = Array of TOperationTypeerrorTypeerrorsItem;
  TOperationTypewarningsItemTypedataArray = Array of TOperationTypewarningsItemTypedataItem;
  TOperationTypewarningsArray = Array of TOperationTypewarningsItem;
  TOperationListTypeitemsArray = Array of TOperation;
  TPolicyTypeauditConfigsArray = Array of TAuditConfig;
  TPolicyTypebindingsArray = Array of TBinding;
  TPolicyTyperulesArray = Array of TRule;
  TRuleTypeconditionsArray = Array of TCondition;
  TRuleTypelogConfigsArray = Array of TLogConfig;
  TUserTypepublicKeysArray = Array of TPublicKey;
  TUserListTypeitemsArray = Array of TUser;
  
  { --------------------------------------------------------------------
    TAuditConfig
    --------------------------------------------------------------------}
  
  TAuditConfig = Class(TGoogleBaseObject)
  Private
    FexemptedMembers : TStringArray;
    Fservice : String;
  Protected
    //Property setters
    Procedure SetexemptedMembers(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setservice(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property exemptedMembers : TStringArray Index 0 Read FexemptedMembers Write SetexemptedMembers;
    Property service : String Index 8 Read Fservice Write Setservice;
  end;
  TAuditConfigClass = Class of TAuditConfig;
  
  { --------------------------------------------------------------------
    TAuthorizedKeysView
    --------------------------------------------------------------------}
  
  TAuthorizedKeysView = Class(TGoogleBaseObject)
  Private
    Fkeys : TStringArray;
    Fsudoer : boolean;
  Protected
    //Property setters
    Procedure Setkeys(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setsudoer(AIndex : Integer; const AValue : boolean); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property keys : TStringArray Index 0 Read Fkeys Write Setkeys;
    Property sudoer : boolean Index 8 Read Fsudoer Write Setsudoer;
  end;
  TAuthorizedKeysViewClass = Class of TAuthorizedKeysView;
  
  { --------------------------------------------------------------------
    TBinding
    --------------------------------------------------------------------}
  
  TBinding = Class(TGoogleBaseObject)
  Private
    Fmembers : TStringArray;
    Frole : String;
  Protected
    //Property setters
    Procedure Setmembers(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setrole(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property members : TStringArray Index 0 Read Fmembers Write Setmembers;
    Property role : String Index 8 Read Frole Write Setrole;
  end;
  TBindingClass = Class of TBinding;
  
  { --------------------------------------------------------------------
    TCondition
    --------------------------------------------------------------------}
  
  TCondition = Class(TGoogleBaseObject)
  Private
    Fiam : String;
    Fop : String;
    Fsvc : String;
    Fsys : String;
    Fvalue : String;
    Fvalues : TStringArray;
  Protected
    //Property setters
    Procedure Setiam(AIndex : Integer; const AValue : String); virtual;
    Procedure Setop(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsvc(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsys(AIndex : Integer; const AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
    Procedure Setvalues(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property iam : String Index 0 Read Fiam Write Setiam;
    Property op : String Index 8 Read Fop Write Setop;
    Property svc : String Index 16 Read Fsvc Write Setsvc;
    Property sys : String Index 24 Read Fsys Write Setsys;
    Property value : String Index 32 Read Fvalue Write Setvalue;
    Property values : TStringArray Index 40 Read Fvalues Write Setvalues;
  end;
  TConditionClass = Class of TCondition;
  
  { --------------------------------------------------------------------
    TGroup
    --------------------------------------------------------------------}
  
  TGroup = Class(TGoogleBaseObject)
  Private
    FcreationTimestamp : String;
    Fdescription : String;
    Fid : String;
    Fkind : String;
    Fmembers : TStringArray;
    Fname : String;
    FselfLink : String;
  Protected
    //Property setters
    Procedure SetcreationTimestamp(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmembers(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property creationTimestamp : String Index 0 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property members : TStringArray Index 32 Read Fmembers Write Setmembers;
    Property name : String Index 40 Read Fname Write Setname;
    Property selfLink : String Index 48 Read FselfLink Write SetselfLink;
  end;
  TGroupClass = Class of TGroup;
  
  { --------------------------------------------------------------------
    TGroupList
    --------------------------------------------------------------------}
  
  TGroupList = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fitems : TGroupListTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FselfLink : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; const AValue : TGroupListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property items : TGroupListTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : String Index 32 Read FselfLink Write SetselfLink;
  end;
  TGroupListClass = Class of TGroupList;
  
  { --------------------------------------------------------------------
    TGroupsAddMemberRequest
    --------------------------------------------------------------------}
  
  TGroupsAddMemberRequest = Class(TGoogleBaseObject)
  Private
    Fusers : TStringArray;
  Protected
    //Property setters
    Procedure Setusers(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property users : TStringArray Index 0 Read Fusers Write Setusers;
  end;
  TGroupsAddMemberRequestClass = Class of TGroupsAddMemberRequest;
  
  { --------------------------------------------------------------------
    TGroupsRemoveMemberRequest
    --------------------------------------------------------------------}
  
  TGroupsRemoveMemberRequest = Class(TGoogleBaseObject)
  Private
    Fusers : TStringArray;
  Protected
    //Property setters
    Procedure Setusers(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property users : TStringArray Index 0 Read Fusers Write Setusers;
  end;
  TGroupsRemoveMemberRequestClass = Class of TGroupsRemoveMemberRequest;
  
  { --------------------------------------------------------------------
    TLinuxAccountViews
    --------------------------------------------------------------------}
  
  TLinuxAccountViews = Class(TGoogleBaseObject)
  Private
    FgroupViews : TLinuxAccountViewsTypegroupViewsArray;
    Fkind : String;
    FuserViews : TLinuxAccountViewsTypeuserViewsArray;
  Protected
    //Property setters
    Procedure SetgroupViews(AIndex : Integer; const AValue : TLinuxAccountViewsTypegroupViewsArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetuserViews(AIndex : Integer; const AValue : TLinuxAccountViewsTypeuserViewsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property groupViews : TLinuxAccountViewsTypegroupViewsArray Index 0 Read FgroupViews Write SetgroupViews;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property userViews : TLinuxAccountViewsTypeuserViewsArray Index 16 Read FuserViews Write SetuserViews;
  end;
  TLinuxAccountViewsClass = Class of TLinuxAccountViews;
  
  { --------------------------------------------------------------------
    TLinuxGetAuthorizedKeysViewResponse
    --------------------------------------------------------------------}
  
  TLinuxGetAuthorizedKeysViewResponse = Class(TGoogleBaseObject)
  Private
    Fresource : TAuthorizedKeysView;
  Protected
    //Property setters
    Procedure Setresource(AIndex : Integer; const AValue : TAuthorizedKeysView); virtual;
  Public
  Published
    Property resource : TAuthorizedKeysView Index 0 Read Fresource Write Setresource;
  end;
  TLinuxGetAuthorizedKeysViewResponseClass = Class of TLinuxGetAuthorizedKeysViewResponse;
  
  { --------------------------------------------------------------------
    TLinuxGetLinuxAccountViewsResponse
    --------------------------------------------------------------------}
  
  TLinuxGetLinuxAccountViewsResponse = Class(TGoogleBaseObject)
  Private
    Fresource : TLinuxAccountViews;
  Protected
    //Property setters
    Procedure Setresource(AIndex : Integer; const AValue : TLinuxAccountViews); virtual;
  Public
  Published
    Property resource : TLinuxAccountViews Index 0 Read Fresource Write Setresource;
  end;
  TLinuxGetLinuxAccountViewsResponseClass = Class of TLinuxGetLinuxAccountViewsResponse;
  
  { --------------------------------------------------------------------
    TLinuxGroupView
    --------------------------------------------------------------------}
  
  TLinuxGroupView = Class(TGoogleBaseObject)
  Private
    Fgid : integer;
    FgroupName : String;
    Fmembers : TStringArray;
  Protected
    //Property setters
    Procedure Setgid(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetgroupName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmembers(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property gid : integer Index 0 Read Fgid Write Setgid;
    Property groupName : String Index 8 Read FgroupName Write SetgroupName;
    Property members : TStringArray Index 16 Read Fmembers Write Setmembers;
  end;
  TLinuxGroupViewClass = Class of TLinuxGroupView;
  
  { --------------------------------------------------------------------
    TLinuxUserView
    --------------------------------------------------------------------}
  
  TLinuxUserView = Class(TGoogleBaseObject)
  Private
    Fgecos : String;
    Fgid : integer;
    FhomeDirectory : String;
    Fshell : String;
    Fuid : integer;
    Fusername : String;
  Protected
    //Property setters
    Procedure Setgecos(AIndex : Integer; const AValue : String); virtual;
    Procedure Setgid(AIndex : Integer; const AValue : integer); virtual;
    Procedure SethomeDirectory(AIndex : Integer; const AValue : String); virtual;
    Procedure Setshell(AIndex : Integer; const AValue : String); virtual;
    Procedure Setuid(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property gecos : String Index 0 Read Fgecos Write Setgecos;
    Property gid : integer Index 8 Read Fgid Write Setgid;
    Property homeDirectory : String Index 16 Read FhomeDirectory Write SethomeDirectory;
    Property shell : String Index 24 Read Fshell Write Setshell;
    Property uid : integer Index 32 Read Fuid Write Setuid;
    Property username : String Index 40 Read Fusername Write Setusername;
  end;
  TLinuxUserViewClass = Class of TLinuxUserView;
  
  { --------------------------------------------------------------------
    TLogConfig
    --------------------------------------------------------------------}
  
  TLogConfig = Class(TGoogleBaseObject)
  Private
    Fcounter : TLogConfigCounterOptions;
  Protected
    //Property setters
    Procedure Setcounter(AIndex : Integer; const AValue : TLogConfigCounterOptions); virtual;
  Public
  Published
    Property counter : TLogConfigCounterOptions Index 0 Read Fcounter Write Setcounter;
  end;
  TLogConfigClass = Class of TLogConfig;
  
  { --------------------------------------------------------------------
    TLogConfigCounterOptions
    --------------------------------------------------------------------}
  
  TLogConfigCounterOptions = Class(TGoogleBaseObject)
  Private
    Ffield : String;
    Fmetric : String;
  Protected
    //Property setters
    Procedure Setfield(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmetric(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property field : String Index 0 Read Ffield Write Setfield;
    Property metric : String Index 8 Read Fmetric Write Setmetric;
  end;
  TLogConfigCounterOptionsClass = Class of TLogConfigCounterOptions;
  
  { --------------------------------------------------------------------
    TOperationTypeerrorTypeerrorsItem
    --------------------------------------------------------------------}
  
  TOperationTypeerrorTypeerrorsItem = Class(TGoogleBaseObject)
  Private
    Fcode : String;
    Flocation : String;
    Fmessage : String;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlocation(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmessage(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property code : String Index 0 Read Fcode Write Setcode;
    Property location : String Index 8 Read Flocation Write Setlocation;
    Property message : String Index 16 Read Fmessage Write Setmessage;
  end;
  TOperationTypeerrorTypeerrorsItemClass = Class of TOperationTypeerrorTypeerrorsItem;
  
  { --------------------------------------------------------------------
    TOperationTypeerror
    --------------------------------------------------------------------}
  
  TOperationTypeerror = Class(TGoogleBaseObject)
  Private
    Ferrors : TOperationTypeerrorTypeerrorsArray;
  Protected
    //Property setters
    Procedure Seterrors(AIndex : Integer; const AValue : TOperationTypeerrorTypeerrorsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property errors : TOperationTypeerrorTypeerrorsArray Index 0 Read Ferrors Write Seterrors;
  end;
  TOperationTypeerrorClass = Class of TOperationTypeerror;
  
  { --------------------------------------------------------------------
    TOperationTypewarningsItemTypedataItem
    --------------------------------------------------------------------}
  
  TOperationTypewarningsItemTypedataItem = Class(TGoogleBaseObject)
  Private
    Fkey : String;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; const AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property key : String Index 0 Read Fkey Write Setkey;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TOperationTypewarningsItemTypedataItemClass = Class of TOperationTypewarningsItemTypedataItem;
  
  { --------------------------------------------------------------------
    TOperationTypewarningsItem
    --------------------------------------------------------------------}
  
  TOperationTypewarningsItem = Class(TGoogleBaseObject)
  Private
    Fcode : String;
    Fdata : TOperationTypewarningsItemTypedataArray;
    Fmessage : String;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdata(AIndex : Integer; const AValue : TOperationTypewarningsItemTypedataArray); virtual;
    Procedure Setmessage(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property code : String Index 0 Read Fcode Write Setcode;
    Property data : TOperationTypewarningsItemTypedataArray Index 8 Read Fdata Write Setdata;
    Property message : String Index 16 Read Fmessage Write Setmessage;
  end;
  TOperationTypewarningsItemClass = Class of TOperationTypewarningsItem;
  
  { --------------------------------------------------------------------
    TOperation
    --------------------------------------------------------------------}
  
  TOperation = Class(TGoogleBaseObject)
  Private
    FclientOperationId : String;
    FcreationTimestamp : String;
    Fdescription : String;
    FendTime : String;
    Ferror : TOperationTypeerror;
    FhttpErrorMessage : String;
    FhttpErrorStatusCode : integer;
    Fid : String;
    FinsertTime : String;
    Fkind : String;
    Fname : String;
    FoperationType : String;
    Fprogress : integer;
    Fregion : String;
    FselfLink : String;
    FstartTime : String;
    Fstatus : String;
    FstatusMessage : String;
    FtargetId : String;
    FtargetLink : String;
    Fuser : String;
    Fwarnings : TOperationTypewarningsArray;
    Fzone : String;
  Protected
    //Property setters
    Procedure SetclientOperationId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure SetendTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Seterror(AIndex : Integer; const AValue : TOperationTypeerror); virtual;
    Procedure SethttpErrorMessage(AIndex : Integer; const AValue : String); virtual;
    Procedure SethttpErrorStatusCode(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinsertTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoperationType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setprogress(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setregion(AIndex : Integer; const AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstartTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstatusMessage(AIndex : Integer; const AValue : String); virtual;
    Procedure SettargetId(AIndex : Integer; const AValue : String); virtual;
    Procedure SettargetLink(AIndex : Integer; const AValue : String); virtual;
    Procedure Setuser(AIndex : Integer; const AValue : String); virtual;
    Procedure Setwarnings(AIndex : Integer; const AValue : TOperationTypewarningsArray); virtual;
    Procedure Setzone(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property clientOperationId : String Index 0 Read FclientOperationId Write SetclientOperationId;
    Property creationTimestamp : String Index 8 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : String Index 16 Read Fdescription Write Setdescription;
    Property endTime : String Index 24 Read FendTime Write SetendTime;
    Property error : TOperationTypeerror Index 32 Read Ferror Write Seterror;
    Property httpErrorMessage : String Index 40 Read FhttpErrorMessage Write SethttpErrorMessage;
    Property httpErrorStatusCode : integer Index 48 Read FhttpErrorStatusCode Write SethttpErrorStatusCode;
    Property id : String Index 56 Read Fid Write Setid;
    Property insertTime : String Index 64 Read FinsertTime Write SetinsertTime;
    Property kind : String Index 72 Read Fkind Write Setkind;
    Property name : String Index 80 Read Fname Write Setname;
    Property operationType : String Index 88 Read FoperationType Write SetoperationType;
    Property progress : integer Index 96 Read Fprogress Write Setprogress;
    Property region : String Index 104 Read Fregion Write Setregion;
    Property selfLink : String Index 112 Read FselfLink Write SetselfLink;
    Property startTime : String Index 120 Read FstartTime Write SetstartTime;
    Property status : String Index 128 Read Fstatus Write Setstatus;
    Property statusMessage : String Index 136 Read FstatusMessage Write SetstatusMessage;
    Property targetId : String Index 144 Read FtargetId Write SettargetId;
    Property targetLink : String Index 152 Read FtargetLink Write SettargetLink;
    Property user : String Index 160 Read Fuser Write Setuser;
    Property warnings : TOperationTypewarningsArray Index 168 Read Fwarnings Write Setwarnings;
    Property zone : String Index 176 Read Fzone Write Setzone;
  end;
  TOperationClass = Class of TOperation;
  
  { --------------------------------------------------------------------
    TOperationList
    --------------------------------------------------------------------}
  
  TOperationList = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fitems : TOperationListTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FselfLink : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; const AValue : TOperationListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property items : TOperationListTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : String Index 32 Read FselfLink Write SetselfLink;
  end;
  TOperationListClass = Class of TOperationList;
  
  { --------------------------------------------------------------------
    TPolicy
    --------------------------------------------------------------------}
  
  TPolicy = Class(TGoogleBaseObject)
  Private
    FauditConfigs : TPolicyTypeauditConfigsArray;
    Fbindings : TPolicyTypebindingsArray;
    Fetag : String;
    FiamOwned : boolean;
    Frules : TPolicyTyperulesArray;
    Fversion : integer;
  Protected
    //Property setters
    Procedure SetauditConfigs(AIndex : Integer; const AValue : TPolicyTypeauditConfigsArray); virtual;
    Procedure Setbindings(AIndex : Integer; const AValue : TPolicyTypebindingsArray); virtual;
    Procedure Setetag(AIndex : Integer; const AValue : String); virtual;
    Procedure SetiamOwned(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setrules(AIndex : Integer; const AValue : TPolicyTyperulesArray); virtual;
    Procedure Setversion(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property auditConfigs : TPolicyTypeauditConfigsArray Index 0 Read FauditConfigs Write SetauditConfigs;
    Property bindings : TPolicyTypebindingsArray Index 8 Read Fbindings Write Setbindings;
    Property etag : String Index 16 Read Fetag Write Setetag;
    Property iamOwned : boolean Index 24 Read FiamOwned Write SetiamOwned;
    Property rules : TPolicyTyperulesArray Index 32 Read Frules Write Setrules;
    Property version : integer Index 40 Read Fversion Write Setversion;
  end;
  TPolicyClass = Class of TPolicy;
  
  { --------------------------------------------------------------------
    TPublicKey
    --------------------------------------------------------------------}
  
  TPublicKey = Class(TGoogleBaseObject)
  Private
    FcreationTimestamp : String;
    Fdescription : String;
    FexpirationTimestamp : String;
    Ffingerprint : String;
    Fkey : String;
  Protected
    //Property setters
    Procedure SetcreationTimestamp(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure SetexpirationTimestamp(AIndex : Integer; const AValue : String); virtual;
    Procedure Setfingerprint(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkey(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property creationTimestamp : String Index 0 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property expirationTimestamp : String Index 16 Read FexpirationTimestamp Write SetexpirationTimestamp;
    Property fingerprint : String Index 24 Read Ffingerprint Write Setfingerprint;
    Property key : String Index 32 Read Fkey Write Setkey;
  end;
  TPublicKeyClass = Class of TPublicKey;
  
  { --------------------------------------------------------------------
    TRule
    --------------------------------------------------------------------}
  
  TRule = Class(TGoogleBaseObject)
  Private
    Faction : String;
    Fconditions : TRuleTypeconditionsArray;
    Fdescription : String;
    Fins : TStringArray;
    FlogConfigs : TRuleTypelogConfigsArray;
    FnotIns : TStringArray;
    Fpermissions : TStringArray;
  Protected
    //Property setters
    Procedure Setaction(AIndex : Integer; const AValue : String); virtual;
    Procedure Setconditions(AIndex : Integer; const AValue : TRuleTypeconditionsArray); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure Setins(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetlogConfigs(AIndex : Integer; const AValue : TRuleTypelogConfigsArray); virtual;
    Procedure SetnotIns(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setpermissions(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property action : String Index 0 Read Faction Write Setaction;
    Property conditions : TRuleTypeconditionsArray Index 8 Read Fconditions Write Setconditions;
    Property description : String Index 16 Read Fdescription Write Setdescription;
    Property ins : TStringArray Index 24 Read Fins Write Setins;
    Property logConfigs : TRuleTypelogConfigsArray Index 32 Read FlogConfigs Write SetlogConfigs;
    Property notIns : TStringArray Index 40 Read FnotIns Write SetnotIns;
    Property permissions : TStringArray Index 48 Read Fpermissions Write Setpermissions;
  end;
  TRuleClass = Class of TRule;
  
  { --------------------------------------------------------------------
    TTestPermissionsRequest
    --------------------------------------------------------------------}
  
  TTestPermissionsRequest = Class(TGoogleBaseObject)
  Private
    Fpermissions : TStringArray;
  Protected
    //Property setters
    Procedure Setpermissions(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property permissions : TStringArray Index 0 Read Fpermissions Write Setpermissions;
  end;
  TTestPermissionsRequestClass = Class of TTestPermissionsRequest;
  
  { --------------------------------------------------------------------
    TTestPermissionsResponse
    --------------------------------------------------------------------}
  
  TTestPermissionsResponse = Class(TGoogleBaseObject)
  Private
    Fpermissions : TStringArray;
  Protected
    //Property setters
    Procedure Setpermissions(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property permissions : TStringArray Index 0 Read Fpermissions Write Setpermissions;
  end;
  TTestPermissionsResponseClass = Class of TTestPermissionsResponse;
  
  { --------------------------------------------------------------------
    TUser
    --------------------------------------------------------------------}
  
  TUser = Class(TGoogleBaseObject)
  Private
    FcreationTimestamp : String;
    Fdescription : String;
    Fgroups : TStringArray;
    Fid : String;
    Fkind : String;
    Fname : String;
    Fowner : String;
    FpublicKeys : TUserTypepublicKeysArray;
    FselfLink : String;
  Protected
    //Property setters
    Procedure SetcreationTimestamp(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure Setgroups(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setowner(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpublicKeys(AIndex : Integer; const AValue : TUserTypepublicKeysArray); virtual;
    Procedure SetselfLink(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property creationTimestamp : String Index 0 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property groups : TStringArray Index 16 Read Fgroups Write Setgroups;
    Property id : String Index 24 Read Fid Write Setid;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property name : String Index 40 Read Fname Write Setname;
    Property owner : String Index 48 Read Fowner Write Setowner;
    Property publicKeys : TUserTypepublicKeysArray Index 56 Read FpublicKeys Write SetpublicKeys;
    Property selfLink : String Index 64 Read FselfLink Write SetselfLink;
  end;
  TUserClass = Class of TUser;
  
  { --------------------------------------------------------------------
    TUserList
    --------------------------------------------------------------------}
  
  TUserList = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fitems : TUserListTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FselfLink : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; const AValue : TUserListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property items : TUserListTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : String Index 32 Read FselfLink Write SetselfLink;
  end;
  TUserListClass = Class of TUserList;
  
  { --------------------------------------------------------------------
    TGlobalAccountsOperationsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TGlobalAccountsOperationsResource, method List
  
  TGlobalAccountsOperationsListOptions = Record
    filter : String;
    maxResults : integer;
    orderBy : String;
    pageToken : String;
  end;
  
  TGlobalAccountsOperationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(operation: string; project: string);
    Function Get(operation: string; project: string) : TOperation;
    Function List(project: string; AQuery : string  = '') : TOperationList;
    Function List(project: string; AQuery : TGlobalAccountsOperationslistOptions) : TOperationList;
  end;
  
  
  { --------------------------------------------------------------------
    TGroupsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TGroupsResource, method List
  
  TGroupsListOptions = Record
    filter : String;
    maxResults : integer;
    orderBy : String;
    pageToken : String;
  end;
  
  TGroupsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function AddMember(groupName: string; project: string; aGroupsAddMemberRequest : TGroupsAddMemberRequest) : TOperation;
    Function Delete(groupName: string; project: string) : TOperation;
    Function Get(groupName: string; project: string) : TGroup;
    Function GetIamPolicy(project: string; resource: string) : TPolicy;
    Function Insert(project: string; aGroup : TGroup) : TOperation;
    Function List(project: string; AQuery : string  = '') : TGroupList;
    Function List(project: string; AQuery : TGroupslistOptions) : TGroupList;
    Function RemoveMember(groupName: string; project: string; aGroupsRemoveMemberRequest : TGroupsRemoveMemberRequest) : TOperation;
    Function SetIamPolicy(project: string; resource: string; aPolicy : TPolicy) : TPolicy;
    Function TestIamPermissions(project: string; resource: string; aTestPermissionsRequest : TTestPermissionsRequest) : TTestPermissionsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TLinuxResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TLinuxResource, method GetAuthorizedKeysView
  
  TLinuxGetAuthorizedKeysViewOptions = Record
    instance : String;
    login : boolean;
  end;
  
  
  //Optional query Options for TLinuxResource, method GetLinuxAccountViews
  
  TLinuxGetLinuxAccountViewsOptions = Record
    filter : String;
    instance : String;
    maxResults : integer;
    orderBy : String;
    pageToken : String;
  end;
  
  TLinuxResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function GetAuthorizedKeysView(project: string; user: string; zone: string; AQuery : string  = '') : TLinuxGetAuthorizedKeysViewResponse;
    Function GetAuthorizedKeysView(project: string; user: string; zone: string; AQuery : TLinuxgetAuthorizedKeysViewOptions) : TLinuxGetAuthorizedKeysViewResponse;
    Function GetLinuxAccountViews(project: string; zone: string; AQuery : string  = '') : TLinuxGetLinuxAccountViewsResponse;
    Function GetLinuxAccountViews(project: string; zone: string; AQuery : TLinuxgetLinuxAccountViewsOptions) : TLinuxGetLinuxAccountViewsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TUsersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TUsersResource, method List
  
  TUsersListOptions = Record
    filter : String;
    maxResults : integer;
    orderBy : String;
    pageToken : String;
  end;
  
  
  //Optional query Options for TUsersResource, method RemovePublicKey
  
  TUsersRemovePublicKeyOptions = Record
    fingerprint : String;
  end;
  
  TUsersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function AddPublicKey(project: string; user: string; aPublicKey : TPublicKey) : TOperation;
    Function Delete(project: string; user: string) : TOperation;
    Function Get(project: string; user: string) : TUser;
    Function GetIamPolicy(project: string; resource: string) : TPolicy;
    Function Insert(project: string; aUser : TUser) : TOperation;
    Function List(project: string; AQuery : string  = '') : TUserList;
    Function List(project: string; AQuery : TUserslistOptions) : TUserList;
    Function RemovePublicKey(project: string; user: string; AQuery : string  = '') : TOperation;
    Function RemovePublicKey(project: string; user: string; AQuery : TUsersremovePublicKeyOptions) : TOperation;
    Function SetIamPolicy(project: string; resource: string; aPolicy : TPolicy) : TPolicy;
    Function TestIamPermissions(project: string; resource: string; aTestPermissionsRequest : TTestPermissionsRequest) : TTestPermissionsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TClouduseraccountsAPI
    --------------------------------------------------------------------}
  
  TClouduseraccountsAPI = Class(TGoogleAPI)
  Private
    FGlobalAccountsOperationsInstance : TGlobalAccountsOperationsResource;
    FGroupsInstance : TGroupsResource;
    FLinuxInstance : TLinuxResource;
    FUsersInstance : TUsersResource;
    Function GetGlobalAccountsOperationsInstance : TGlobalAccountsOperationsResource;virtual;
    Function GetGroupsInstance : TGroupsResource;virtual;
    Function GetLinuxInstance : TLinuxResource;virtual;
    Function GetUsersInstance : TUsersResource;virtual;
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
    Function CreateGlobalAccountsOperationsResource(AOwner : TComponent) : TGlobalAccountsOperationsResource;virtual;overload;
    Function CreateGlobalAccountsOperationsResource : TGlobalAccountsOperationsResource;virtual;overload;
    Function CreateGroupsResource(AOwner : TComponent) : TGroupsResource;virtual;overload;
    Function CreateGroupsResource : TGroupsResource;virtual;overload;
    Function CreateLinuxResource(AOwner : TComponent) : TLinuxResource;virtual;overload;
    Function CreateLinuxResource : TLinuxResource;virtual;overload;
    Function CreateUsersResource(AOwner : TComponent) : TUsersResource;virtual;overload;
    Function CreateUsersResource : TUsersResource;virtual;overload;
    //Add default on-demand instances for resources
    Property GlobalAccountsOperationsResource : TGlobalAccountsOperationsResource Read GetGlobalAccountsOperationsInstance;
    Property GroupsResource : TGroupsResource Read GetGroupsInstance;
    Property LinuxResource : TLinuxResource Read GetLinuxInstance;
    Property UsersResource : TUsersResource Read GetUsersInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAuditConfig
  --------------------------------------------------------------------}


Procedure TAuditConfig.SetexemptedMembers(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FexemptedMembers=AValue) then exit;
  FexemptedMembers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAuditConfig.Setservice(AIndex : Integer; const AValue : String); 

begin
  If (Fservice=AValue) then exit;
  Fservice:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAuditConfig.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'exemptedmembers' : SetLength(FexemptedMembers,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAuthorizedKeysView
  --------------------------------------------------------------------}


Procedure TAuthorizedKeysView.Setkeys(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fkeys=AValue) then exit;
  Fkeys:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAuthorizedKeysView.Setsudoer(AIndex : Integer; const AValue : boolean); 

begin
  If (Fsudoer=AValue) then exit;
  Fsudoer:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAuthorizedKeysView.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'keys' : SetLength(Fkeys,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBinding
  --------------------------------------------------------------------}


Procedure TBinding.Setmembers(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fmembers=AValue) then exit;
  Fmembers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBinding.Setrole(AIndex : Integer; const AValue : String); 

begin
  If (Frole=AValue) then exit;
  Frole:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBinding.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'members' : SetLength(Fmembers,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCondition
  --------------------------------------------------------------------}


Procedure TCondition.Setiam(AIndex : Integer; const AValue : String); 

begin
  If (Fiam=AValue) then exit;
  Fiam:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCondition.Setop(AIndex : Integer; const AValue : String); 

begin
  If (Fop=AValue) then exit;
  Fop:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCondition.Setsvc(AIndex : Integer; const AValue : String); 

begin
  If (Fsvc=AValue) then exit;
  Fsvc:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCondition.Setsys(AIndex : Integer; const AValue : String); 

begin
  If (Fsys=AValue) then exit;
  Fsys:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCondition.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCondition.Setvalues(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fvalues=AValue) then exit;
  Fvalues:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCondition.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'values' : SetLength(Fvalues,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGroup
  --------------------------------------------------------------------}


Procedure TGroup.SetcreationTimestamp(AIndex : Integer; const AValue : String); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.Setmembers(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fmembers=AValue) then exit;
  Fmembers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.SetselfLink(AIndex : Integer; const AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGroup.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'members' : SetLength(Fmembers,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGroupList
  --------------------------------------------------------------------}


Procedure TGroupList.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupList.Setitems(AIndex : Integer; const AValue : TGroupListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupList.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupList.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupList.SetselfLink(AIndex : Integer; const AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGroupList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGroupsAddMemberRequest
  --------------------------------------------------------------------}


Procedure TGroupsAddMemberRequest.Setusers(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fusers=AValue) then exit;
  Fusers:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGroupsAddMemberRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'users' : SetLength(Fusers,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGroupsRemoveMemberRequest
  --------------------------------------------------------------------}


Procedure TGroupsRemoveMemberRequest.Setusers(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fusers=AValue) then exit;
  Fusers:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGroupsRemoveMemberRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'users' : SetLength(Fusers,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TLinuxAccountViews
  --------------------------------------------------------------------}


Procedure TLinuxAccountViews.SetgroupViews(AIndex : Integer; const AValue : TLinuxAccountViewsTypegroupViewsArray); 

begin
  If (FgroupViews=AValue) then exit;
  FgroupViews:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinuxAccountViews.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinuxAccountViews.SetuserViews(AIndex : Integer; const AValue : TLinuxAccountViewsTypeuserViewsArray); 

begin
  If (FuserViews=AValue) then exit;
  FuserViews:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TLinuxAccountViews.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'groupviews' : SetLength(FgroupViews,ALength);
  'userviews' : SetLength(FuserViews,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TLinuxGetAuthorizedKeysViewResponse
  --------------------------------------------------------------------}


Procedure TLinuxGetAuthorizedKeysViewResponse.Setresource(AIndex : Integer; const AValue : TAuthorizedKeysView); 

begin
  If (Fresource=AValue) then exit;
  Fresource:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLinuxGetLinuxAccountViewsResponse
  --------------------------------------------------------------------}


Procedure TLinuxGetLinuxAccountViewsResponse.Setresource(AIndex : Integer; const AValue : TLinuxAccountViews); 

begin
  If (Fresource=AValue) then exit;
  Fresource:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLinuxGroupView
  --------------------------------------------------------------------}


Procedure TLinuxGroupView.Setgid(AIndex : Integer; const AValue : integer); 

begin
  If (Fgid=AValue) then exit;
  Fgid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinuxGroupView.SetgroupName(AIndex : Integer; const AValue : String); 

begin
  If (FgroupName=AValue) then exit;
  FgroupName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinuxGroupView.Setmembers(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fmembers=AValue) then exit;
  Fmembers:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TLinuxGroupView.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'members' : SetLength(Fmembers,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TLinuxUserView
  --------------------------------------------------------------------}


Procedure TLinuxUserView.Setgecos(AIndex : Integer; const AValue : String); 

begin
  If (Fgecos=AValue) then exit;
  Fgecos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinuxUserView.Setgid(AIndex : Integer; const AValue : integer); 

begin
  If (Fgid=AValue) then exit;
  Fgid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinuxUserView.SethomeDirectory(AIndex : Integer; const AValue : String); 

begin
  If (FhomeDirectory=AValue) then exit;
  FhomeDirectory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinuxUserView.Setshell(AIndex : Integer; const AValue : String); 

begin
  If (Fshell=AValue) then exit;
  Fshell:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinuxUserView.Setuid(AIndex : Integer; const AValue : integer); 

begin
  If (Fuid=AValue) then exit;
  Fuid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinuxUserView.Setusername(AIndex : Integer; const AValue : String); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLogConfig
  --------------------------------------------------------------------}


Procedure TLogConfig.Setcounter(AIndex : Integer; const AValue : TLogConfigCounterOptions); 

begin
  If (Fcounter=AValue) then exit;
  Fcounter:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLogConfigCounterOptions
  --------------------------------------------------------------------}


Procedure TLogConfigCounterOptions.Setfield(AIndex : Integer; const AValue : String); 

begin
  If (Ffield=AValue) then exit;
  Ffield:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogConfigCounterOptions.Setmetric(AIndex : Integer; const AValue : String); 

begin
  If (Fmetric=AValue) then exit;
  Fmetric:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationTypeerrorTypeerrorsItem
  --------------------------------------------------------------------}


Procedure TOperationTypeerrorTypeerrorsItem.Setcode(AIndex : Integer; const AValue : String); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationTypeerrorTypeerrorsItem.Setlocation(AIndex : Integer; const AValue : String); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationTypeerrorTypeerrorsItem.Setmessage(AIndex : Integer; const AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationTypeerror
  --------------------------------------------------------------------}


Procedure TOperationTypeerror.Seterrors(AIndex : Integer; const AValue : TOperationTypeerrorTypeerrorsArray); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TOperationTypeerror.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'errors' : SetLength(Ferrors,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TOperationTypewarningsItemTypedataItem
  --------------------------------------------------------------------}


Procedure TOperationTypewarningsItemTypedataItem.Setkey(AIndex : Integer; const AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationTypewarningsItemTypedataItem.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationTypewarningsItem
  --------------------------------------------------------------------}


Procedure TOperationTypewarningsItem.Setcode(AIndex : Integer; const AValue : String); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationTypewarningsItem.Setdata(AIndex : Integer; const AValue : TOperationTypewarningsItemTypedataArray); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationTypewarningsItem.Setmessage(AIndex : Integer; const AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TOperationTypewarningsItem.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'data' : SetLength(Fdata,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TOperation
  --------------------------------------------------------------------}


Procedure TOperation.SetclientOperationId(AIndex : Integer; const AValue : String); 

begin
  If (FclientOperationId=AValue) then exit;
  FclientOperationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetcreationTimestamp(AIndex : Integer; const AValue : String); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetendTime(AIndex : Integer; const AValue : String); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Seterror(AIndex : Integer; const AValue : TOperationTypeerror); 

begin
  If (Ferror=AValue) then exit;
  Ferror:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SethttpErrorMessage(AIndex : Integer; const AValue : String); 

begin
  If (FhttpErrorMessage=AValue) then exit;
  FhttpErrorMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SethttpErrorStatusCode(AIndex : Integer; const AValue : integer); 

begin
  If (FhttpErrorStatusCode=AValue) then exit;
  FhttpErrorStatusCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetinsertTime(AIndex : Integer; const AValue : String); 

begin
  If (FinsertTime=AValue) then exit;
  FinsertTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetoperationType(AIndex : Integer; const AValue : String); 

begin
  If (FoperationType=AValue) then exit;
  FoperationType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setprogress(AIndex : Integer; const AValue : integer); 

begin
  If (Fprogress=AValue) then exit;
  Fprogress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setregion(AIndex : Integer; const AValue : String); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetselfLink(AIndex : Integer; const AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetstartTime(AIndex : Integer; const AValue : String); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setstatus(AIndex : Integer; const AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetstatusMessage(AIndex : Integer; const AValue : String); 

begin
  If (FstatusMessage=AValue) then exit;
  FstatusMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SettargetId(AIndex : Integer; const AValue : String); 

begin
  If (FtargetId=AValue) then exit;
  FtargetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SettargetLink(AIndex : Integer; const AValue : String); 

begin
  If (FtargetLink=AValue) then exit;
  FtargetLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setuser(AIndex : Integer; const AValue : String); 

begin
  If (Fuser=AValue) then exit;
  Fuser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setwarnings(AIndex : Integer; const AValue : TOperationTypewarningsArray); 

begin
  If (Fwarnings=AValue) then exit;
  Fwarnings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setzone(AIndex : Integer; const AValue : String); 

begin
  If (Fzone=AValue) then exit;
  Fzone:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TOperation.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'warnings' : SetLength(Fwarnings,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TOperationList
  --------------------------------------------------------------------}


Procedure TOperationList.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.Setitems(AIndex : Integer; const AValue : TOperationListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.SetselfLink(AIndex : Integer; const AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TOperationList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPolicy
  --------------------------------------------------------------------}


Procedure TPolicy.SetauditConfigs(AIndex : Integer; const AValue : TPolicyTypeauditConfigsArray); 

begin
  If (FauditConfigs=AValue) then exit;
  FauditConfigs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPolicy.Setbindings(AIndex : Integer; const AValue : TPolicyTypebindingsArray); 

begin
  If (Fbindings=AValue) then exit;
  Fbindings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPolicy.Setetag(AIndex : Integer; const AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPolicy.SetiamOwned(AIndex : Integer; const AValue : boolean); 

begin
  If (FiamOwned=AValue) then exit;
  FiamOwned:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPolicy.Setrules(AIndex : Integer; const AValue : TPolicyTyperulesArray); 

begin
  If (Frules=AValue) then exit;
  Frules:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPolicy.Setversion(AIndex : Integer; const AValue : integer); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPolicy.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'auditconfigs' : SetLength(FauditConfigs,ALength);
  'bindings' : SetLength(Fbindings,ALength);
  'rules' : SetLength(Frules,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPublicKey
  --------------------------------------------------------------------}


Procedure TPublicKey.SetcreationTimestamp(AIndex : Integer; const AValue : String); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublicKey.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublicKey.SetexpirationTimestamp(AIndex : Integer; const AValue : String); 

begin
  If (FexpirationTimestamp=AValue) then exit;
  FexpirationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublicKey.Setfingerprint(AIndex : Integer; const AValue : String); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublicKey.Setkey(AIndex : Integer; const AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRule
  --------------------------------------------------------------------}


Procedure TRule.Setaction(AIndex : Integer; const AValue : String); 

begin
  If (Faction=AValue) then exit;
  Faction:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.Setconditions(AIndex : Integer; const AValue : TRuleTypeconditionsArray); 

begin
  If (Fconditions=AValue) then exit;
  Fconditions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.Setins(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fins=AValue) then exit;
  Fins:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.SetlogConfigs(AIndex : Integer; const AValue : TRuleTypelogConfigsArray); 

begin
  If (FlogConfigs=AValue) then exit;
  FlogConfigs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.SetnotIns(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FnotIns=AValue) then exit;
  FnotIns:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.Setpermissions(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fpermissions=AValue) then exit;
  Fpermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TRule.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'conditions' : SetLength(Fconditions,ALength);
  'ins' : SetLength(Fins,ALength);
  'logconfigs' : SetLength(FlogConfigs,ALength);
  'notins' : SetLength(FnotIns,ALength);
  'permissions' : SetLength(Fpermissions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTestPermissionsRequest
  --------------------------------------------------------------------}


Procedure TTestPermissionsRequest.Setpermissions(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fpermissions=AValue) then exit;
  Fpermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTestPermissionsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'permissions' : SetLength(Fpermissions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTestPermissionsResponse
  --------------------------------------------------------------------}


Procedure TTestPermissionsResponse.Setpermissions(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fpermissions=AValue) then exit;
  Fpermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTestPermissionsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'permissions' : SetLength(Fpermissions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TUser
  --------------------------------------------------------------------}


Procedure TUser.SetcreationTimestamp(AIndex : Integer; const AValue : String); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setgroups(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fgroups=AValue) then exit;
  Fgroups:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setowner(AIndex : Integer; const AValue : String); 

begin
  If (Fowner=AValue) then exit;
  Fowner:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.SetpublicKeys(AIndex : Integer; const AValue : TUserTypepublicKeysArray); 

begin
  If (FpublicKeys=AValue) then exit;
  FpublicKeys:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.SetselfLink(AIndex : Integer; const AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TUser.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'groups' : SetLength(Fgroups,ALength);
  'publickeys' : SetLength(FpublicKeys,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TUserList
  --------------------------------------------------------------------}


Procedure TUserList.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserList.Setitems(AIndex : Integer; const AValue : TUserListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserList.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserList.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserList.SetselfLink(AIndex : Integer; const AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TUserList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGlobalAccountsOperationsResource
  --------------------------------------------------------------------}


Class Function TGlobalAccountsOperationsResource.ResourceName : String;

begin
  Result:='globalAccountsOperations';
end;

Class Function TGlobalAccountsOperationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TclouduseraccountsAPI;
end;

Procedure TGlobalAccountsOperationsResource.Delete(operation: string; project: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/global/operations/{operation}';
  _Methodid   = 'clouduseraccounts.globalAccountsOperations.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['operation',operation,'project',project]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TGlobalAccountsOperationsResource.Get(operation: string; project: string) : TOperation;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/operations/{operation}';
  _Methodid   = 'clouduseraccounts.globalAccountsOperations.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['operation',operation,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TGlobalAccountsOperationsResource.List(project: string; AQuery : string = '') : TOperationList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/operations';
  _Methodid   = 'clouduseraccounts.globalAccountsOperations.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TOperationList) as TOperationList;
end;


Function TGlobalAccountsOperationsResource.List(project: string; AQuery : TGlobalAccountsOperationslistOptions) : TOperationList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'orderBy',AQuery.orderBy);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;



{ --------------------------------------------------------------------
  TGroupsResource
  --------------------------------------------------------------------}


Class Function TGroupsResource.ResourceName : String;

begin
  Result:='groups';
end;

Class Function TGroupsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TclouduseraccountsAPI;
end;

Function TGroupsResource.AddMember(groupName: string; project: string; aGroupsAddMemberRequest : TGroupsAddMemberRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/groups/{groupName}/addMember';
  _Methodid   = 'clouduseraccounts.groups.addMember';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['groupName',groupName,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aGroupsAddMemberRequest,TOperation) as TOperation;
end;

Function TGroupsResource.Delete(groupName: string; project: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/global/groups/{groupName}';
  _Methodid   = 'clouduseraccounts.groups.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['groupName',groupName,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TGroupsResource.Get(groupName: string; project: string) : TGroup;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/groups/{groupName}';
  _Methodid   = 'clouduseraccounts.groups.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['groupName',groupName,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TGroup) as TGroup;
end;

Function TGroupsResource.GetIamPolicy(project: string; resource: string) : TPolicy;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/groups/{resource}/getIamPolicy';
  _Methodid   = 'clouduseraccounts.groups.getIamPolicy';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'resource',resource]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPolicy) as TPolicy;
end;

Function TGroupsResource.Insert(project: string; aGroup : TGroup) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/groups';
  _Methodid   = 'clouduseraccounts.groups.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aGroup,TOperation) as TOperation;
end;

Function TGroupsResource.List(project: string; AQuery : string = '') : TGroupList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/groups';
  _Methodid   = 'clouduseraccounts.groups.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TGroupList) as TGroupList;
end;


Function TGroupsResource.List(project: string; AQuery : TGroupslistOptions) : TGroupList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'orderBy',AQuery.orderBy);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;

Function TGroupsResource.RemoveMember(groupName: string; project: string; aGroupsRemoveMemberRequest : TGroupsRemoveMemberRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/groups/{groupName}/removeMember';
  _Methodid   = 'clouduseraccounts.groups.removeMember';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['groupName',groupName,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aGroupsRemoveMemberRequest,TOperation) as TOperation;
end;

Function TGroupsResource.SetIamPolicy(project: string; resource: string; aPolicy : TPolicy) : TPolicy;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/groups/{resource}/setIamPolicy';
  _Methodid   = 'clouduseraccounts.groups.setIamPolicy';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'resource',resource]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aPolicy,TPolicy) as TPolicy;
end;

Function TGroupsResource.TestIamPermissions(project: string; resource: string; aTestPermissionsRequest : TTestPermissionsRequest) : TTestPermissionsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/groups/{resource}/testIamPermissions';
  _Methodid   = 'clouduseraccounts.groups.testIamPermissions';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'resource',resource]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTestPermissionsRequest,TTestPermissionsResponse) as TTestPermissionsResponse;
end;



{ --------------------------------------------------------------------
  TLinuxResource
  --------------------------------------------------------------------}


Class Function TLinuxResource.ResourceName : String;

begin
  Result:='linux';
end;

Class Function TLinuxResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TclouduseraccountsAPI;
end;

Function TLinuxResource.GetAuthorizedKeysView(project: string; user: string; zone: string; AQuery : string = '') : TLinuxGetAuthorizedKeysViewResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/authorizedKeysView/{user}';
  _Methodid   = 'clouduseraccounts.linux.getAuthorizedKeysView';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'user',user,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TLinuxGetAuthorizedKeysViewResponse) as TLinuxGetAuthorizedKeysViewResponse;
end;


Function TLinuxResource.GetAuthorizedKeysView(project: string; user: string; zone: string; AQuery : TLinuxgetAuthorizedKeysViewOptions) : TLinuxGetAuthorizedKeysViewResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'instance',AQuery.instance);
  AddToQuery(_Q,'login',AQuery.login);
  Result:=GetAuthorizedKeysView(project,user,zone,_Q);
end;

Function TLinuxResource.GetLinuxAccountViews(project: string; zone: string; AQuery : string = '') : TLinuxGetLinuxAccountViewsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/linuxAccountViews';
  _Methodid   = 'clouduseraccounts.linux.getLinuxAccountViews';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TLinuxGetLinuxAccountViewsResponse) as TLinuxGetLinuxAccountViewsResponse;
end;


Function TLinuxResource.GetLinuxAccountViews(project: string; zone: string; AQuery : TLinuxgetLinuxAccountViewsOptions) : TLinuxGetLinuxAccountViewsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'instance',AQuery.instance);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'orderBy',AQuery.orderBy);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=GetLinuxAccountViews(project,zone,_Q);
end;



{ --------------------------------------------------------------------
  TUsersResource
  --------------------------------------------------------------------}


Class Function TUsersResource.ResourceName : String;

begin
  Result:='users';
end;

Class Function TUsersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TclouduseraccountsAPI;
end;

Function TUsersResource.AddPublicKey(project: string; user: string; aPublicKey : TPublicKey) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/users/{user}/addPublicKey';
  _Methodid   = 'clouduseraccounts.users.addPublicKey';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'user',user]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aPublicKey,TOperation) as TOperation;
end;

Function TUsersResource.Delete(project: string; user: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/global/users/{user}';
  _Methodid   = 'clouduseraccounts.users.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'user',user]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TUsersResource.Get(project: string; user: string) : TUser;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/users/{user}';
  _Methodid   = 'clouduseraccounts.users.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'user',user]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TUser) as TUser;
end;

Function TUsersResource.GetIamPolicy(project: string; resource: string) : TPolicy;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/users/{resource}/getIamPolicy';
  _Methodid   = 'clouduseraccounts.users.getIamPolicy';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'resource',resource]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPolicy) as TPolicy;
end;

Function TUsersResource.Insert(project: string; aUser : TUser) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/users';
  _Methodid   = 'clouduseraccounts.users.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aUser,TOperation) as TOperation;
end;

Function TUsersResource.List(project: string; AQuery : string = '') : TUserList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/users';
  _Methodid   = 'clouduseraccounts.users.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TUserList) as TUserList;
end;


Function TUsersResource.List(project: string; AQuery : TUserslistOptions) : TUserList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'orderBy',AQuery.orderBy);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;

Function TUsersResource.RemovePublicKey(project: string; user: string; AQuery : string = '') : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/users/{user}/removePublicKey';
  _Methodid   = 'clouduseraccounts.users.removePublicKey';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'user',user]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TOperation) as TOperation;
end;


Function TUsersResource.RemovePublicKey(project: string; user: string; AQuery : TUsersremovePublicKeyOptions) : TOperation;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'fingerprint',AQuery.fingerprint);
  Result:=RemovePublicKey(project,user,_Q);
end;

Function TUsersResource.SetIamPolicy(project: string; resource: string; aPolicy : TPolicy) : TPolicy;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/users/{resource}/setIamPolicy';
  _Methodid   = 'clouduseraccounts.users.setIamPolicy';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'resource',resource]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aPolicy,TPolicy) as TPolicy;
end;

Function TUsersResource.TestIamPermissions(project: string; resource: string; aTestPermissionsRequest : TTestPermissionsRequest) : TTestPermissionsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/users/{resource}/testIamPermissions';
  _Methodid   = 'clouduseraccounts.users.testIamPermissions';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'resource',resource]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTestPermissionsRequest,TTestPermissionsResponse) as TTestPermissionsResponse;
end;



{ --------------------------------------------------------------------
  TClouduseraccountsAPI
  --------------------------------------------------------------------}

Class Function TClouduseraccountsAPI.APIName : String;

begin
  Result:='clouduseraccounts';
end;

Class Function TClouduseraccountsAPI.APIVersion : String;

begin
  Result:='vm_alpha';
end;

Class Function TClouduseraccountsAPI.APIRevision : String;

begin
  Result:='20160316';
end;

Class Function TClouduseraccountsAPI.APIID : String;

begin
  Result:='clouduseraccounts:vm_alpha';
end;

Class Function TClouduseraccountsAPI.APITitle : String;

begin
  Result:='Cloud User Accounts API';
end;

Class Function TClouduseraccountsAPI.APIDescription : String;

begin
  Result:='Creates and manages users and groups for accessing Google Compute Engine virtual machines.';
end;

Class Function TClouduseraccountsAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TClouduseraccountsAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TClouduseraccountsAPI.APIIcon16 : String;

begin
  Result:='https://www.google.com/images/icons/product/compute_engine-16.png';
end;

Class Function TClouduseraccountsAPI.APIIcon32 : String;

begin
  Result:='https://www.google.com/images/icons/product/compute_engine-32.png';
end;

Class Function TClouduseraccountsAPI.APIdocumentationLink : String;

begin
  Result:='https://cloud.google.com/compute/docs/access/user-accounts/api/latest/';
end;

Class Function TClouduseraccountsAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TClouduseraccountsAPI.APIbasePath : string;

begin
  Result:='/clouduseraccounts/vm_alpha/projects/';
end;

Class Function TClouduseraccountsAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/clouduseraccounts/vm_alpha/projects/';
end;

Class Function TClouduseraccountsAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TClouduseraccountsAPI.APIservicePath : string;

begin
  Result:='clouduseraccounts/vm_alpha/projects/';
end;

Class Function TClouduseraccountsAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TClouduseraccountsAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,4);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  Result[1].Name:='https://www.googleapis.com/auth/cloud-platform.read-only';
  Result[1].Description:='View your data across Google Cloud Platform services';
  Result[2].Name:='https://www.googleapis.com/auth/cloud.useraccounts';
  Result[2].Description:='Manage your Google Cloud User Accounts';
  Result[3].Name:='https://www.googleapis.com/auth/cloud.useraccounts.readonly';
  Result[3].Description:='View your Google Cloud User Accounts';
  
end;

Class Function TClouduseraccountsAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TClouduseraccountsAPI.RegisterAPIResources;

begin
  TAuditConfig.RegisterObject;
  TAuthorizedKeysView.RegisterObject;
  TBinding.RegisterObject;
  TCondition.RegisterObject;
  TGroup.RegisterObject;
  TGroupList.RegisterObject;
  TGroupsAddMemberRequest.RegisterObject;
  TGroupsRemoveMemberRequest.RegisterObject;
  TLinuxAccountViews.RegisterObject;
  TLinuxGetAuthorizedKeysViewResponse.RegisterObject;
  TLinuxGetLinuxAccountViewsResponse.RegisterObject;
  TLinuxGroupView.RegisterObject;
  TLinuxUserView.RegisterObject;
  TLogConfig.RegisterObject;
  TLogConfigCounterOptions.RegisterObject;
  TOperationTypeerrorTypeerrorsItem.RegisterObject;
  TOperationTypeerror.RegisterObject;
  TOperationTypewarningsItemTypedataItem.RegisterObject;
  TOperationTypewarningsItem.RegisterObject;
  TOperation.RegisterObject;
  TOperationList.RegisterObject;
  TPolicy.RegisterObject;
  TPublicKey.RegisterObject;
  TRule.RegisterObject;
  TTestPermissionsRequest.RegisterObject;
  TTestPermissionsResponse.RegisterObject;
  TUser.RegisterObject;
  TUserList.RegisterObject;
end;


Function TClouduseraccountsAPI.GetGlobalAccountsOperationsInstance : TGlobalAccountsOperationsResource;

begin
  if (FGlobalAccountsOperationsInstance=Nil) then
    FGlobalAccountsOperationsInstance:=CreateGlobalAccountsOperationsResource;
  Result:=FGlobalAccountsOperationsInstance;
end;

Function TClouduseraccountsAPI.CreateGlobalAccountsOperationsResource : TGlobalAccountsOperationsResource;

begin
  Result:=CreateGlobalAccountsOperationsResource(Self);
end;


Function TClouduseraccountsAPI.CreateGlobalAccountsOperationsResource(AOwner : TComponent) : TGlobalAccountsOperationsResource;

begin
  Result:=TGlobalAccountsOperationsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TClouduseraccountsAPI.GetGroupsInstance : TGroupsResource;

begin
  if (FGroupsInstance=Nil) then
    FGroupsInstance:=CreateGroupsResource;
  Result:=FGroupsInstance;
end;

Function TClouduseraccountsAPI.CreateGroupsResource : TGroupsResource;

begin
  Result:=CreateGroupsResource(Self);
end;


Function TClouduseraccountsAPI.CreateGroupsResource(AOwner : TComponent) : TGroupsResource;

begin
  Result:=TGroupsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TClouduseraccountsAPI.GetLinuxInstance : TLinuxResource;

begin
  if (FLinuxInstance=Nil) then
    FLinuxInstance:=CreateLinuxResource;
  Result:=FLinuxInstance;
end;

Function TClouduseraccountsAPI.CreateLinuxResource : TLinuxResource;

begin
  Result:=CreateLinuxResource(Self);
end;


Function TClouduseraccountsAPI.CreateLinuxResource(AOwner : TComponent) : TLinuxResource;

begin
  Result:=TLinuxResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TClouduseraccountsAPI.GetUsersInstance : TUsersResource;

begin
  if (FUsersInstance=Nil) then
    FUsersInstance:=CreateUsersResource;
  Result:=FUsersInstance;
end;

Function TClouduseraccountsAPI.CreateUsersResource : TUsersResource;

begin
  Result:=CreateUsersResource(Self);
end;


Function TClouduseraccountsAPI.CreateUsersResource(AOwner : TComponent) : TUsersResource;

begin
  Result:=TUsersResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TClouduseraccountsAPI.RegisterAPI;
end.
