unit googlecomputeaccounts;
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
//Generated on: 9-5-15 13:22:50
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAuthorizedKeysView = class;
  TGroup = class;
  TGroupList = class;
  TGroupsAddMemberRequest = class;
  TGroupsRemoveMemberRequest = class;
  TLinuxAccountViews = class;
  TLinuxGetAuthorizedKeysViewResponse = class;
  TLinuxGetLinuxAccountViewsResponse = class;
  TLinuxGroupView = class;
  TLinuxUserView = class;
  TOperation = class;
  TOperationList = class;
  TPublicKey = class;
  TUser = class;
  TUserList = class;
  TAuthorizedKeysViewArray = Array of TAuthorizedKeysView;
  TGroupArray = Array of TGroup;
  TGroupListArray = Array of TGroupList;
  TGroupsAddMemberRequestArray = Array of TGroupsAddMemberRequest;
  TGroupsRemoveMemberRequestArray = Array of TGroupsRemoveMemberRequest;
  TLinuxAccountViewsArray = Array of TLinuxAccountViews;
  TLinuxGetAuthorizedKeysViewResponseArray = Array of TLinuxGetAuthorizedKeysViewResponse;
  TLinuxGetLinuxAccountViewsResponseArray = Array of TLinuxGetLinuxAccountViewsResponse;
  TLinuxGroupViewArray = Array of TLinuxGroupView;
  TLinuxUserViewArray = Array of TLinuxUserView;
  TOperationArray = Array of TOperation;
  TOperationListArray = Array of TOperationList;
  TPublicKeyArray = Array of TPublicKey;
  TUserArray = Array of TUser;
  TUserListArray = Array of TUserList;
  //Anonymous types, using auto-generated names
  TOperationTypeerrorTypeerrorsItem = class;
  TOperationTypeerror = class;
  TOperationTypewarningsItemTypedataItem = class;
  TOperationTypewarningsItem = class;
  TGroupListTypeitemsArray = Array of TGroup;
  TLinuxAccountViewsTypegroupViewsArray = Array of TLinuxGroupView;
  TLinuxAccountViewsTypeuserViewsArray = Array of TLinuxUserView;
  TOperationTypeerrorTypeerrorsArray = Array of TOperationTypeerrorTypeerrorsItem;
  TOperationTypewarningsItemTypedataArray = Array of TOperationTypewarningsItemTypedataItem;
  TOperationTypewarningsArray = Array of TOperationTypewarningsItem;
  TOperationListTypeitemsArray = Array of TOperation;
  TUserTypepublicKeysArray = Array of TPublicKey;
  TUserListTypeitemsArray = Array of TUser;
  
  { --------------------------------------------------------------------
    TAuthorizedKeysView
    --------------------------------------------------------------------}
  
  TAuthorizedKeysView = Class(TGoogleBaseObject)
  Private
    Fkeys : TStringArray;
  Protected
    //Property setters
    Procedure Setkeys(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property keys : TStringArray Index 0 Read Fkeys Write Setkeys;
  end;
  TAuthorizedKeysViewClass = Class of TAuthorizedKeysView;
  
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
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setmembers(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
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
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TGroupListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
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
    Procedure Setusers(AIndex : Integer; AValue : TStringArray); virtual;
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
    Procedure Setusers(AIndex : Integer; AValue : TStringArray); virtual;
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
    Procedure SetgroupViews(AIndex : Integer; AValue : TLinuxAccountViewsTypegroupViewsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetuserViews(AIndex : Integer; AValue : TLinuxAccountViewsTypeuserViewsArray); virtual;
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
    Procedure Setresource(AIndex : Integer; AValue : TAuthorizedKeysView); virtual;
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
    Procedure Setresource(AIndex : Integer; AValue : TLinuxAccountViews); virtual;
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
    Procedure Setgid(AIndex : Integer; AValue : integer); virtual;
    Procedure SetgroupName(AIndex : Integer; AValue : String); virtual;
    Procedure Setmembers(AIndex : Integer; AValue : TStringArray); virtual;
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
    Procedure Setgecos(AIndex : Integer; AValue : String); virtual;
    Procedure Setgid(AIndex : Integer; AValue : integer); virtual;
    Procedure SethomeDirectory(AIndex : Integer; AValue : String); virtual;
    Procedure Setshell(AIndex : Integer; AValue : String); virtual;
    Procedure Setuid(AIndex : Integer; AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; AValue : String); virtual;
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
    TOperationTypeerrorTypeerrorsItem
    --------------------------------------------------------------------}
  
  TOperationTypeerrorTypeerrorsItem = Class(TGoogleBaseObject)
  Private
    Fcode : String;
    Flocation : String;
    Fmessage : String;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : String); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : String); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : String); virtual;
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
    Procedure Seterrors(AIndex : Integer; AValue : TOperationTypeerrorTypeerrorsArray); virtual;
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
    Procedure Setkey(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
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
    Procedure Setcode(AIndex : Integer; AValue : String); virtual;
    Procedure Setdata(AIndex : Integer; AValue : TOperationTypewarningsItemTypedataArray); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : String); virtual;
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
    Procedure SetclientOperationId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : String); virtual;
    Procedure SetendTime(AIndex : Integer; AValue : String); virtual;
    Procedure Seterror(AIndex : Integer; AValue : TOperationTypeerror); virtual;
    Procedure SethttpErrorMessage(AIndex : Integer; AValue : String); virtual;
    Procedure SethttpErrorStatusCode(AIndex : Integer; AValue : integer); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetinsertTime(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetoperationType(AIndex : Integer; AValue : String); virtual;
    Procedure Setprogress(AIndex : Integer; AValue : integer); virtual;
    Procedure Setregion(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartTime(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure SetstatusMessage(AIndex : Integer; AValue : String); virtual;
    Procedure SettargetId(AIndex : Integer; AValue : String); virtual;
    Procedure SettargetLink(AIndex : Integer; AValue : String); virtual;
    Procedure Setuser(AIndex : Integer; AValue : String); virtual;
    Procedure Setwarnings(AIndex : Integer; AValue : TOperationTypewarningsArray); virtual;
    Procedure Setzone(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property clientOperationId : String Index 0 Read FclientOperationId Write SetclientOperationId;
    Property creationTimestamp : String Index 8 Read FcreationTimestamp Write SetcreationTimestamp;
    Property endTime : String Index 16 Read FendTime Write SetendTime;
    Property error : TOperationTypeerror Index 24 Read Ferror Write Seterror;
    Property httpErrorMessage : String Index 32 Read FhttpErrorMessage Write SethttpErrorMessage;
    Property httpErrorStatusCode : integer Index 40 Read FhttpErrorStatusCode Write SethttpErrorStatusCode;
    Property id : String Index 48 Read Fid Write Setid;
    Property insertTime : String Index 56 Read FinsertTime Write SetinsertTime;
    Property kind : String Index 64 Read Fkind Write Setkind;
    Property name : String Index 72 Read Fname Write Setname;
    Property operationType : String Index 80 Read FoperationType Write SetoperationType;
    Property progress : integer Index 88 Read Fprogress Write Setprogress;
    Property region : String Index 96 Read Fregion Write Setregion;
    Property selfLink : String Index 104 Read FselfLink Write SetselfLink;
    Property startTime : String Index 112 Read FstartTime Write SetstartTime;
    Property status : String Index 120 Read Fstatus Write Setstatus;
    Property statusMessage : String Index 128 Read FstatusMessage Write SetstatusMessage;
    Property targetId : String Index 136 Read FtargetId Write SettargetId;
    Property targetLink : String Index 144 Read FtargetLink Write SettargetLink;
    Property user : String Index 152 Read Fuser Write Setuser;
    Property warnings : TOperationTypewarningsArray Index 160 Read Fwarnings Write Setwarnings;
    Property zone : String Index 168 Read Fzone Write Setzone;
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
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TOperationListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
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
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetexpirationTimestamp(AIndex : Integer; AValue : String); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : String); virtual;
    Procedure Setkey(AIndex : Integer; AValue : String); virtual;
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
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setgroups(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setowner(AIndex : Integer; AValue : String); virtual;
    Procedure SetpublicKeys(AIndex : Integer; AValue : TUserTypepublicKeysArray); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
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
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TUserListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
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
    pageToken : String;
  end;
  
  TGroupsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function AddMember(groupName: string; project: string; aGroupsAddMemberRequest : TGroupsAddMemberRequest) : TOperation;
    Function Delete(groupName: string; project: string) : TOperation;
    Function Get(groupName: string; project: string) : TGroup;
    Function Insert(project: string; aGroup : TGroup) : TOperation;
    Function List(project: string; AQuery : string  = '') : TGroupList;
    Function List(project: string; AQuery : TGroupslistOptions) : TGroupList;
    Function RemoveMember(groupName: string; project: string; aGroupsRemoveMemberRequest : TGroupsRemoveMemberRequest) : TOperation;
  end;
  
  
  { --------------------------------------------------------------------
    TLinuxResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TLinuxResource, method GetAuthorizedKeysView
  
  TLinuxGetAuthorizedKeysViewOptions = Record
    instance : String;
  end;
  
  
  //Optional query Options for TLinuxResource, method GetLinuxAccountViews
  
  TLinuxGetLinuxAccountViewsOptions = Record
    filter : String;
    instance : String;
    maxResults : integer;
    pageToken : String;
    user : String;
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
    Function Insert(project: string; aUser : TUser) : TOperation;
    Function List(project: string; AQuery : string  = '') : TUserList;
    Function List(project: string; AQuery : TUserslistOptions) : TUserList;
    Function RemovePublicKey(project: string; user: string; AQuery : string  = '') : TOperation;
    Function RemovePublicKey(project: string; user: string; AQuery : TUsersremovePublicKeyOptions) : TOperation;
  end;
  
  
  { --------------------------------------------------------------------
    TComputeaccountsAPI
    --------------------------------------------------------------------}
  
  TComputeaccountsAPI = Class(TGoogleAPI)
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
  TAuthorizedKeysView
  --------------------------------------------------------------------}


Procedure TAuthorizedKeysView.Setkeys(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fkeys=AValue) then exit;
  Fkeys:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGroup
  --------------------------------------------------------------------}


Procedure TGroup.SetcreationTimestamp(AIndex : Integer; AValue : String); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.Setmembers(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fmembers=AValue) then exit;
  Fmembers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGroupList
  --------------------------------------------------------------------}


Procedure TGroupList.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupList.Setitems(AIndex : Integer; AValue : TGroupListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupList.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupList.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGroupsAddMemberRequest
  --------------------------------------------------------------------}


Procedure TGroupsAddMemberRequest.Setusers(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fusers=AValue) then exit;
  Fusers:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGroupsRemoveMemberRequest
  --------------------------------------------------------------------}


Procedure TGroupsRemoveMemberRequest.Setusers(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fusers=AValue) then exit;
  Fusers:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLinuxAccountViews
  --------------------------------------------------------------------}


Procedure TLinuxAccountViews.SetgroupViews(AIndex : Integer; AValue : TLinuxAccountViewsTypegroupViewsArray); 

begin
  If (FgroupViews=AValue) then exit;
  FgroupViews:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinuxAccountViews.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinuxAccountViews.SetuserViews(AIndex : Integer; AValue : TLinuxAccountViewsTypeuserViewsArray); 

begin
  If (FuserViews=AValue) then exit;
  FuserViews:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLinuxGetAuthorizedKeysViewResponse
  --------------------------------------------------------------------}


Procedure TLinuxGetAuthorizedKeysViewResponse.Setresource(AIndex : Integer; AValue : TAuthorizedKeysView); 

begin
  If (Fresource=AValue) then exit;
  Fresource:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLinuxGetLinuxAccountViewsResponse
  --------------------------------------------------------------------}


Procedure TLinuxGetLinuxAccountViewsResponse.Setresource(AIndex : Integer; AValue : TLinuxAccountViews); 

begin
  If (Fresource=AValue) then exit;
  Fresource:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLinuxGroupView
  --------------------------------------------------------------------}


Procedure TLinuxGroupView.Setgid(AIndex : Integer; AValue : integer); 

begin
  If (Fgid=AValue) then exit;
  Fgid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinuxGroupView.SetgroupName(AIndex : Integer; AValue : String); 

begin
  If (FgroupName=AValue) then exit;
  FgroupName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinuxGroupView.Setmembers(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fmembers=AValue) then exit;
  Fmembers:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLinuxUserView
  --------------------------------------------------------------------}


Procedure TLinuxUserView.Setgecos(AIndex : Integer; AValue : String); 

begin
  If (Fgecos=AValue) then exit;
  Fgecos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinuxUserView.Setgid(AIndex : Integer; AValue : integer); 

begin
  If (Fgid=AValue) then exit;
  Fgid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinuxUserView.SethomeDirectory(AIndex : Integer; AValue : String); 

begin
  If (FhomeDirectory=AValue) then exit;
  FhomeDirectory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinuxUserView.Setshell(AIndex : Integer; AValue : String); 

begin
  If (Fshell=AValue) then exit;
  Fshell:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinuxUserView.Setuid(AIndex : Integer; AValue : integer); 

begin
  If (Fuid=AValue) then exit;
  Fuid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinuxUserView.Setusername(AIndex : Integer; AValue : String); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationTypeerrorTypeerrorsItem
  --------------------------------------------------------------------}


Procedure TOperationTypeerrorTypeerrorsItem.Setcode(AIndex : Integer; AValue : String); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationTypeerrorTypeerrorsItem.Setlocation(AIndex : Integer; AValue : String); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationTypeerrorTypeerrorsItem.Setmessage(AIndex : Integer; AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationTypeerror
  --------------------------------------------------------------------}


Procedure TOperationTypeerror.Seterrors(AIndex : Integer; AValue : TOperationTypeerrorTypeerrorsArray); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationTypewarningsItemTypedataItem
  --------------------------------------------------------------------}


Procedure TOperationTypewarningsItemTypedataItem.Setkey(AIndex : Integer; AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationTypewarningsItemTypedataItem.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationTypewarningsItem
  --------------------------------------------------------------------}


Procedure TOperationTypewarningsItem.Setcode(AIndex : Integer; AValue : String); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationTypewarningsItem.Setdata(AIndex : Integer; AValue : TOperationTypewarningsItemTypedataArray); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationTypewarningsItem.Setmessage(AIndex : Integer; AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperation
  --------------------------------------------------------------------}


Procedure TOperation.SetclientOperationId(AIndex : Integer; AValue : String); 

begin
  If (FclientOperationId=AValue) then exit;
  FclientOperationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetcreationTimestamp(AIndex : Integer; AValue : String); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetendTime(AIndex : Integer; AValue : String); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Seterror(AIndex : Integer; AValue : TOperationTypeerror); 

begin
  If (Ferror=AValue) then exit;
  Ferror:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SethttpErrorMessage(AIndex : Integer; AValue : String); 

begin
  If (FhttpErrorMessage=AValue) then exit;
  FhttpErrorMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SethttpErrorStatusCode(AIndex : Integer; AValue : integer); 

begin
  If (FhttpErrorStatusCode=AValue) then exit;
  FhttpErrorStatusCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetinsertTime(AIndex : Integer; AValue : String); 

begin
  If (FinsertTime=AValue) then exit;
  FinsertTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetoperationType(AIndex : Integer; AValue : String); 

begin
  If (FoperationType=AValue) then exit;
  FoperationType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setprogress(AIndex : Integer; AValue : integer); 

begin
  If (Fprogress=AValue) then exit;
  Fprogress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setregion(AIndex : Integer; AValue : String); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetstartTime(AIndex : Integer; AValue : String); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetstatusMessage(AIndex : Integer; AValue : String); 

begin
  If (FstatusMessage=AValue) then exit;
  FstatusMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SettargetId(AIndex : Integer; AValue : String); 

begin
  If (FtargetId=AValue) then exit;
  FtargetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SettargetLink(AIndex : Integer; AValue : String); 

begin
  If (FtargetLink=AValue) then exit;
  FtargetLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setuser(AIndex : Integer; AValue : String); 

begin
  If (Fuser=AValue) then exit;
  Fuser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setwarnings(AIndex : Integer; AValue : TOperationTypewarningsArray); 

begin
  If (Fwarnings=AValue) then exit;
  Fwarnings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setzone(AIndex : Integer; AValue : String); 

begin
  If (Fzone=AValue) then exit;
  Fzone:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationList
  --------------------------------------------------------------------}


Procedure TOperationList.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.Setitems(AIndex : Integer; AValue : TOperationListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPublicKey
  --------------------------------------------------------------------}


Procedure TPublicKey.SetcreationTimestamp(AIndex : Integer; AValue : String); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublicKey.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublicKey.SetexpirationTimestamp(AIndex : Integer; AValue : String); 

begin
  If (FexpirationTimestamp=AValue) then exit;
  FexpirationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublicKey.Setfingerprint(AIndex : Integer; AValue : String); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublicKey.Setkey(AIndex : Integer; AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUser
  --------------------------------------------------------------------}


Procedure TUser.SetcreationTimestamp(AIndex : Integer; AValue : String); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setgroups(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fgroups=AValue) then exit;
  Fgroups:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setowner(AIndex : Integer; AValue : String); 

begin
  If (Fowner=AValue) then exit;
  Fowner:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.SetpublicKeys(AIndex : Integer; AValue : TUserTypepublicKeysArray); 

begin
  If (FpublicKeys=AValue) then exit;
  FpublicKeys:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUserList
  --------------------------------------------------------------------}


Procedure TUserList.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserList.Setitems(AIndex : Integer; AValue : TUserListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserList.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserList.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGlobalAccountsOperationsResource
  --------------------------------------------------------------------}


Class Function TGlobalAccountsOperationsResource.ResourceName : String;

begin
  Result:='globalAccountsOperations';
end;

Class Function TGlobalAccountsOperationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcomputeaccountsAPI;
end;

Procedure TGlobalAccountsOperationsResource.Delete(operation: string; project: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/global/operations/{operation}';
  _Methodid   = 'computeaccounts.globalAccountsOperations.delete';

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
  _Methodid   = 'computeaccounts.globalAccountsOperations.get';

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
  _Methodid   = 'computeaccounts.globalAccountsOperations.list';

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
  Result:=TcomputeaccountsAPI;
end;

Function TGroupsResource.AddMember(groupName: string; project: string; aGroupsAddMemberRequest : TGroupsAddMemberRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/groups/{groupName}/addMember';
  _Methodid   = 'computeaccounts.groups.addMember';

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
  _Methodid   = 'computeaccounts.groups.delete';

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
  _Methodid   = 'computeaccounts.groups.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['groupName',groupName,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TGroup) as TGroup;
end;

Function TGroupsResource.Insert(project: string; aGroup : TGroup) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/groups';
  _Methodid   = 'computeaccounts.groups.insert';

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
  _Methodid   = 'computeaccounts.groups.list';

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
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;

Function TGroupsResource.RemoveMember(groupName: string; project: string; aGroupsRemoveMemberRequest : TGroupsRemoveMemberRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/groups/{groupName}/removeMember';
  _Methodid   = 'computeaccounts.groups.removeMember';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['groupName',groupName,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aGroupsRemoveMemberRequest,TOperation) as TOperation;
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
  Result:=TcomputeaccountsAPI;
end;

Function TLinuxResource.GetAuthorizedKeysView(project: string; user: string; zone: string; AQuery : string = '') : TLinuxGetAuthorizedKeysViewResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/authorizedKeysView/{user}';
  _Methodid   = 'computeaccounts.linux.getAuthorizedKeysView';

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
  Result:=GetAuthorizedKeysView(project,user,zone,_Q);
end;

Function TLinuxResource.GetLinuxAccountViews(project: string; zone: string; AQuery : string = '') : TLinuxGetLinuxAccountViewsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/linuxAccountViews';
  _Methodid   = 'computeaccounts.linux.getLinuxAccountViews';

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
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'user',AQuery.user);
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
  Result:=TcomputeaccountsAPI;
end;

Function TUsersResource.AddPublicKey(project: string; user: string; aPublicKey : TPublicKey) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/users/{user}/addPublicKey';
  _Methodid   = 'computeaccounts.users.addPublicKey';

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
  _Methodid   = 'computeaccounts.users.delete';

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
  _Methodid   = 'computeaccounts.users.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'user',user]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TUser) as TUser;
end;

Function TUsersResource.Insert(project: string; aUser : TUser) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/users';
  _Methodid   = 'computeaccounts.users.insert';

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
  _Methodid   = 'computeaccounts.users.list';

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
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;

Function TUsersResource.RemovePublicKey(project: string; user: string; AQuery : string = '') : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/users/{user}/removePublicKey';
  _Methodid   = 'computeaccounts.users.removePublicKey';

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



{ --------------------------------------------------------------------
  TComputeaccountsAPI
  --------------------------------------------------------------------}

Class Function TComputeaccountsAPI.APIName : String;

begin
  Result:='computeaccounts';
end;

Class Function TComputeaccountsAPI.APIVersion : String;

begin
  Result:='alpha';
end;

Class Function TComputeaccountsAPI.APIRevision : String;

begin
  Result:='20150423';
end;

Class Function TComputeaccountsAPI.APIID : String;

begin
  Result:='computeaccounts:alpha';
end;

Class Function TComputeaccountsAPI.APITitle : String;

begin
  Result:='Compute Accounts API';
end;

Class Function TComputeaccountsAPI.APIDescription : String;

begin
  Result:='API for the Google Compute Accounts service.';
end;

Class Function TComputeaccountsAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TComputeaccountsAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TComputeaccountsAPI.APIIcon16 : String;

begin
  Result:='https://www.google.com/images/icons/product/compute_engine-16.png';
end;

Class Function TComputeaccountsAPI.APIIcon32 : String;

begin
  Result:='https://www.google.com/images/icons/product/compute_engine-32.png';
end;

Class Function TComputeaccountsAPI.APIdocumentationLink : String;

begin
  Result:='https://cloud.google.com/compute/docs/access/user-accounts/api/latest/';
end;

Class Function TComputeaccountsAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TComputeaccountsAPI.APIbasePath : string;

begin
  Result:='/computeaccounts/alpha/projects/';
end;

Class Function TComputeaccountsAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/computeaccounts/alpha/projects/';
end;

Class Function TComputeaccountsAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TComputeaccountsAPI.APIservicePath : string;

begin
  Result:='computeaccounts/alpha/projects/';
end;

Class Function TComputeaccountsAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TComputeaccountsAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,3);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  Result[1].Name:='https://www.googleapis.com/auth/computeaccounts';
  Result[1].Description:='Manage your Google Compute Accounts';
  Result[2].Name:='https://www.googleapis.com/auth/computeaccounts.readonly';
  Result[2].Description:='View your Google Compute Accounts';
  
end;

Class Function TComputeaccountsAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TComputeaccountsAPI.RegisterAPIResources;

begin
  TAuthorizedKeysView.RegisterObject;
  TGroup.RegisterObject;
  TGroupList.RegisterObject;
  TGroupsAddMemberRequest.RegisterObject;
  TGroupsRemoveMemberRequest.RegisterObject;
  TLinuxAccountViews.RegisterObject;
  TLinuxGetAuthorizedKeysViewResponse.RegisterObject;
  TLinuxGetLinuxAccountViewsResponse.RegisterObject;
  TLinuxGroupView.RegisterObject;
  TLinuxUserView.RegisterObject;
  TOperationTypeerrorTypeerrorsItem.RegisterObject;
  TOperationTypeerror.RegisterObject;
  TOperationTypewarningsItemTypedataItem.RegisterObject;
  TOperationTypewarningsItem.RegisterObject;
  TOperation.RegisterObject;
  TOperationList.RegisterObject;
  TPublicKey.RegisterObject;
  TUser.RegisterObject;
  TUserList.RegisterObject;
end;


Function TComputeaccountsAPI.GetGlobalAccountsOperationsInstance : TGlobalAccountsOperationsResource;

begin
  if (FGlobalAccountsOperationsInstance=Nil) then
    FGlobalAccountsOperationsInstance:=CreateGlobalAccountsOperationsResource;
  Result:=FGlobalAccountsOperationsInstance;
end;

Function TComputeaccountsAPI.CreateGlobalAccountsOperationsResource : TGlobalAccountsOperationsResource;

begin
  Result:=CreateGlobalAccountsOperationsResource(Self);
end;


Function TComputeaccountsAPI.CreateGlobalAccountsOperationsResource(AOwner : TComponent) : TGlobalAccountsOperationsResource;

begin
  Result:=TGlobalAccountsOperationsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeaccountsAPI.GetGroupsInstance : TGroupsResource;

begin
  if (FGroupsInstance=Nil) then
    FGroupsInstance:=CreateGroupsResource;
  Result:=FGroupsInstance;
end;

Function TComputeaccountsAPI.CreateGroupsResource : TGroupsResource;

begin
  Result:=CreateGroupsResource(Self);
end;


Function TComputeaccountsAPI.CreateGroupsResource(AOwner : TComponent) : TGroupsResource;

begin
  Result:=TGroupsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeaccountsAPI.GetLinuxInstance : TLinuxResource;

begin
  if (FLinuxInstance=Nil) then
    FLinuxInstance:=CreateLinuxResource;
  Result:=FLinuxInstance;
end;

Function TComputeaccountsAPI.CreateLinuxResource : TLinuxResource;

begin
  Result:=CreateLinuxResource(Self);
end;


Function TComputeaccountsAPI.CreateLinuxResource(AOwner : TComponent) : TLinuxResource;

begin
  Result:=TLinuxResource.Create(AOwner);
  Result.API:=Self;
end;



Function TComputeaccountsAPI.GetUsersInstance : TUsersResource;

begin
  if (FUsersInstance=Nil) then
    FUsersInstance:=CreateUsersResource;
  Result:=FUsersInstance;
end;

Function TComputeaccountsAPI.CreateUsersResource : TUsersResource;

begin
  Result:=CreateUsersResource(Self);
end;


Function TComputeaccountsAPI.CreateUsersResource(AOwner : TComponent) : TUsersResource;

begin
  Result:=TUsersResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TComputeaccountsAPI.RegisterAPI;
end.
