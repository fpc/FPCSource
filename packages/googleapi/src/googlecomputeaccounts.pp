unit googlecomputeaccounts;
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
  TAuthorizedKeysView = class;
  TAuthorizedKeysViewArray = Array of TAuthorizedKeysView;
  TAuthorizedKeysViewkeys = class;
  TAuthorizedKeysViewkeysArray = Array of TAuthorizedKeysViewkeys;
  TGroup = class;
  TGroupArray = Array of TGroup;
  TGroupmembers = class;
  TGroupmembersArray = Array of TGroupmembers;
  TGroupList = class;
  TGroupListArray = Array of TGroupList;
  TGroupListitems = class;
  TGroupListitemsArray = Array of TGroupListitems;
  TGroupsAddMemberRequest = class;
  TGroupsAddMemberRequestArray = Array of TGroupsAddMemberRequest;
  TGroupsAddMemberRequestusers = class;
  TGroupsAddMemberRequestusersArray = Array of TGroupsAddMemberRequestusers;
  TGroupsRemoveMemberRequest = class;
  TGroupsRemoveMemberRequestArray = Array of TGroupsRemoveMemberRequest;
  TGroupsRemoveMemberRequestusers = class;
  TGroupsRemoveMemberRequestusersArray = Array of TGroupsRemoveMemberRequestusers;
  TLinuxAccountViews = class;
  TLinuxAccountViewsArray = Array of TLinuxAccountViews;
  TLinuxAccountViewsgroupViews = class;
  TLinuxAccountViewsgroupViewsArray = Array of TLinuxAccountViewsgroupViews;
  TLinuxAccountViewsuserViews = class;
  TLinuxAccountViewsuserViewsArray = Array of TLinuxAccountViewsuserViews;
  TLinuxGetAuthorizedKeysViewResponse = class;
  TLinuxGetAuthorizedKeysViewResponseArray = Array of TLinuxGetAuthorizedKeysViewResponse;
  TLinuxGetLinuxAccountViewsResponse = class;
  TLinuxGetLinuxAccountViewsResponseArray = Array of TLinuxGetLinuxAccountViewsResponse;
  TLinuxGroupView = class;
  TLinuxGroupViewArray = Array of TLinuxGroupView;
  TLinuxGroupViewmembers = class;
  TLinuxGroupViewmembersArray = Array of TLinuxGroupViewmembers;
  TLinuxUserView = class;
  TLinuxUserViewArray = Array of TLinuxUserView;
  TOperation = class;
  TOperationArray = Array of TOperation;
  TOperationerror = class;
  TOperationerrorArray = Array of TOperationerror;
  TOperationerrorerrors = class;
  TOperationerrorerrorsArray = Array of TOperationerrorerrors;
  TOperationwarnings = class;
  TOperationwarningsArray = Array of TOperationwarnings;
  TOperationwarningsdata = class;
  TOperationwarningsdataArray = Array of TOperationwarningsdata;
  TOperationList = class;
  TOperationListArray = Array of TOperationList;
  TOperationListitems = class;
  TOperationListitemsArray = Array of TOperationListitems;
  TPublicKey = class;
  TPublicKeyArray = Array of TPublicKey;
  TUser = class;
  TUserArray = Array of TUser;
  TUsergroups = class;
  TUsergroupsArray = Array of TUsergroups;
  TUserpublicKeys = class;
  TUserpublicKeysArray = Array of TUserpublicKeys;
  TUserList = class;
  TUserListArray = Array of TUserList;
  TUserListitems = class;
  TUserListitemsArray = Array of TUserListitems;
  
  { --------------------------------------------------------------------
    TAuthorizedKeysView
    --------------------------------------------------------------------}
  
  TAuthorizedKeysView = Class(TGoogleBaseObject)
  Private
    Fkeys : TAuthorizedKeysViewkeys;
  Protected
    //Property setters
    Procedure Setkeys(AIndex : Integer; AValue : TAuthorizedKeysViewkeys); virtual;
  Public
  Published
    Property keys : TAuthorizedKeysViewkeys Index 0 Read Fkeys Write Setkeys;
  end;
  TAuthorizedKeysViewClass = Class of TAuthorizedKeysView;
  
  { --------------------------------------------------------------------
    TAuthorizedKeysViewkeys
    --------------------------------------------------------------------}
  
  TAuthorizedKeysViewkeys = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAuthorizedKeysViewkeysClass = Class of TAuthorizedKeysViewkeys;
  
  { --------------------------------------------------------------------
    TGroup
    --------------------------------------------------------------------}
  
  TGroup = Class(TGoogleBaseObject)
  Private
    FcreationTimestamp : string;
    Fdescription : string;
    Fid : string;
    Fkind : string;
    Fmembers : TGroupmembers;
    Fname : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setmembers(AIndex : Integer; AValue : TGroupmembers); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property creationTimestamp : string Index 0 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : string Index 8 Read Fdescription Write Setdescription;
    Property id : string Index 16 Read Fid Write Setid;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property members : TGroupmembers Index 32 Read Fmembers Write Setmembers;
    Property name : string Index 40 Read Fname Write Setname;
    Property selfLink : string Index 48 Read FselfLink Write SetselfLink;
  end;
  TGroupClass = Class of TGroup;
  
  { --------------------------------------------------------------------
    TGroupmembers
    --------------------------------------------------------------------}
  
  TGroupmembers = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TGroupmembersClass = Class of TGroupmembers;
  
  { --------------------------------------------------------------------
    TGroupList
    --------------------------------------------------------------------}
  
  TGroupList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TGroupListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TGroupListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TGroupListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TGroupListClass = Class of TGroupList;
  
  { --------------------------------------------------------------------
    TGroupListitems
    --------------------------------------------------------------------}
  
  TGroupListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TGroupListitemsClass = Class of TGroupListitems;
  
  { --------------------------------------------------------------------
    TGroupsAddMemberRequest
    --------------------------------------------------------------------}
  
  TGroupsAddMemberRequest = Class(TGoogleBaseObject)
  Private
    Fusers : TGroupsAddMemberRequestusers;
  Protected
    //Property setters
    Procedure Setusers(AIndex : Integer; AValue : TGroupsAddMemberRequestusers); virtual;
  Public
  Published
    Property users : TGroupsAddMemberRequestusers Index 0 Read Fusers Write Setusers;
  end;
  TGroupsAddMemberRequestClass = Class of TGroupsAddMemberRequest;
  
  { --------------------------------------------------------------------
    TGroupsAddMemberRequestusers
    --------------------------------------------------------------------}
  
  TGroupsAddMemberRequestusers = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TGroupsAddMemberRequestusersClass = Class of TGroupsAddMemberRequestusers;
  
  { --------------------------------------------------------------------
    TGroupsRemoveMemberRequest
    --------------------------------------------------------------------}
  
  TGroupsRemoveMemberRequest = Class(TGoogleBaseObject)
  Private
    Fusers : TGroupsRemoveMemberRequestusers;
  Protected
    //Property setters
    Procedure Setusers(AIndex : Integer; AValue : TGroupsRemoveMemberRequestusers); virtual;
  Public
  Published
    Property users : TGroupsRemoveMemberRequestusers Index 0 Read Fusers Write Setusers;
  end;
  TGroupsRemoveMemberRequestClass = Class of TGroupsRemoveMemberRequest;
  
  { --------------------------------------------------------------------
    TGroupsRemoveMemberRequestusers
    --------------------------------------------------------------------}
  
  TGroupsRemoveMemberRequestusers = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TGroupsRemoveMemberRequestusersClass = Class of TGroupsRemoveMemberRequestusers;
  
  { --------------------------------------------------------------------
    TLinuxAccountViews
    --------------------------------------------------------------------}
  
  TLinuxAccountViews = Class(TGoogleBaseObject)
  Private
    FgroupViews : TLinuxAccountViewsgroupViews;
    Fkind : string;
    FuserViews : TLinuxAccountViewsuserViews;
  Protected
    //Property setters
    Procedure SetgroupViews(AIndex : Integer; AValue : TLinuxAccountViewsgroupViews); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetuserViews(AIndex : Integer; AValue : TLinuxAccountViewsuserViews); virtual;
  Public
  Published
    Property groupViews : TLinuxAccountViewsgroupViews Index 0 Read FgroupViews Write SetgroupViews;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property userViews : TLinuxAccountViewsuserViews Index 16 Read FuserViews Write SetuserViews;
  end;
  TLinuxAccountViewsClass = Class of TLinuxAccountViews;
  
  { --------------------------------------------------------------------
    TLinuxAccountViewsgroupViews
    --------------------------------------------------------------------}
  
  TLinuxAccountViewsgroupViews = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLinuxAccountViewsgroupViewsClass = Class of TLinuxAccountViewsgroupViews;
  
  { --------------------------------------------------------------------
    TLinuxAccountViewsuserViews
    --------------------------------------------------------------------}
  
  TLinuxAccountViewsuserViews = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLinuxAccountViewsuserViewsClass = Class of TLinuxAccountViewsuserViews;
  
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
    FgroupName : string;
    Fmembers : TLinuxGroupViewmembers;
  Protected
    //Property setters
    Procedure Setgid(AIndex : Integer; AValue : integer); virtual;
    Procedure SetgroupName(AIndex : Integer; AValue : string); virtual;
    Procedure Setmembers(AIndex : Integer; AValue : TLinuxGroupViewmembers); virtual;
  Public
  Published
    Property gid : integer Index 0 Read Fgid Write Setgid;
    Property groupName : string Index 8 Read FgroupName Write SetgroupName;
    Property members : TLinuxGroupViewmembers Index 16 Read Fmembers Write Setmembers;
  end;
  TLinuxGroupViewClass = Class of TLinuxGroupView;
  
  { --------------------------------------------------------------------
    TLinuxGroupViewmembers
    --------------------------------------------------------------------}
  
  TLinuxGroupViewmembers = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLinuxGroupViewmembersClass = Class of TLinuxGroupViewmembers;
  
  { --------------------------------------------------------------------
    TLinuxUserView
    --------------------------------------------------------------------}
  
  TLinuxUserView = Class(TGoogleBaseObject)
  Private
    Fgecos : string;
    Fgid : integer;
    FhomeDirectory : string;
    Fshell : string;
    Fuid : integer;
    Fusername : string;
  Protected
    //Property setters
    Procedure Setgecos(AIndex : Integer; AValue : string); virtual;
    Procedure Setgid(AIndex : Integer; AValue : integer); virtual;
    Procedure SethomeDirectory(AIndex : Integer; AValue : string); virtual;
    Procedure Setshell(AIndex : Integer; AValue : string); virtual;
    Procedure Setuid(AIndex : Integer; AValue : integer); virtual;
    Procedure Setusername(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property gecos : string Index 0 Read Fgecos Write Setgecos;
    Property gid : integer Index 8 Read Fgid Write Setgid;
    Property homeDirectory : string Index 16 Read FhomeDirectory Write SethomeDirectory;
    Property shell : string Index 24 Read Fshell Write Setshell;
    Property uid : integer Index 32 Read Fuid Write Setuid;
    Property username : string Index 40 Read Fusername Write Setusername;
  end;
  TLinuxUserViewClass = Class of TLinuxUserView;
  
  { --------------------------------------------------------------------
    TOperation
    --------------------------------------------------------------------}
  
  TOperation = Class(TGoogleBaseObject)
  Private
    FclientOperationId : string;
    FcreationTimestamp : string;
    FendTime : string;
    Ferror : TOperationerror;
    FhttpErrorMessage : string;
    FhttpErrorStatusCode : integer;
    Fid : string;
    FinsertTime : string;
    Fkind : string;
    Fname : string;
    FoperationType : string;
    Fprogress : integer;
    Fregion : string;
    FselfLink : string;
    FstartTime : string;
    Fstatus : string;
    FstatusMessage : string;
    FtargetId : string;
    FtargetLink : string;
    Fuser : string;
    Fwarnings : TOperationwarnings;
    Fzone : string;
  Protected
    //Property setters
    Procedure SetclientOperationId(AIndex : Integer; AValue : string); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure SetendTime(AIndex : Integer; AValue : string); virtual;
    Procedure Seterror(AIndex : Integer; AValue : TOperationerror); virtual;
    Procedure SethttpErrorMessage(AIndex : Integer; AValue : string); virtual;
    Procedure SethttpErrorStatusCode(AIndex : Integer; AValue : integer); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetinsertTime(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetoperationType(AIndex : Integer; AValue : string); virtual;
    Procedure Setprogress(AIndex : Integer; AValue : integer); virtual;
    Procedure Setregion(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartTime(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure SetstatusMessage(AIndex : Integer; AValue : string); virtual;
    Procedure SettargetId(AIndex : Integer; AValue : string); virtual;
    Procedure SettargetLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setuser(AIndex : Integer; AValue : string); virtual;
    Procedure Setwarnings(AIndex : Integer; AValue : TOperationwarnings); virtual;
    Procedure Setzone(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property clientOperationId : string Index 0 Read FclientOperationId Write SetclientOperationId;
    Property creationTimestamp : string Index 8 Read FcreationTimestamp Write SetcreationTimestamp;
    Property endTime : string Index 16 Read FendTime Write SetendTime;
    Property error : TOperationerror Index 24 Read Ferror Write Seterror;
    Property httpErrorMessage : string Index 32 Read FhttpErrorMessage Write SethttpErrorMessage;
    Property httpErrorStatusCode : integer Index 40 Read FhttpErrorStatusCode Write SethttpErrorStatusCode;
    Property id : string Index 48 Read Fid Write Setid;
    Property insertTime : string Index 56 Read FinsertTime Write SetinsertTime;
    Property kind : string Index 64 Read Fkind Write Setkind;
    Property name : string Index 72 Read Fname Write Setname;
    Property operationType : string Index 80 Read FoperationType Write SetoperationType;
    Property progress : integer Index 88 Read Fprogress Write Setprogress;
    Property region : string Index 96 Read Fregion Write Setregion;
    Property selfLink : string Index 104 Read FselfLink Write SetselfLink;
    Property startTime : string Index 112 Read FstartTime Write SetstartTime;
    Property status : string Index 120 Read Fstatus Write Setstatus;
    Property statusMessage : string Index 128 Read FstatusMessage Write SetstatusMessage;
    Property targetId : string Index 136 Read FtargetId Write SettargetId;
    Property targetLink : string Index 144 Read FtargetLink Write SettargetLink;
    Property user : string Index 152 Read Fuser Write Setuser;
    Property warnings : TOperationwarnings Index 160 Read Fwarnings Write Setwarnings;
    Property zone : string Index 168 Read Fzone Write Setzone;
  end;
  TOperationClass = Class of TOperation;
  
  { --------------------------------------------------------------------
    TOperationerror
    --------------------------------------------------------------------}
  
  TOperationerror = Class(TGoogleBaseObject)
  Private
    Ferrors : TOperationerrorerrors;
  Protected
    //Property setters
    Procedure Seterrors(AIndex : Integer; AValue : TOperationerrorerrors); virtual;
  Public
  Published
    Property errors : TOperationerrorerrors Index 0 Read Ferrors Write Seterrors;
  end;
  TOperationerrorClass = Class of TOperationerror;
  
  { --------------------------------------------------------------------
    TOperationerrorerrors
    --------------------------------------------------------------------}
  
  TOperationerrorerrors = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Flocation : string;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : string); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property location : string Index 8 Read Flocation Write Setlocation;
    Property message : string Index 16 Read Fmessage Write Setmessage;
  end;
  TOperationerrorerrorsClass = Class of TOperationerrorerrors;
  
  { --------------------------------------------------------------------
    TOperationwarnings
    --------------------------------------------------------------------}
  
  TOperationwarnings = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Fdata : TOperationwarningsdata;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setdata(AIndex : Integer; AValue : TOperationwarningsdata); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property data : TOperationwarningsdata Index 8 Read Fdata Write Setdata;
    Property message : string Index 16 Read Fmessage Write Setmessage;
  end;
  TOperationwarningsClass = Class of TOperationwarnings;
  
  { --------------------------------------------------------------------
    TOperationwarningsdata
    --------------------------------------------------------------------}
  
  TOperationwarningsdata = Class(TGoogleBaseObject)
  Private
    Fkey : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property key : string Index 0 Read Fkey Write Setkey;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TOperationwarningsdataClass = Class of TOperationwarningsdata;
  
  { --------------------------------------------------------------------
    TOperationList
    --------------------------------------------------------------------}
  
  TOperationList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TOperationListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TOperationListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TOperationListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TOperationListClass = Class of TOperationList;
  
  { --------------------------------------------------------------------
    TOperationListitems
    --------------------------------------------------------------------}
  
  TOperationListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TOperationListitemsClass = Class of TOperationListitems;
  
  { --------------------------------------------------------------------
    TPublicKey
    --------------------------------------------------------------------}
  
  TPublicKey = Class(TGoogleBaseObject)
  Private
    FcreationTimestamp : string;
    Fdescription : string;
    FexpirationTimestamp : string;
    Ffingerprint : string;
    Fkey : string;
  Protected
    //Property setters
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetexpirationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : string); virtual;
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property creationTimestamp : string Index 0 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : string Index 8 Read Fdescription Write Setdescription;
    Property expirationTimestamp : string Index 16 Read FexpirationTimestamp Write SetexpirationTimestamp;
    Property fingerprint : string Index 24 Read Ffingerprint Write Setfingerprint;
    Property key : string Index 32 Read Fkey Write Setkey;
  end;
  TPublicKeyClass = Class of TPublicKey;
  
  { --------------------------------------------------------------------
    TUser
    --------------------------------------------------------------------}
  
  TUser = Class(TGoogleBaseObject)
  Private
    FcreationTimestamp : string;
    Fdescription : string;
    Fgroups : TUsergroups;
    Fid : string;
    Fkind : string;
    Fname : string;
    Fowner : string;
    FpublicKeys : TUserpublicKeys;
    FselfLink : string;
  Protected
    //Property setters
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setgroups(AIndex : Integer; AValue : TUsergroups); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setowner(AIndex : Integer; AValue : string); virtual;
    Procedure SetpublicKeys(AIndex : Integer; AValue : TUserpublicKeys); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property creationTimestamp : string Index 0 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : string Index 8 Read Fdescription Write Setdescription;
    Property groups : TUsergroups Index 16 Read Fgroups Write Setgroups;
    Property id : string Index 24 Read Fid Write Setid;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property name : string Index 40 Read Fname Write Setname;
    Property owner : string Index 48 Read Fowner Write Setowner;
    Property publicKeys : TUserpublicKeys Index 56 Read FpublicKeys Write SetpublicKeys;
    Property selfLink : string Index 64 Read FselfLink Write SetselfLink;
  end;
  TUserClass = Class of TUser;
  
  { --------------------------------------------------------------------
    TUsergroups
    --------------------------------------------------------------------}
  
  TUsergroups = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TUsergroupsClass = Class of TUsergroups;
  
  { --------------------------------------------------------------------
    TUserpublicKeys
    --------------------------------------------------------------------}
  
  TUserpublicKeys = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TUserpublicKeysClass = Class of TUserpublicKeys;
  
  { --------------------------------------------------------------------
    TUserList
    --------------------------------------------------------------------}
  
  TUserList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TUserListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TUserListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TUserListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TUserListClass = Class of TUserList;
  
  { --------------------------------------------------------------------
    TUserListitems
    --------------------------------------------------------------------}
  
  TUserListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TUserListitemsClass = Class of TUserListitems;
  
  { --------------------------------------------------------------------
    TGlobalAccountsOperationsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TGlobalAccountsOperationsResource, method List
  
  TGlobalAccountsOperationsListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
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
    filter : string;
    maxResults : integer;
    pageToken : string;
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
    instance : string;
  end;
  
  
  //Optional query Options for TLinuxResource, method GetLinuxAccountViews
  
  TLinuxGetLinuxAccountViewsOptions = Record
    filter : string;
    instance : string;
    maxResults : integer;
    pageToken : string;
    user : string;
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
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  
  //Optional query Options for TUsersResource, method RemovePublicKey
  
  TUsersRemovePublicKeyOptions = Record
    fingerprint : string;
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


Procedure TAuthorizedKeysView.Setkeys(AIndex : Integer; AValue : TAuthorizedKeysViewkeys); 

begin
  If (Fkeys=AValue) then exit;
  Fkeys:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAuthorizedKeysViewkeys
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGroup
  --------------------------------------------------------------------}


Procedure TGroup.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.Setmembers(AIndex : Integer; AValue : TGroupmembers); 

begin
  If (Fmembers=AValue) then exit;
  Fmembers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroup.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGroupmembers
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGroupList
  --------------------------------------------------------------------}


Procedure TGroupList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupList.Setitems(AIndex : Integer; AValue : TGroupListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGroupListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGroupsAddMemberRequest
  --------------------------------------------------------------------}


Procedure TGroupsAddMemberRequest.Setusers(AIndex : Integer; AValue : TGroupsAddMemberRequestusers); 

begin
  If (Fusers=AValue) then exit;
  Fusers:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGroupsAddMemberRequestusers
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGroupsRemoveMemberRequest
  --------------------------------------------------------------------}


Procedure TGroupsRemoveMemberRequest.Setusers(AIndex : Integer; AValue : TGroupsRemoveMemberRequestusers); 

begin
  If (Fusers=AValue) then exit;
  Fusers:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGroupsRemoveMemberRequestusers
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLinuxAccountViews
  --------------------------------------------------------------------}


Procedure TLinuxAccountViews.SetgroupViews(AIndex : Integer; AValue : TLinuxAccountViewsgroupViews); 

begin
  If (FgroupViews=AValue) then exit;
  FgroupViews:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinuxAccountViews.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinuxAccountViews.SetuserViews(AIndex : Integer; AValue : TLinuxAccountViewsuserViews); 

begin
  If (FuserViews=AValue) then exit;
  FuserViews:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLinuxAccountViewsgroupViews
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLinuxAccountViewsuserViews
  --------------------------------------------------------------------}




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



Procedure TLinuxGroupView.SetgroupName(AIndex : Integer; AValue : string); 

begin
  If (FgroupName=AValue) then exit;
  FgroupName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinuxGroupView.Setmembers(AIndex : Integer; AValue : TLinuxGroupViewmembers); 

begin
  If (Fmembers=AValue) then exit;
  Fmembers:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLinuxGroupViewmembers
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLinuxUserView
  --------------------------------------------------------------------}


Procedure TLinuxUserView.Setgecos(AIndex : Integer; AValue : string); 

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



Procedure TLinuxUserView.SethomeDirectory(AIndex : Integer; AValue : string); 

begin
  If (FhomeDirectory=AValue) then exit;
  FhomeDirectory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinuxUserView.Setshell(AIndex : Integer; AValue : string); 

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



Procedure TLinuxUserView.Setusername(AIndex : Integer; AValue : string); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperation
  --------------------------------------------------------------------}


Procedure TOperation.SetclientOperationId(AIndex : Integer; AValue : string); 

begin
  If (FclientOperationId=AValue) then exit;
  FclientOperationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetendTime(AIndex : Integer; AValue : string); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Seterror(AIndex : Integer; AValue : TOperationerror); 

begin
  If (Ferror=AValue) then exit;
  Ferror:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SethttpErrorMessage(AIndex : Integer; AValue : string); 

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



Procedure TOperation.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetinsertTime(AIndex : Integer; AValue : string); 

begin
  If (FinsertTime=AValue) then exit;
  FinsertTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetoperationType(AIndex : Integer; AValue : string); 

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



Procedure TOperation.Setregion(AIndex : Integer; AValue : string); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetstartTime(AIndex : Integer; AValue : string); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetstatusMessage(AIndex : Integer; AValue : string); 

begin
  If (FstatusMessage=AValue) then exit;
  FstatusMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SettargetId(AIndex : Integer; AValue : string); 

begin
  If (FtargetId=AValue) then exit;
  FtargetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SettargetLink(AIndex : Integer; AValue : string); 

begin
  If (FtargetLink=AValue) then exit;
  FtargetLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setuser(AIndex : Integer; AValue : string); 

begin
  If (Fuser=AValue) then exit;
  Fuser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setwarnings(AIndex : Integer; AValue : TOperationwarnings); 

begin
  If (Fwarnings=AValue) then exit;
  Fwarnings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setzone(AIndex : Integer; AValue : string); 

begin
  If (Fzone=AValue) then exit;
  Fzone:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationerror
  --------------------------------------------------------------------}


Procedure TOperationerror.Seterrors(AIndex : Integer; AValue : TOperationerrorerrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationerrorerrors
  --------------------------------------------------------------------}


Procedure TOperationerrorerrors.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationerrorerrors.Setlocation(AIndex : Integer; AValue : string); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationerrorerrors.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationwarnings
  --------------------------------------------------------------------}


Procedure TOperationwarnings.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationwarnings.Setdata(AIndex : Integer; AValue : TOperationwarningsdata); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationwarnings.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationwarningsdata
  --------------------------------------------------------------------}


Procedure TOperationwarningsdata.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationwarningsdata.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationList
  --------------------------------------------------------------------}


Procedure TOperationList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.Setitems(AIndex : Integer; AValue : TOperationListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPublicKey
  --------------------------------------------------------------------}


Procedure TPublicKey.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublicKey.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublicKey.SetexpirationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FexpirationTimestamp=AValue) then exit;
  FexpirationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublicKey.Setfingerprint(AIndex : Integer; AValue : string); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublicKey.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUser
  --------------------------------------------------------------------}


Procedure TUser.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setgroups(AIndex : Integer; AValue : TUsergroups); 

begin
  If (Fgroups=AValue) then exit;
  Fgroups:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setowner(AIndex : Integer; AValue : string); 

begin
  If (Fowner=AValue) then exit;
  Fowner:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.SetpublicKeys(AIndex : Integer; AValue : TUserpublicKeys); 

begin
  If (FpublicKeys=AValue) then exit;
  FpublicKeys:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUsergroups
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TUserpublicKeys
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TUserList
  --------------------------------------------------------------------}


Procedure TUserList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserList.Setitems(AIndex : Integer; AValue : TUserListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUserListitems
  --------------------------------------------------------------------}




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
  Result:='20150326';
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
  Result[1].Description:='New Service: https://www.googleapis.com/auth/computeaccounts';
  Result[2].Name:='https://www.googleapis.com/auth/computeaccounts.readonly';
  Result[2].Description:='New Service: https://www.googleapis.com/auth/computeaccounts.readonly';
  
end;

Class Function TComputeaccountsAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TComputeaccountsAPI.RegisterAPIResources;

begin
  TAuthorizedKeysView.RegisterObject;
  TAuthorizedKeysViewkeys.RegisterObject;
  TGroup.RegisterObject;
  TGroupmembers.RegisterObject;
  TGroupList.RegisterObject;
  TGroupListitems.RegisterObject;
  TGroupsAddMemberRequest.RegisterObject;
  TGroupsAddMemberRequestusers.RegisterObject;
  TGroupsRemoveMemberRequest.RegisterObject;
  TGroupsRemoveMemberRequestusers.RegisterObject;
  TLinuxAccountViews.RegisterObject;
  TLinuxAccountViewsgroupViews.RegisterObject;
  TLinuxAccountViewsuserViews.RegisterObject;
  TLinuxGetAuthorizedKeysViewResponse.RegisterObject;
  TLinuxGetLinuxAccountViewsResponse.RegisterObject;
  TLinuxGroupView.RegisterObject;
  TLinuxGroupViewmembers.RegisterObject;
  TLinuxUserView.RegisterObject;
  TOperation.RegisterObject;
  TOperationerror.RegisterObject;
  TOperationerrorerrors.RegisterObject;
  TOperationwarnings.RegisterObject;
  TOperationwarningsdata.RegisterObject;
  TOperationList.RegisterObject;
  TOperationListitems.RegisterObject;
  TPublicKey.RegisterObject;
  TUser.RegisterObject;
  TUsergroups.RegisterObject;
  TUserpublicKeys.RegisterObject;
  TUserList.RegisterObject;
  TUserListitems.RegisterObject;
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
