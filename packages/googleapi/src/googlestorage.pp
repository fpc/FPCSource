unit googlestorage;
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
  TBucket = class;
  TBucketArray = Array of TBucket;
  TBucketacl = class;
  TBucketaclArray = Array of TBucketacl;
  TBucketcors = class;
  TBucketcorsArray = Array of TBucketcors;
  TBucketcorsmethod = class;
  TBucketcorsmethodArray = Array of TBucketcorsmethod;
  TBucketcorsorigin = class;
  TBucketcorsoriginArray = Array of TBucketcorsorigin;
  TBucketcorsresponseHeader = class;
  TBucketcorsresponseHeaderArray = Array of TBucketcorsresponseHeader;
  TBucketdefaultObjectAcl = class;
  TBucketdefaultObjectAclArray = Array of TBucketdefaultObjectAcl;
  TBucketlifecycle = class;
  TBucketlifecycleArray = Array of TBucketlifecycle;
  TBucketlifecyclerule = class;
  TBucketlifecycleruleArray = Array of TBucketlifecyclerule;
  TBucketlifecycleruleaction = class;
  TBucketlifecycleruleactionArray = Array of TBucketlifecycleruleaction;
  TBucketlifecyclerulecondition = class;
  TBucketlifecycleruleconditionArray = Array of TBucketlifecyclerulecondition;
  TBucketlogging = class;
  TBucketloggingArray = Array of TBucketlogging;
  TBucketowner = class;
  TBucketownerArray = Array of TBucketowner;
  TBucketversioning = class;
  TBucketversioningArray = Array of TBucketversioning;
  TBucketwebsite = class;
  TBucketwebsiteArray = Array of TBucketwebsite;
  TBucketAccessControl = class;
  TBucketAccessControlArray = Array of TBucketAccessControl;
  TBucketAccessControlprojectTeam = class;
  TBucketAccessControlprojectTeamArray = Array of TBucketAccessControlprojectTeam;
  TBucketAccessControls = class;
  TBucketAccessControlsArray = Array of TBucketAccessControls;
  TBucketAccessControlsitems = class;
  TBucketAccessControlsitemsArray = Array of TBucketAccessControlsitems;
  TBuckets = class;
  TBucketsArray = Array of TBuckets;
  TBucketsitems = class;
  TBucketsitemsArray = Array of TBucketsitems;
  TChannel = class;
  TChannelArray = Array of TChannel;
  TChannelparams = class;
  TChannelparamsArray = Array of TChannelparams;
  TComposeRequest = class;
  TComposeRequestArray = Array of TComposeRequest;
  TComposeRequestsourceObjects = class;
  TComposeRequestsourceObjectsArray = Array of TComposeRequestsourceObjects;
  TComposeRequestsourceObjectsobjectPreconditions = class;
  TComposeRequestsourceObjectsobjectPreconditionsArray = Array of TComposeRequestsourceObjectsobjectPreconditions;
  TObject = class;
  TObjectArray = Array of TObject;
  TObjectacl = class;
  TObjectaclArray = Array of TObjectacl;
  TObjectmetadata = class;
  TObjectmetadataArray = Array of TObjectmetadata;
  TObjectowner = class;
  TObjectownerArray = Array of TObjectowner;
  TObjectAccessControl = class;
  TObjectAccessControlArray = Array of TObjectAccessControl;
  TObjectAccessControlprojectTeam = class;
  TObjectAccessControlprojectTeamArray = Array of TObjectAccessControlprojectTeam;
  TObjectAccessControls = class;
  TObjectAccessControlsArray = Array of TObjectAccessControls;
  TObjectAccessControlsitems = class;
  TObjectAccessControlsitemsArray = Array of TObjectAccessControlsitems;
  TObjects = class;
  TObjectsArray = Array of TObjects;
  TObjectsitems = class;
  TObjectsitemsArray = Array of TObjectsitems;
  TObjectsprefixes = class;
  TObjectsprefixesArray = Array of TObjectsprefixes;
  TRewriteResponse = class;
  TRewriteResponseArray = Array of TRewriteResponse;
  
  { --------------------------------------------------------------------
    TBucket
    --------------------------------------------------------------------}
  
  TBucket = Class(TGoogleBaseObject)
  Private
    Facl : TBucketacl;
    Fcors : TBucketcors;
    FdefaultObjectAcl : TBucketdefaultObjectAcl;
    Fetag : string;
    Fid : string;
    Fkind : string;
    Flifecycle : TBucketlifecycle;
    Flocation : string;
    Flogging : TBucketlogging;
    Fmetageneration : string;
    Fname : string;
    Fowner : TBucketowner;
    FprojectNumber : string;
    FselfLink : string;
    FstorageClass : string;
    FtimeCreated : TDatetime;
    Fversioning : TBucketversioning;
    Fwebsite : TBucketwebsite;
  Protected
    //Property setters
    Procedure Setacl(AIndex : Integer; AValue : TBucketacl); virtual;
    Procedure Setcors(AIndex : Integer; AValue : TBucketcors); virtual;
    Procedure SetdefaultObjectAcl(AIndex : Integer; AValue : TBucketdefaultObjectAcl); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlifecycle(AIndex : Integer; AValue : TBucketlifecycle); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : string); virtual;
    Procedure Setlogging(AIndex : Integer; AValue : TBucketlogging); virtual;
    Procedure Setmetageneration(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setowner(AIndex : Integer; AValue : TBucketowner); virtual;
    Procedure SetprojectNumber(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetstorageClass(AIndex : Integer; AValue : string); virtual;
    Procedure SettimeCreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setversioning(AIndex : Integer; AValue : TBucketversioning); virtual;
    Procedure Setwebsite(AIndex : Integer; AValue : TBucketwebsite); virtual;
  Public
  Published
    Property acl : TBucketacl Index 0 Read Facl Write Setacl;
    Property cors : TBucketcors Index 8 Read Fcors Write Setcors;
    Property defaultObjectAcl : TBucketdefaultObjectAcl Index 16 Read FdefaultObjectAcl Write SetdefaultObjectAcl;
    Property etag : string Index 24 Read Fetag Write Setetag;
    Property id : string Index 32 Read Fid Write Setid;
    Property kind : string Index 40 Read Fkind Write Setkind;
    Property lifecycle : TBucketlifecycle Index 48 Read Flifecycle Write Setlifecycle;
    Property location : string Index 56 Read Flocation Write Setlocation;
    Property logging : TBucketlogging Index 64 Read Flogging Write Setlogging;
    Property metageneration : string Index 72 Read Fmetageneration Write Setmetageneration;
    Property name : string Index 80 Read Fname Write Setname;
    Property owner : TBucketowner Index 88 Read Fowner Write Setowner;
    Property projectNumber : string Index 96 Read FprojectNumber Write SetprojectNumber;
    Property selfLink : string Index 104 Read FselfLink Write SetselfLink;
    Property storageClass : string Index 112 Read FstorageClass Write SetstorageClass;
    Property timeCreated : TDatetime Index 120 Read FtimeCreated Write SettimeCreated;
    Property versioning : TBucketversioning Index 128 Read Fversioning Write Setversioning;
    Property website : TBucketwebsite Index 136 Read Fwebsite Write Setwebsite;
  end;
  TBucketClass = Class of TBucket;
  
  { --------------------------------------------------------------------
    TBucketacl
    --------------------------------------------------------------------}
  
  TBucketacl = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TBucketaclClass = Class of TBucketacl;
  
  { --------------------------------------------------------------------
    TBucketcors
    --------------------------------------------------------------------}
  
  TBucketcors = Class(TGoogleBaseObject)
  Private
    FmaxAgeSeconds : integer;
    Fmethod : TBucketcorsmethod;
    Forigin : TBucketcorsorigin;
    FresponseHeader : TBucketcorsresponseHeader;
  Protected
    //Property setters
    Procedure SetmaxAgeSeconds(AIndex : Integer; AValue : integer); virtual;
    Procedure Setmethod(AIndex : Integer; AValue : TBucketcorsmethod); virtual;
    Procedure Setorigin(AIndex : Integer; AValue : TBucketcorsorigin); virtual;
    Procedure SetresponseHeader(AIndex : Integer; AValue : TBucketcorsresponseHeader); virtual;
  Public
  Published
    Property maxAgeSeconds : integer Index 0 Read FmaxAgeSeconds Write SetmaxAgeSeconds;
    Property method : TBucketcorsmethod Index 8 Read Fmethod Write Setmethod;
    Property origin : TBucketcorsorigin Index 16 Read Forigin Write Setorigin;
    Property responseHeader : TBucketcorsresponseHeader Index 24 Read FresponseHeader Write SetresponseHeader;
  end;
  TBucketcorsClass = Class of TBucketcors;
  
  { --------------------------------------------------------------------
    TBucketcorsmethod
    --------------------------------------------------------------------}
  
  TBucketcorsmethod = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TBucketcorsmethodClass = Class of TBucketcorsmethod;
  
  { --------------------------------------------------------------------
    TBucketcorsorigin
    --------------------------------------------------------------------}
  
  TBucketcorsorigin = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TBucketcorsoriginClass = Class of TBucketcorsorigin;
  
  { --------------------------------------------------------------------
    TBucketcorsresponseHeader
    --------------------------------------------------------------------}
  
  TBucketcorsresponseHeader = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TBucketcorsresponseHeaderClass = Class of TBucketcorsresponseHeader;
  
  { --------------------------------------------------------------------
    TBucketdefaultObjectAcl
    --------------------------------------------------------------------}
  
  TBucketdefaultObjectAcl = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TBucketdefaultObjectAclClass = Class of TBucketdefaultObjectAcl;
  
  { --------------------------------------------------------------------
    TBucketlifecycle
    --------------------------------------------------------------------}
  
  TBucketlifecycle = Class(TGoogleBaseObject)
  Private
    Frule : TBucketlifecyclerule;
  Protected
    //Property setters
    Procedure Setrule(AIndex : Integer; AValue : TBucketlifecyclerule); virtual;
  Public
  Published
    Property rule : TBucketlifecyclerule Index 0 Read Frule Write Setrule;
  end;
  TBucketlifecycleClass = Class of TBucketlifecycle;
  
  { --------------------------------------------------------------------
    TBucketlifecyclerule
    --------------------------------------------------------------------}
  
  TBucketlifecyclerule = Class(TGoogleBaseObject)
  Private
    Faction : TBucketlifecycleruleaction;
    Fcondition : TBucketlifecyclerulecondition;
  Protected
    //Property setters
    Procedure Setaction(AIndex : Integer; AValue : TBucketlifecycleruleaction); virtual;
    Procedure Setcondition(AIndex : Integer; AValue : TBucketlifecyclerulecondition); virtual;
  Public
  Published
    Property action : TBucketlifecycleruleaction Index 0 Read Faction Write Setaction;
    Property condition : TBucketlifecyclerulecondition Index 8 Read Fcondition Write Setcondition;
  end;
  TBucketlifecycleruleClass = Class of TBucketlifecyclerule;
  
  { --------------------------------------------------------------------
    TBucketlifecycleruleaction
    --------------------------------------------------------------------}
  
  TBucketlifecycleruleaction = Class(TGoogleBaseObject)
  Private
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property _type : string Index 0 Read F_type Write Set_type;
  end;
  TBucketlifecycleruleactionClass = Class of TBucketlifecycleruleaction;
  
  { --------------------------------------------------------------------
    TBucketlifecyclerulecondition
    --------------------------------------------------------------------}
  
  TBucketlifecyclerulecondition = Class(TGoogleBaseObject)
  Private
    Fage : integer;
    FcreatedBefore : TDate;
    FisLive : boolean;
    FnumNewerVersions : integer;
  Protected
    //Property setters
    Procedure Setage(AIndex : Integer; AValue : integer); virtual;
    Procedure SetcreatedBefore(AIndex : Integer; AValue : TDate); virtual;
    Procedure SetisLive(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetnumNewerVersions(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property age : integer Index 0 Read Fage Write Setage;
    Property createdBefore : TDate Index 8 Read FcreatedBefore Write SetcreatedBefore;
    Property isLive : boolean Index 16 Read FisLive Write SetisLive;
    Property numNewerVersions : integer Index 24 Read FnumNewerVersions Write SetnumNewerVersions;
  end;
  TBucketlifecycleruleconditionClass = Class of TBucketlifecyclerulecondition;
  
  { --------------------------------------------------------------------
    TBucketlogging
    --------------------------------------------------------------------}
  
  TBucketlogging = Class(TGoogleBaseObject)
  Private
    FlogBucket : string;
    FlogObjectPrefix : string;
  Protected
    //Property setters
    Procedure SetlogBucket(AIndex : Integer; AValue : string); virtual;
    Procedure SetlogObjectPrefix(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property logBucket : string Index 0 Read FlogBucket Write SetlogBucket;
    Property logObjectPrefix : string Index 8 Read FlogObjectPrefix Write SetlogObjectPrefix;
  end;
  TBucketloggingClass = Class of TBucketlogging;
  
  { --------------------------------------------------------------------
    TBucketowner
    --------------------------------------------------------------------}
  
  TBucketowner = Class(TGoogleBaseObject)
  Private
    Fentity : string;
    FentityId : string;
  Protected
    //Property setters
    Procedure Setentity(AIndex : Integer; AValue : string); virtual;
    Procedure SetentityId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property entity : string Index 0 Read Fentity Write Setentity;
    Property entityId : string Index 8 Read FentityId Write SetentityId;
  end;
  TBucketownerClass = Class of TBucketowner;
  
  { --------------------------------------------------------------------
    TBucketversioning
    --------------------------------------------------------------------}
  
  TBucketversioning = Class(TGoogleBaseObject)
  Private
    Fenabled : boolean;
  Protected
    //Property setters
    Procedure Setenabled(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property enabled : boolean Index 0 Read Fenabled Write Setenabled;
  end;
  TBucketversioningClass = Class of TBucketversioning;
  
  { --------------------------------------------------------------------
    TBucketwebsite
    --------------------------------------------------------------------}
  
  TBucketwebsite = Class(TGoogleBaseObject)
  Private
    FmainPageSuffix : string;
    FnotFoundPage : string;
  Protected
    //Property setters
    Procedure SetmainPageSuffix(AIndex : Integer; AValue : string); virtual;
    Procedure SetnotFoundPage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property mainPageSuffix : string Index 0 Read FmainPageSuffix Write SetmainPageSuffix;
    Property notFoundPage : string Index 8 Read FnotFoundPage Write SetnotFoundPage;
  end;
  TBucketwebsiteClass = Class of TBucketwebsite;
  
  { --------------------------------------------------------------------
    TBucketAccessControl
    --------------------------------------------------------------------}
  
  TBucketAccessControl = Class(TGoogleBaseObject)
  Private
    Fbucket : string;
    Fdomain : string;
    Femail : string;
    Fentity : string;
    FentityId : string;
    Fetag : string;
    Fid : string;
    Fkind : string;
    FprojectTeam : TBucketAccessControlprojectTeam;
    Frole : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setbucket(AIndex : Integer; AValue : string); virtual;
    Procedure Setdomain(AIndex : Integer; AValue : string); virtual;
    Procedure Setemail(AIndex : Integer; AValue : string); virtual;
    Procedure Setentity(AIndex : Integer; AValue : string); virtual;
    Procedure SetentityId(AIndex : Integer; AValue : string); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetprojectTeam(AIndex : Integer; AValue : TBucketAccessControlprojectTeam); virtual;
    Procedure Setrole(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property bucket : string Index 0 Read Fbucket Write Setbucket;
    Property domain : string Index 8 Read Fdomain Write Setdomain;
    Property email : string Index 16 Read Femail Write Setemail;
    Property entity : string Index 24 Read Fentity Write Setentity;
    Property entityId : string Index 32 Read FentityId Write SetentityId;
    Property etag : string Index 40 Read Fetag Write Setetag;
    Property id : string Index 48 Read Fid Write Setid;
    Property kind : string Index 56 Read Fkind Write Setkind;
    Property projectTeam : TBucketAccessControlprojectTeam Index 64 Read FprojectTeam Write SetprojectTeam;
    Property role : string Index 72 Read Frole Write Setrole;
    Property selfLink : string Index 80 Read FselfLink Write SetselfLink;
  end;
  TBucketAccessControlClass = Class of TBucketAccessControl;
  
  { --------------------------------------------------------------------
    TBucketAccessControlprojectTeam
    --------------------------------------------------------------------}
  
  TBucketAccessControlprojectTeam = Class(TGoogleBaseObject)
  Private
    FprojectNumber : string;
    Fteam : string;
  Protected
    //Property setters
    Procedure SetprojectNumber(AIndex : Integer; AValue : string); virtual;
    Procedure Setteam(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property projectNumber : string Index 0 Read FprojectNumber Write SetprojectNumber;
    Property team : string Index 8 Read Fteam Write Setteam;
  end;
  TBucketAccessControlprojectTeamClass = Class of TBucketAccessControlprojectTeam;
  
  { --------------------------------------------------------------------
    TBucketAccessControls
    --------------------------------------------------------------------}
  
  TBucketAccessControls = Class(TGoogleBaseObject)
  Private
    Fitems : TBucketAccessControlsitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TBucketAccessControlsitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TBucketAccessControlsitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TBucketAccessControlsClass = Class of TBucketAccessControls;
  
  { --------------------------------------------------------------------
    TBucketAccessControlsitems
    --------------------------------------------------------------------}
  
  TBucketAccessControlsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TBucketAccessControlsitemsClass = Class of TBucketAccessControlsitems;
  
  { --------------------------------------------------------------------
    TBuckets
    --------------------------------------------------------------------}
  
  TBuckets = Class(TGoogleBaseObject)
  Private
    Fitems : TBucketsitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TBucketsitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TBucketsitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TBucketsClass = Class of TBuckets;
  
  { --------------------------------------------------------------------
    TBucketsitems
    --------------------------------------------------------------------}
  
  TBucketsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TBucketsitemsClass = Class of TBucketsitems;
  
  { --------------------------------------------------------------------
    TChannel
    --------------------------------------------------------------------}
  
  TChannel = Class(TGoogleBaseObject)
  Private
    Faddress : string;
    Fexpiration : string;
    Fid : string;
    Fkind : string;
    Fparams : TChannelparams;
    Fpayload : boolean;
    FresourceId : string;
    FresourceUri : string;
    Ftoken : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setaddress(AIndex : Integer; AValue : string); virtual;
    Procedure Setexpiration(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setparams(AIndex : Integer; AValue : TChannelparams); virtual;
    Procedure Setpayload(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetresourceId(AIndex : Integer; AValue : string); virtual;
    Procedure SetresourceUri(AIndex : Integer; AValue : string); virtual;
    Procedure Settoken(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property address : string Index 0 Read Faddress Write Setaddress;
    Property expiration : string Index 8 Read Fexpiration Write Setexpiration;
    Property id : string Index 16 Read Fid Write Setid;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property params : TChannelparams Index 32 Read Fparams Write Setparams;
    Property payload : boolean Index 40 Read Fpayload Write Setpayload;
    Property resourceId : string Index 48 Read FresourceId Write SetresourceId;
    Property resourceUri : string Index 56 Read FresourceUri Write SetresourceUri;
    Property token : string Index 64 Read Ftoken Write Settoken;
    Property _type : string Index 72 Read F_type Write Set_type;
  end;
  TChannelClass = Class of TChannel;
  
  { --------------------------------------------------------------------
    TChannelparams
    --------------------------------------------------------------------}
  
  TChannelparams = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TChannelparamsClass = Class of TChannelparams;
  
  { --------------------------------------------------------------------
    TComposeRequest
    --------------------------------------------------------------------}
  
  TComposeRequest = Class(TGoogleBaseObject)
  Private
    Fdestination : TObject;
    Fkind : string;
    FsourceObjects : TComposeRequestsourceObjects;
  Protected
    //Property setters
    Procedure Setdestination(AIndex : Integer; AValue : TObject); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetsourceObjects(AIndex : Integer; AValue : TComposeRequestsourceObjects); virtual;
  Public
  Published
    Property destination : TObject Index 0 Read Fdestination Write Setdestination;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property sourceObjects : TComposeRequestsourceObjects Index 16 Read FsourceObjects Write SetsourceObjects;
  end;
  TComposeRequestClass = Class of TComposeRequest;
  
  { --------------------------------------------------------------------
    TComposeRequestsourceObjects
    --------------------------------------------------------------------}
  
  TComposeRequestsourceObjects = Class(TGoogleBaseObject)
  Private
    Fgeneration : string;
    Fname : string;
    FobjectPreconditions : TComposeRequestsourceObjectsobjectPreconditions;
  Protected
    //Property setters
    Procedure Setgeneration(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetobjectPreconditions(AIndex : Integer; AValue : TComposeRequestsourceObjectsobjectPreconditions); virtual;
  Public
  Published
    Property generation : string Index 0 Read Fgeneration Write Setgeneration;
    Property name : string Index 8 Read Fname Write Setname;
    Property objectPreconditions : TComposeRequestsourceObjectsobjectPreconditions Index 16 Read FobjectPreconditions Write SetobjectPreconditions;
  end;
  TComposeRequestsourceObjectsClass = Class of TComposeRequestsourceObjects;
  
  { --------------------------------------------------------------------
    TComposeRequestsourceObjectsobjectPreconditions
    --------------------------------------------------------------------}
  
  TComposeRequestsourceObjectsobjectPreconditions = Class(TGoogleBaseObject)
  Private
    FifGenerationMatch : string;
  Protected
    //Property setters
    Procedure SetifGenerationMatch(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property ifGenerationMatch : string Index 0 Read FifGenerationMatch Write SetifGenerationMatch;
  end;
  TComposeRequestsourceObjectsobjectPreconditionsClass = Class of TComposeRequestsourceObjectsobjectPreconditions;
  
  { --------------------------------------------------------------------
    TObject
    --------------------------------------------------------------------}
  
  TObject = Class(TGoogleBaseObject)
  Private
    Facl : TObjectacl;
    Fbucket : string;
    FcacheControl : string;
    FcomponentCount : integer;
    FcontentDisposition : string;
    FcontentEncoding : string;
    FcontentLanguage : string;
    FcontentType : string;
    Fcrc32c : string;
    Fetag : string;
    Fgeneration : string;
    Fid : string;
    Fkind : string;
    Fmd5Hash : string;
    FmediaLink : string;
    Fmetadata : TObjectmetadata;
    Fmetageneration : string;
    Fname : string;
    Fowner : TObjectowner;
    FselfLink : string;
    Fsize : string;
    FstorageClass : string;
    FtimeDeleted : TDatetime;
    Fupdated : TDatetime;
  Protected
    //Property setters
    Procedure Setacl(AIndex : Integer; AValue : TObjectacl); virtual;
    Procedure Setbucket(AIndex : Integer; AValue : string); virtual;
    Procedure SetcacheControl(AIndex : Integer; AValue : string); virtual;
    Procedure SetcomponentCount(AIndex : Integer; AValue : integer); virtual;
    Procedure SetcontentDisposition(AIndex : Integer; AValue : string); virtual;
    Procedure SetcontentEncoding(AIndex : Integer; AValue : string); virtual;
    Procedure SetcontentLanguage(AIndex : Integer; AValue : string); virtual;
    Procedure SetcontentType(AIndex : Integer; AValue : string); virtual;
    Procedure Setcrc32c(AIndex : Integer; AValue : string); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setgeneration(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setmd5Hash(AIndex : Integer; AValue : string); virtual;
    Procedure SetmediaLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setmetadata(AIndex : Integer; AValue : TObjectmetadata); virtual;
    Procedure Setmetageneration(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setowner(AIndex : Integer; AValue : TObjectowner); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setsize(AIndex : Integer; AValue : string); virtual;
    Procedure SetstorageClass(AIndex : Integer; AValue : string); virtual;
    Procedure SettimeDeleted(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property acl : TObjectacl Index 0 Read Facl Write Setacl;
    Property bucket : string Index 8 Read Fbucket Write Setbucket;
    Property cacheControl : string Index 16 Read FcacheControl Write SetcacheControl;
    Property componentCount : integer Index 24 Read FcomponentCount Write SetcomponentCount;
    Property contentDisposition : string Index 32 Read FcontentDisposition Write SetcontentDisposition;
    Property contentEncoding : string Index 40 Read FcontentEncoding Write SetcontentEncoding;
    Property contentLanguage : string Index 48 Read FcontentLanguage Write SetcontentLanguage;
    Property contentType : string Index 56 Read FcontentType Write SetcontentType;
    Property crc32c : string Index 64 Read Fcrc32c Write Setcrc32c;
    Property etag : string Index 72 Read Fetag Write Setetag;
    Property generation : string Index 80 Read Fgeneration Write Setgeneration;
    Property id : string Index 88 Read Fid Write Setid;
    Property kind : string Index 96 Read Fkind Write Setkind;
    Property md5Hash : string Index 104 Read Fmd5Hash Write Setmd5Hash;
    Property mediaLink : string Index 112 Read FmediaLink Write SetmediaLink;
    Property metadata : TObjectmetadata Index 120 Read Fmetadata Write Setmetadata;
    Property metageneration : string Index 128 Read Fmetageneration Write Setmetageneration;
    Property name : string Index 136 Read Fname Write Setname;
    Property owner : TObjectowner Index 144 Read Fowner Write Setowner;
    Property selfLink : string Index 152 Read FselfLink Write SetselfLink;
    Property size : string Index 160 Read Fsize Write Setsize;
    Property storageClass : string Index 168 Read FstorageClass Write SetstorageClass;
    Property timeDeleted : TDatetime Index 176 Read FtimeDeleted Write SettimeDeleted;
    Property updated : TDatetime Index 184 Read Fupdated Write Setupdated;
  end;
  TObjectClass = Class of TObject;
  
  { --------------------------------------------------------------------
    TObjectacl
    --------------------------------------------------------------------}
  
  TObjectacl = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TObjectaclClass = Class of TObjectacl;
  
  { --------------------------------------------------------------------
    TObjectmetadata
    --------------------------------------------------------------------}
  
  TObjectmetadata = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TObjectmetadataClass = Class of TObjectmetadata;
  
  { --------------------------------------------------------------------
    TObjectowner
    --------------------------------------------------------------------}
  
  TObjectowner = Class(TGoogleBaseObject)
  Private
    Fentity : string;
    FentityId : string;
  Protected
    //Property setters
    Procedure Setentity(AIndex : Integer; AValue : string); virtual;
    Procedure SetentityId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property entity : string Index 0 Read Fentity Write Setentity;
    Property entityId : string Index 8 Read FentityId Write SetentityId;
  end;
  TObjectownerClass = Class of TObjectowner;
  
  { --------------------------------------------------------------------
    TObjectAccessControl
    --------------------------------------------------------------------}
  
  TObjectAccessControl = Class(TGoogleBaseObject)
  Private
    Fbucket : string;
    Fdomain : string;
    Femail : string;
    Fentity : string;
    FentityId : string;
    Fetag : string;
    Fgeneration : string;
    Fid : string;
    Fkind : string;
    F_object : string;
    FprojectTeam : TObjectAccessControlprojectTeam;
    Frole : string;
    FselfLink : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setbucket(AIndex : Integer; AValue : string); virtual;
    Procedure Setdomain(AIndex : Integer; AValue : string); virtual;
    Procedure Setemail(AIndex : Integer; AValue : string); virtual;
    Procedure Setentity(AIndex : Integer; AValue : string); virtual;
    Procedure SetentityId(AIndex : Integer; AValue : string); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setgeneration(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Set_object(AIndex : Integer; AValue : string); virtual;
    Procedure SetprojectTeam(AIndex : Integer; AValue : TObjectAccessControlprojectTeam); virtual;
    Procedure Setrole(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property bucket : string Index 0 Read Fbucket Write Setbucket;
    Property domain : string Index 8 Read Fdomain Write Setdomain;
    Property email : string Index 16 Read Femail Write Setemail;
    Property entity : string Index 24 Read Fentity Write Setentity;
    Property entityId : string Index 32 Read FentityId Write SetentityId;
    Property etag : string Index 40 Read Fetag Write Setetag;
    Property generation : string Index 48 Read Fgeneration Write Setgeneration;
    Property id : string Index 56 Read Fid Write Setid;
    Property kind : string Index 64 Read Fkind Write Setkind;
    Property _object : string Index 72 Read F_object Write Set_object;
    Property projectTeam : TObjectAccessControlprojectTeam Index 80 Read FprojectTeam Write SetprojectTeam;
    Property role : string Index 88 Read Frole Write Setrole;
    Property selfLink : string Index 96 Read FselfLink Write SetselfLink;
  end;
  TObjectAccessControlClass = Class of TObjectAccessControl;
  
  { --------------------------------------------------------------------
    TObjectAccessControlprojectTeam
    --------------------------------------------------------------------}
  
  TObjectAccessControlprojectTeam = Class(TGoogleBaseObject)
  Private
    FprojectNumber : string;
    Fteam : string;
  Protected
    //Property setters
    Procedure SetprojectNumber(AIndex : Integer; AValue : string); virtual;
    Procedure Setteam(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property projectNumber : string Index 0 Read FprojectNumber Write SetprojectNumber;
    Property team : string Index 8 Read Fteam Write Setteam;
  end;
  TObjectAccessControlprojectTeamClass = Class of TObjectAccessControlprojectTeam;
  
  { --------------------------------------------------------------------
    TObjectAccessControls
    --------------------------------------------------------------------}
  
  TObjectAccessControls = Class(TGoogleBaseObject)
  Private
    Fitems : TObjectAccessControlsitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TObjectAccessControlsitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TObjectAccessControlsitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TObjectAccessControlsClass = Class of TObjectAccessControls;
  
  { --------------------------------------------------------------------
    TObjectAccessControlsitems
    --------------------------------------------------------------------}
  
  TObjectAccessControlsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TObjectAccessControlsitemsClass = Class of TObjectAccessControlsitems;
  
  { --------------------------------------------------------------------
    TObjects
    --------------------------------------------------------------------}
  
  TObjects = Class(TGoogleBaseObject)
  Private
    Fitems : TObjectsitems;
    Fkind : string;
    FnextPageToken : string;
    Fprefixes : TObjectsprefixes;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TObjectsitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setprefixes(AIndex : Integer; AValue : TObjectsprefixes); virtual;
  Public
  Published
    Property items : TObjectsitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
    Property prefixes : TObjectsprefixes Index 24 Read Fprefixes Write Setprefixes;
  end;
  TObjectsClass = Class of TObjects;
  
  { --------------------------------------------------------------------
    TObjectsitems
    --------------------------------------------------------------------}
  
  TObjectsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TObjectsitemsClass = Class of TObjectsitems;
  
  { --------------------------------------------------------------------
    TObjectsprefixes
    --------------------------------------------------------------------}
  
  TObjectsprefixes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TObjectsprefixesClass = Class of TObjectsprefixes;
  
  { --------------------------------------------------------------------
    TRewriteResponse
    --------------------------------------------------------------------}
  
  TRewriteResponse = Class(TGoogleBaseObject)
  Private
    Fdone : boolean;
    Fkind : string;
    FobjectSize : string;
    Fresource : TObject;
    FrewriteToken : string;
    FtotalBytesRewritten : string;
  Protected
    //Property setters
    Procedure Setdone(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetobjectSize(AIndex : Integer; AValue : string); virtual;
    Procedure Setresource(AIndex : Integer; AValue : TObject); virtual;
    Procedure SetrewriteToken(AIndex : Integer; AValue : string); virtual;
    Procedure SettotalBytesRewritten(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property done : boolean Index 0 Read Fdone Write Setdone;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property objectSize : string Index 16 Read FobjectSize Write SetobjectSize;
    Property resource : TObject Index 24 Read Fresource Write Setresource;
    Property rewriteToken : string Index 32 Read FrewriteToken Write SetrewriteToken;
    Property totalBytesRewritten : string Index 40 Read FtotalBytesRewritten Write SettotalBytesRewritten;
  end;
  TRewriteResponseClass = Class of TRewriteResponse;
  
  { --------------------------------------------------------------------
    TBucketAccessControlsResource
    --------------------------------------------------------------------}
  
  TBucketAccessControlsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(bucket: string; entity: string);
    Function Get(bucket: string; entity: string) : TBucketAccessControl;
    Function Insert(bucket: string; aBucketAccessControl : TBucketAccessControl) : TBucketAccessControl;
    Function List(bucket: string) : TBucketAccessControls;
    Function Patch(bucket: string; entity: string; aBucketAccessControl : TBucketAccessControl) : TBucketAccessControl;
    Function Update(bucket: string; entity: string; aBucketAccessControl : TBucketAccessControl) : TBucketAccessControl;
  end;
  
  
  { --------------------------------------------------------------------
    TBucketsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TBucketsResource, method Delete
  
  TBucketsDeleteOptions = Record
    ifMetagenerationMatch : int64;
    ifMetagenerationNotMatch : int64;
  end;
  
  
  //Optional query Options for TBucketsResource, method Get
  
  TBucketsGetOptions = Record
    ifMetagenerationMatch : int64;
    ifMetagenerationNotMatch : int64;
    projection : string;
  end;
  
  
  //Optional query Options for TBucketsResource, method Insert
  
  TBucketsInsertOptions = Record
    predefinedAcl : string;
    predefinedDefaultObjectAcl : string;
    project : string;
    projection : string;
  end;
  
  
  //Optional query Options for TBucketsResource, method List
  
  TBucketsListOptions = Record
    maxResults : integer;
    pageToken : string;
    prefix : string;
    project : string;
    projection : string;
  end;
  
  
  //Optional query Options for TBucketsResource, method Patch
  
  TBucketsPatchOptions = Record
    ifMetagenerationMatch : int64;
    ifMetagenerationNotMatch : int64;
    predefinedAcl : string;
    predefinedDefaultObjectAcl : string;
    projection : string;
  end;
  
  
  //Optional query Options for TBucketsResource, method Update
  
  TBucketsUpdateOptions = Record
    ifMetagenerationMatch : int64;
    ifMetagenerationNotMatch : int64;
    predefinedAcl : string;
    predefinedDefaultObjectAcl : string;
    projection : string;
  end;
  
  TBucketsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(bucket: string; AQuery : string  = '');
    Procedure Delete(bucket: string; AQuery : TBucketsdeleteOptions);
    Function Get(bucket: string; AQuery : string  = '') : TBucket;
    Function Get(bucket: string; AQuery : TBucketsgetOptions) : TBucket;
    Function Insert(aBucket : TBucket; AQuery : string  = '') : TBucket;
    Function Insert(aBucket : TBucket; AQuery : TBucketsinsertOptions) : TBucket;
    Function List(AQuery : string  = '') : TBuckets;
    Function List(AQuery : TBucketslistOptions) : TBuckets;
    Function Patch(bucket: string; aBucket : TBucket; AQuery : string  = '') : TBucket;
    Function Patch(bucket: string; aBucket : TBucket; AQuery : TBucketspatchOptions) : TBucket;
    Function Update(bucket: string; aBucket : TBucket; AQuery : string  = '') : TBucket;
    Function Update(bucket: string; aBucket : TBucket; AQuery : TBucketsupdateOptions) : TBucket;
  end;
  
  
  { --------------------------------------------------------------------
    TChannelsResource
    --------------------------------------------------------------------}
  
  TChannelsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Stop(aChannel : TChannel);
  end;
  
  
  { --------------------------------------------------------------------
    TDefaultObjectAccessControlsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TDefaultObjectAccessControlsResource, method List
  
  TDefaultObjectAccessControlsListOptions = Record
    ifMetagenerationMatch : int64;
    ifMetagenerationNotMatch : int64;
  end;
  
  TDefaultObjectAccessControlsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(bucket: string; entity: string);
    Function Get(bucket: string; entity: string) : TObjectAccessControl;
    Function Insert(bucket: string; aObjectAccessControl : TObjectAccessControl) : TObjectAccessControl;
    Function List(bucket: string; AQuery : string  = '') : TObjectAccessControls;
    Function List(bucket: string; AQuery : TDefaultObjectAccessControlslistOptions) : TObjectAccessControls;
    Function Patch(bucket: string; entity: string; aObjectAccessControl : TObjectAccessControl) : TObjectAccessControl;
    Function Update(bucket: string; entity: string; aObjectAccessControl : TObjectAccessControl) : TObjectAccessControl;
  end;
  
  
  { --------------------------------------------------------------------
    TObjectAccessControlsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TObjectAccessControlsResource, method Delete
  
  TObjectAccessControlsDeleteOptions = Record
    generation : int64;
  end;
  
  
  //Optional query Options for TObjectAccessControlsResource, method Get
  
  TObjectAccessControlsGetOptions = Record
    generation : int64;
  end;
  
  
  //Optional query Options for TObjectAccessControlsResource, method Insert
  
  TObjectAccessControlsInsertOptions = Record
    generation : int64;
  end;
  
  
  //Optional query Options for TObjectAccessControlsResource, method List
  
  TObjectAccessControlsListOptions = Record
    generation : int64;
  end;
  
  
  //Optional query Options for TObjectAccessControlsResource, method Patch
  
  TObjectAccessControlsPatchOptions = Record
    generation : int64;
  end;
  
  
  //Optional query Options for TObjectAccessControlsResource, method Update
  
  TObjectAccessControlsUpdateOptions = Record
    generation : int64;
  end;
  
  TObjectAccessControlsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(bucket: string; entity: string; _object: string; AQuery : string  = '');
    Procedure Delete(bucket: string; entity: string; _object: string; AQuery : TObjectAccessControlsdeleteOptions);
    Function Get(bucket: string; entity: string; _object: string; AQuery : string  = '') : TObjectAccessControl;
    Function Get(bucket: string; entity: string; _object: string; AQuery : TObjectAccessControlsgetOptions) : TObjectAccessControl;
    Function Insert(bucket: string; _object: string; aObjectAccessControl : TObjectAccessControl; AQuery : string  = '') : TObjectAccessControl;
    Function Insert(bucket: string; _object: string; aObjectAccessControl : TObjectAccessControl; AQuery : TObjectAccessControlsinsertOptions) : TObjectAccessControl;
    Function List(bucket: string; _object: string; AQuery : string  = '') : TObjectAccessControls;
    Function List(bucket: string; _object: string; AQuery : TObjectAccessControlslistOptions) : TObjectAccessControls;
    Function Patch(bucket: string; entity: string; _object: string; aObjectAccessControl : TObjectAccessControl; AQuery : string  = '') : TObjectAccessControl;
    Function Patch(bucket: string; entity: string; _object: string; aObjectAccessControl : TObjectAccessControl; AQuery : TObjectAccessControlspatchOptions) : TObjectAccessControl;
    Function Update(bucket: string; entity: string; _object: string; aObjectAccessControl : TObjectAccessControl; AQuery : string  = '') : TObjectAccessControl;
    Function Update(bucket: string; entity: string; _object: string; aObjectAccessControl : TObjectAccessControl; AQuery : TObjectAccessControlsupdateOptions) : TObjectAccessControl;
  end;
  
  
  { --------------------------------------------------------------------
    TObjectsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TObjectsResource, method Compose
  
  TObjectsComposeOptions = Record
    destinationPredefinedAcl : string;
    ifGenerationMatch : int64;
    ifMetagenerationMatch : int64;
  end;
  
  
  //Optional query Options for TObjectsResource, method Copy
  
  TObjectsCopyOptions = Record
    destinationPredefinedAcl : string;
    ifGenerationMatch : int64;
    ifGenerationNotMatch : int64;
    ifMetagenerationMatch : int64;
    ifMetagenerationNotMatch : int64;
    ifSourceGenerationMatch : int64;
    ifSourceGenerationNotMatch : int64;
    ifSourceMetagenerationMatch : int64;
    ifSourceMetagenerationNotMatch : int64;
    projection : string;
    sourceGeneration : int64;
  end;
  
  
  //Optional query Options for TObjectsResource, method Delete
  
  TObjectsDeleteOptions = Record
    generation : int64;
    ifGenerationMatch : int64;
    ifGenerationNotMatch : int64;
    ifMetagenerationMatch : int64;
    ifMetagenerationNotMatch : int64;
  end;
  
  
  //Optional query Options for TObjectsResource, method Get
  
  TObjectsGetOptions = Record
    generation : int64;
    ifGenerationMatch : int64;
    ifGenerationNotMatch : int64;
    ifMetagenerationMatch : int64;
    ifMetagenerationNotMatch : int64;
    projection : string;
  end;
  
  
  //Optional query Options for TObjectsResource, method Insert
  
  TObjectsInsertOptions = Record
    contentEncoding : string;
    ifGenerationMatch : int64;
    ifGenerationNotMatch : int64;
    ifMetagenerationMatch : int64;
    ifMetagenerationNotMatch : int64;
    _name : string;
    predefinedAcl : string;
    projection : string;
  end;
  
  
  //Optional query Options for TObjectsResource, method List
  
  TObjectsListOptions = Record
    delimiter : string;
    maxResults : integer;
    pageToken : string;
    prefix : string;
    projection : string;
    versions : boolean;
  end;
  
  
  //Optional query Options for TObjectsResource, method Patch
  
  TObjectsPatchOptions = Record
    generation : int64;
    ifGenerationMatch : int64;
    ifGenerationNotMatch : int64;
    ifMetagenerationMatch : int64;
    ifMetagenerationNotMatch : int64;
    predefinedAcl : string;
    projection : string;
  end;
  
  
  //Optional query Options for TObjectsResource, method Rewrite
  
  TObjectsRewriteOptions = Record
    destinationPredefinedAcl : string;
    ifGenerationMatch : int64;
    ifGenerationNotMatch : int64;
    ifMetagenerationMatch : int64;
    ifMetagenerationNotMatch : int64;
    ifSourceGenerationMatch : int64;
    ifSourceGenerationNotMatch : int64;
    ifSourceMetagenerationMatch : int64;
    ifSourceMetagenerationNotMatch : int64;
    maxBytesRewrittenPerCall : int64;
    projection : string;
    rewriteToken : string;
    sourceGeneration : int64;
  end;
  
  
  //Optional query Options for TObjectsResource, method Update
  
  TObjectsUpdateOptions = Record
    generation : int64;
    ifGenerationMatch : int64;
    ifGenerationNotMatch : int64;
    ifMetagenerationMatch : int64;
    ifMetagenerationNotMatch : int64;
    predefinedAcl : string;
    projection : string;
  end;
  
  
  //Optional query Options for TObjectsResource, method WatchAll
  
  TObjectsWatchAllOptions = Record
    delimiter : string;
    maxResults : integer;
    pageToken : string;
    prefix : string;
    projection : string;
    versions : boolean;
  end;
  
  TObjectsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Compose(destinationBucket: string; destinationObject: string; aComposeRequest : TComposeRequest; AQuery : string  = '') : TObject;
    Function Compose(destinationBucket: string; destinationObject: string; aComposeRequest : TComposeRequest; AQuery : TObjectscomposeOptions) : TObject;
    Function Copy(destinationBucket: string; destinationObject: string; sourceBucket: string; sourceObject: string; aObject : TObject; AQuery : string  = '') : TObject;
    Function Copy(destinationBucket: string; destinationObject: string; sourceBucket: string; sourceObject: string; aObject : TObject; AQuery : TObjectscopyOptions) : TObject;
    Procedure Delete(bucket: string; _object: string; AQuery : string  = '');
    Procedure Delete(bucket: string; _object: string; AQuery : TObjectsdeleteOptions);
    Function Get(bucket: string; _object: string; AQuery : string  = '') : TObject;
    Function Get(bucket: string; _object: string; AQuery : TObjectsgetOptions) : TObject;
    Function Insert(bucket: string; aObject : TObject; AQuery : string  = '') : TObject;
    Function Insert(bucket: string; aObject : TObject; AQuery : TObjectsinsertOptions) : TObject;
    Function List(bucket: string; AQuery : string  = '') : TObjects;
    Function List(bucket: string; AQuery : TObjectslistOptions) : TObjects;
    Function Patch(bucket: string; _object: string; aObject : TObject; AQuery : string  = '') : TObject;
    Function Patch(bucket: string; _object: string; aObject : TObject; AQuery : TObjectspatchOptions) : TObject;
    Function Rewrite(destinationBucket: string; destinationObject: string; sourceBucket: string; sourceObject: string; aObject : TObject; AQuery : string  = '') : TRewriteResponse;
    Function Rewrite(destinationBucket: string; destinationObject: string; sourceBucket: string; sourceObject: string; aObject : TObject; AQuery : TObjectsrewriteOptions) : TRewriteResponse;
    Function Update(bucket: string; _object: string; aObject : TObject; AQuery : string  = '') : TObject;
    Function Update(bucket: string; _object: string; aObject : TObject; AQuery : TObjectsupdateOptions) : TObject;
    Function WatchAll(bucket: string; aChannel : TChannel; AQuery : string  = '') : TChannel;
    Function WatchAll(bucket: string; aChannel : TChannel; AQuery : TObjectswatchAllOptions) : TChannel;
  end;
  
  
  { --------------------------------------------------------------------
    TStorageAPI
    --------------------------------------------------------------------}
  
  TStorageAPI = Class(TGoogleAPI)
  Private
    FBucketAccessControlsInstance : TBucketAccessControlsResource;
    FBucketsInstance : TBucketsResource;
    FChannelsInstance : TChannelsResource;
    FDefaultObjectAccessControlsInstance : TDefaultObjectAccessControlsResource;
    FObjectAccessControlsInstance : TObjectAccessControlsResource;
    FObjectsInstance : TObjectsResource;
    Function GetBucketAccessControlsInstance : TBucketAccessControlsResource;virtual;
    Function GetBucketsInstance : TBucketsResource;virtual;
    Function GetChannelsInstance : TChannelsResource;virtual;
    Function GetDefaultObjectAccessControlsInstance : TDefaultObjectAccessControlsResource;virtual;
    Function GetObjectAccessControlsInstance : TObjectAccessControlsResource;virtual;
    Function GetObjectsInstance : TObjectsResource;virtual;
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
    Function CreateBucketAccessControlsResource(AOwner : TComponent) : TBucketAccessControlsResource;virtual;overload;
    Function CreateBucketAccessControlsResource : TBucketAccessControlsResource;virtual;overload;
    Function CreateBucketsResource(AOwner : TComponent) : TBucketsResource;virtual;overload;
    Function CreateBucketsResource : TBucketsResource;virtual;overload;
    Function CreateChannelsResource(AOwner : TComponent) : TChannelsResource;virtual;overload;
    Function CreateChannelsResource : TChannelsResource;virtual;overload;
    Function CreateDefaultObjectAccessControlsResource(AOwner : TComponent) : TDefaultObjectAccessControlsResource;virtual;overload;
    Function CreateDefaultObjectAccessControlsResource : TDefaultObjectAccessControlsResource;virtual;overload;
    Function CreateObjectAccessControlsResource(AOwner : TComponent) : TObjectAccessControlsResource;virtual;overload;
    Function CreateObjectAccessControlsResource : TObjectAccessControlsResource;virtual;overload;
    Function CreateObjectsResource(AOwner : TComponent) : TObjectsResource;virtual;overload;
    Function CreateObjectsResource : TObjectsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property BucketAccessControlsResource : TBucketAccessControlsResource Read GetBucketAccessControlsInstance;
    Property BucketsResource : TBucketsResource Read GetBucketsInstance;
    Property ChannelsResource : TChannelsResource Read GetChannelsInstance;
    Property DefaultObjectAccessControlsResource : TDefaultObjectAccessControlsResource Read GetDefaultObjectAccessControlsInstance;
    Property ObjectAccessControlsResource : TObjectAccessControlsResource Read GetObjectAccessControlsInstance;
    Property ObjectsResource : TObjectsResource Read GetObjectsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TBucket
  --------------------------------------------------------------------}


Procedure TBucket.Setacl(AIndex : Integer; AValue : TBucketacl); 

begin
  If (Facl=AValue) then exit;
  Facl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Setcors(AIndex : Integer; AValue : TBucketcors); 

begin
  If (Fcors=AValue) then exit;
  Fcors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.SetdefaultObjectAcl(AIndex : Integer; AValue : TBucketdefaultObjectAcl); 

begin
  If (FdefaultObjectAcl=AValue) then exit;
  FdefaultObjectAcl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Setlifecycle(AIndex : Integer; AValue : TBucketlifecycle); 

begin
  If (Flifecycle=AValue) then exit;
  Flifecycle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Setlocation(AIndex : Integer; AValue : string); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Setlogging(AIndex : Integer; AValue : TBucketlogging); 

begin
  If (Flogging=AValue) then exit;
  Flogging:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Setmetageneration(AIndex : Integer; AValue : string); 

begin
  If (Fmetageneration=AValue) then exit;
  Fmetageneration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Setowner(AIndex : Integer; AValue : TBucketowner); 

begin
  If (Fowner=AValue) then exit;
  Fowner:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.SetprojectNumber(AIndex : Integer; AValue : string); 

begin
  If (FprojectNumber=AValue) then exit;
  FprojectNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.SetstorageClass(AIndex : Integer; AValue : string); 

begin
  If (FstorageClass=AValue) then exit;
  FstorageClass:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.SettimeCreated(AIndex : Integer; AValue : TDatetime); 

begin
  If (FtimeCreated=AValue) then exit;
  FtimeCreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Setversioning(AIndex : Integer; AValue : TBucketversioning); 

begin
  If (Fversioning=AValue) then exit;
  Fversioning:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Setwebsite(AIndex : Integer; AValue : TBucketwebsite); 

begin
  If (Fwebsite=AValue) then exit;
  Fwebsite:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBucketacl
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TBucketcors
  --------------------------------------------------------------------}


Procedure TBucketcors.SetmaxAgeSeconds(AIndex : Integer; AValue : integer); 

begin
  If (FmaxAgeSeconds=AValue) then exit;
  FmaxAgeSeconds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketcors.Setmethod(AIndex : Integer; AValue : TBucketcorsmethod); 

begin
  If (Fmethod=AValue) then exit;
  Fmethod:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketcors.Setorigin(AIndex : Integer; AValue : TBucketcorsorigin); 

begin
  If (Forigin=AValue) then exit;
  Forigin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketcors.SetresponseHeader(AIndex : Integer; AValue : TBucketcorsresponseHeader); 

begin
  If (FresponseHeader=AValue) then exit;
  FresponseHeader:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBucketcorsmethod
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TBucketcorsorigin
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TBucketcorsresponseHeader
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TBucketdefaultObjectAcl
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TBucketlifecycle
  --------------------------------------------------------------------}


Procedure TBucketlifecycle.Setrule(AIndex : Integer; AValue : TBucketlifecyclerule); 

begin
  If (Frule=AValue) then exit;
  Frule:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBucketlifecyclerule
  --------------------------------------------------------------------}


Procedure TBucketlifecyclerule.Setaction(AIndex : Integer; AValue : TBucketlifecycleruleaction); 

begin
  If (Faction=AValue) then exit;
  Faction:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketlifecyclerule.Setcondition(AIndex : Integer; AValue : TBucketlifecyclerulecondition); 

begin
  If (Fcondition=AValue) then exit;
  Fcondition:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBucketlifecycleruleaction
  --------------------------------------------------------------------}


Procedure TBucketlifecycleruleaction.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TBucketlifecycleruleaction.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TBucketlifecyclerulecondition
  --------------------------------------------------------------------}


Procedure TBucketlifecyclerulecondition.Setage(AIndex : Integer; AValue : integer); 

begin
  If (Fage=AValue) then exit;
  Fage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketlifecyclerulecondition.SetcreatedBefore(AIndex : Integer; AValue : TDate); 

begin
  If (FcreatedBefore=AValue) then exit;
  FcreatedBefore:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketlifecyclerulecondition.SetisLive(AIndex : Integer; AValue : boolean); 

begin
  If (FisLive=AValue) then exit;
  FisLive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketlifecyclerulecondition.SetnumNewerVersions(AIndex : Integer; AValue : integer); 

begin
  If (FnumNewerVersions=AValue) then exit;
  FnumNewerVersions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBucketlogging
  --------------------------------------------------------------------}


Procedure TBucketlogging.SetlogBucket(AIndex : Integer; AValue : string); 

begin
  If (FlogBucket=AValue) then exit;
  FlogBucket:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketlogging.SetlogObjectPrefix(AIndex : Integer; AValue : string); 

begin
  If (FlogObjectPrefix=AValue) then exit;
  FlogObjectPrefix:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBucketowner
  --------------------------------------------------------------------}


Procedure TBucketowner.Setentity(AIndex : Integer; AValue : string); 

begin
  If (Fentity=AValue) then exit;
  Fentity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketowner.SetentityId(AIndex : Integer; AValue : string); 

begin
  If (FentityId=AValue) then exit;
  FentityId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBucketversioning
  --------------------------------------------------------------------}


Procedure TBucketversioning.Setenabled(AIndex : Integer; AValue : boolean); 

begin
  If (Fenabled=AValue) then exit;
  Fenabled:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBucketwebsite
  --------------------------------------------------------------------}


Procedure TBucketwebsite.SetmainPageSuffix(AIndex : Integer; AValue : string); 

begin
  If (FmainPageSuffix=AValue) then exit;
  FmainPageSuffix:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketwebsite.SetnotFoundPage(AIndex : Integer; AValue : string); 

begin
  If (FnotFoundPage=AValue) then exit;
  FnotFoundPage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBucketAccessControl
  --------------------------------------------------------------------}


Procedure TBucketAccessControl.Setbucket(AIndex : Integer; AValue : string); 

begin
  If (Fbucket=AValue) then exit;
  Fbucket:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketAccessControl.Setdomain(AIndex : Integer; AValue : string); 

begin
  If (Fdomain=AValue) then exit;
  Fdomain:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketAccessControl.Setemail(AIndex : Integer; AValue : string); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketAccessControl.Setentity(AIndex : Integer; AValue : string); 

begin
  If (Fentity=AValue) then exit;
  Fentity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketAccessControl.SetentityId(AIndex : Integer; AValue : string); 

begin
  If (FentityId=AValue) then exit;
  FentityId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketAccessControl.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketAccessControl.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketAccessControl.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketAccessControl.SetprojectTeam(AIndex : Integer; AValue : TBucketAccessControlprojectTeam); 

begin
  If (FprojectTeam=AValue) then exit;
  FprojectTeam:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketAccessControl.Setrole(AIndex : Integer; AValue : string); 

begin
  If (Frole=AValue) then exit;
  Frole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketAccessControl.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBucketAccessControlprojectTeam
  --------------------------------------------------------------------}


Procedure TBucketAccessControlprojectTeam.SetprojectNumber(AIndex : Integer; AValue : string); 

begin
  If (FprojectNumber=AValue) then exit;
  FprojectNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketAccessControlprojectTeam.Setteam(AIndex : Integer; AValue : string); 

begin
  If (Fteam=AValue) then exit;
  Fteam:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBucketAccessControls
  --------------------------------------------------------------------}


Procedure TBucketAccessControls.Setitems(AIndex : Integer; AValue : TBucketAccessControlsitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketAccessControls.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBucketAccessControlsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TBuckets
  --------------------------------------------------------------------}


Procedure TBuckets.Setitems(AIndex : Integer; AValue : TBucketsitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBuckets.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBuckets.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBucketsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TChannel
  --------------------------------------------------------------------}


Procedure TChannel.Setaddress(AIndex : Integer; AValue : string); 

begin
  If (Faddress=AValue) then exit;
  Faddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Setexpiration(AIndex : Integer; AValue : string); 

begin
  If (Fexpiration=AValue) then exit;
  Fexpiration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Setparams(AIndex : Integer; AValue : TChannelparams); 

begin
  If (Fparams=AValue) then exit;
  Fparams:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Setpayload(AIndex : Integer; AValue : boolean); 

begin
  If (Fpayload=AValue) then exit;
  Fpayload:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.SetresourceId(AIndex : Integer; AValue : string); 

begin
  If (FresourceId=AValue) then exit;
  FresourceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.SetresourceUri(AIndex : Integer; AValue : string); 

begin
  If (FresourceUri=AValue) then exit;
  FresourceUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Settoken(AIndex : Integer; AValue : string); 

begin
  If (Ftoken=AValue) then exit;
  Ftoken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TChannel.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TChannelparams
  --------------------------------------------------------------------}


Class Function TChannelparams.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TComposeRequest
  --------------------------------------------------------------------}


Procedure TComposeRequest.Setdestination(AIndex : Integer; AValue : TObject); 

begin
  If (Fdestination=AValue) then exit;
  Fdestination:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComposeRequest.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComposeRequest.SetsourceObjects(AIndex : Integer; AValue : TComposeRequestsourceObjects); 

begin
  If (FsourceObjects=AValue) then exit;
  FsourceObjects:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TComposeRequestsourceObjects
  --------------------------------------------------------------------}


Procedure TComposeRequestsourceObjects.Setgeneration(AIndex : Integer; AValue : string); 

begin
  If (Fgeneration=AValue) then exit;
  Fgeneration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComposeRequestsourceObjects.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComposeRequestsourceObjects.SetobjectPreconditions(AIndex : Integer; AValue : TComposeRequestsourceObjectsobjectPreconditions); 

begin
  If (FobjectPreconditions=AValue) then exit;
  FobjectPreconditions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TComposeRequestsourceObjectsobjectPreconditions
  --------------------------------------------------------------------}


Procedure TComposeRequestsourceObjectsobjectPreconditions.SetifGenerationMatch(AIndex : Integer; AValue : string); 

begin
  If (FifGenerationMatch=AValue) then exit;
  FifGenerationMatch:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TObject
  --------------------------------------------------------------------}


Procedure TObject.Setacl(AIndex : Integer; AValue : TObjectacl); 

begin
  If (Facl=AValue) then exit;
  Facl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.Setbucket(AIndex : Integer; AValue : string); 

begin
  If (Fbucket=AValue) then exit;
  Fbucket:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.SetcacheControl(AIndex : Integer; AValue : string); 

begin
  If (FcacheControl=AValue) then exit;
  FcacheControl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.SetcomponentCount(AIndex : Integer; AValue : integer); 

begin
  If (FcomponentCount=AValue) then exit;
  FcomponentCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.SetcontentDisposition(AIndex : Integer; AValue : string); 

begin
  If (FcontentDisposition=AValue) then exit;
  FcontentDisposition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.SetcontentEncoding(AIndex : Integer; AValue : string); 

begin
  If (FcontentEncoding=AValue) then exit;
  FcontentEncoding:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.SetcontentLanguage(AIndex : Integer; AValue : string); 

begin
  If (FcontentLanguage=AValue) then exit;
  FcontentLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.SetcontentType(AIndex : Integer; AValue : string); 

begin
  If (FcontentType=AValue) then exit;
  FcontentType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.Setcrc32c(AIndex : Integer; AValue : string); 

begin
  If (Fcrc32c=AValue) then exit;
  Fcrc32c:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.Setgeneration(AIndex : Integer; AValue : string); 

begin
  If (Fgeneration=AValue) then exit;
  Fgeneration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.Setmd5Hash(AIndex : Integer; AValue : string); 

begin
  If (Fmd5Hash=AValue) then exit;
  Fmd5Hash:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.SetmediaLink(AIndex : Integer; AValue : string); 

begin
  If (FmediaLink=AValue) then exit;
  FmediaLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.Setmetadata(AIndex : Integer; AValue : TObjectmetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.Setmetageneration(AIndex : Integer; AValue : string); 

begin
  If (Fmetageneration=AValue) then exit;
  Fmetageneration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.Setowner(AIndex : Integer; AValue : TObjectowner); 

begin
  If (Fowner=AValue) then exit;
  Fowner:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.Setsize(AIndex : Integer; AValue : string); 

begin
  If (Fsize=AValue) then exit;
  Fsize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.SetstorageClass(AIndex : Integer; AValue : string); 

begin
  If (FstorageClass=AValue) then exit;
  FstorageClass:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.SettimeDeleted(AIndex : Integer; AValue : TDatetime); 

begin
  If (FtimeDeleted=AValue) then exit;
  FtimeDeleted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TObjectacl
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TObjectmetadata
  --------------------------------------------------------------------}


Class Function TObjectmetadata.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TObjectowner
  --------------------------------------------------------------------}


Procedure TObjectowner.Setentity(AIndex : Integer; AValue : string); 

begin
  If (Fentity=AValue) then exit;
  Fentity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectowner.SetentityId(AIndex : Integer; AValue : string); 

begin
  If (FentityId=AValue) then exit;
  FentityId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TObjectAccessControl
  --------------------------------------------------------------------}


Procedure TObjectAccessControl.Setbucket(AIndex : Integer; AValue : string); 

begin
  If (Fbucket=AValue) then exit;
  Fbucket:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectAccessControl.Setdomain(AIndex : Integer; AValue : string); 

begin
  If (Fdomain=AValue) then exit;
  Fdomain:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectAccessControl.Setemail(AIndex : Integer; AValue : string); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectAccessControl.Setentity(AIndex : Integer; AValue : string); 

begin
  If (Fentity=AValue) then exit;
  Fentity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectAccessControl.SetentityId(AIndex : Integer; AValue : string); 

begin
  If (FentityId=AValue) then exit;
  FentityId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectAccessControl.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectAccessControl.Setgeneration(AIndex : Integer; AValue : string); 

begin
  If (Fgeneration=AValue) then exit;
  Fgeneration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectAccessControl.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectAccessControl.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectAccessControl.Set_object(AIndex : Integer; AValue : string); 

begin
  If (F_object=AValue) then exit;
  F_object:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectAccessControl.SetprojectTeam(AIndex : Integer; AValue : TObjectAccessControlprojectTeam); 

begin
  If (FprojectTeam=AValue) then exit;
  FprojectTeam:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectAccessControl.Setrole(AIndex : Integer; AValue : string); 

begin
  If (Frole=AValue) then exit;
  Frole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectAccessControl.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TObjectAccessControl.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_object' : Result:='object';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TObjectAccessControlprojectTeam
  --------------------------------------------------------------------}


Procedure TObjectAccessControlprojectTeam.SetprojectNumber(AIndex : Integer; AValue : string); 

begin
  If (FprojectNumber=AValue) then exit;
  FprojectNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectAccessControlprojectTeam.Setteam(AIndex : Integer; AValue : string); 

begin
  If (Fteam=AValue) then exit;
  Fteam:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TObjectAccessControls
  --------------------------------------------------------------------}


Procedure TObjectAccessControls.Setitems(AIndex : Integer; AValue : TObjectAccessControlsitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectAccessControls.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TObjectAccessControlsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TObjects
  --------------------------------------------------------------------}


Procedure TObjects.Setitems(AIndex : Integer; AValue : TObjectsitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjects.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjects.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjects.Setprefixes(AIndex : Integer; AValue : TObjectsprefixes); 

begin
  If (Fprefixes=AValue) then exit;
  Fprefixes:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TObjectsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TObjectsprefixes
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRewriteResponse
  --------------------------------------------------------------------}


Procedure TRewriteResponse.Setdone(AIndex : Integer; AValue : boolean); 

begin
  If (Fdone=AValue) then exit;
  Fdone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRewriteResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRewriteResponse.SetobjectSize(AIndex : Integer; AValue : string); 

begin
  If (FobjectSize=AValue) then exit;
  FobjectSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRewriteResponse.Setresource(AIndex : Integer; AValue : TObject); 

begin
  If (Fresource=AValue) then exit;
  Fresource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRewriteResponse.SetrewriteToken(AIndex : Integer; AValue : string); 

begin
  If (FrewriteToken=AValue) then exit;
  FrewriteToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRewriteResponse.SettotalBytesRewritten(AIndex : Integer; AValue : string); 

begin
  If (FtotalBytesRewritten=AValue) then exit;
  FtotalBytesRewritten:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBucketAccessControlsResource
  --------------------------------------------------------------------}


Class Function TBucketAccessControlsResource.ResourceName : String;

begin
  Result:='bucketAccessControls';
end;

Class Function TBucketAccessControlsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TstorageAPI;
end;

Procedure TBucketAccessControlsResource.Delete(bucket: string; entity: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'b/{bucket}/acl/{entity}';
  _Methodid   = 'storage.bucketAccessControls.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket,'entity',entity]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TBucketAccessControlsResource.Get(bucket: string; entity: string) : TBucketAccessControl;

Const
  _HTTPMethod = 'GET';
  _Path       = 'b/{bucket}/acl/{entity}';
  _Methodid   = 'storage.bucketAccessControls.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket,'entity',entity]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TBucketAccessControl) as TBucketAccessControl;
end;

Function TBucketAccessControlsResource.Insert(bucket: string; aBucketAccessControl : TBucketAccessControl) : TBucketAccessControl;

Const
  _HTTPMethod = 'POST';
  _Path       = 'b/{bucket}/acl';
  _Methodid   = 'storage.bucketAccessControls.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aBucketAccessControl,TBucketAccessControl) as TBucketAccessControl;
end;

Function TBucketAccessControlsResource.List(bucket: string) : TBucketAccessControls;

Const
  _HTTPMethod = 'GET';
  _Path       = 'b/{bucket}/acl';
  _Methodid   = 'storage.bucketAccessControls.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TBucketAccessControls) as TBucketAccessControls;
end;

Function TBucketAccessControlsResource.Patch(bucket: string; entity: string; aBucketAccessControl : TBucketAccessControl) : TBucketAccessControl;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'b/{bucket}/acl/{entity}';
  _Methodid   = 'storage.bucketAccessControls.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket,'entity',entity]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aBucketAccessControl,TBucketAccessControl) as TBucketAccessControl;
end;

Function TBucketAccessControlsResource.Update(bucket: string; entity: string; aBucketAccessControl : TBucketAccessControl) : TBucketAccessControl;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'b/{bucket}/acl/{entity}';
  _Methodid   = 'storage.bucketAccessControls.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket,'entity',entity]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aBucketAccessControl,TBucketAccessControl) as TBucketAccessControl;
end;



{ --------------------------------------------------------------------
  TBucketsResource
  --------------------------------------------------------------------}


Class Function TBucketsResource.ResourceName : String;

begin
  Result:='buckets';
end;

Class Function TBucketsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TstorageAPI;
end;

Procedure TBucketsResource.Delete(bucket: string; AQuery : string = '');

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'b/{bucket}';
  _Methodid   = 'storage.buckets.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket]);
  ServiceCall(_HTTPMethod,_P,AQuery,Nil,Nil);
end;


Procedure TBucketsResource.Delete(bucket: string; AQuery : TBucketsdeleteOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'ifMetagenerationMatch',AQuery.ifMetagenerationMatch);
  AddToQuery(_Q,'ifMetagenerationNotMatch',AQuery.ifMetagenerationNotMatch);
  Delete(bucket,_Q);
end;

Function TBucketsResource.Get(bucket: string; AQuery : string = '') : TBucket;

Const
  _HTTPMethod = 'GET';
  _Path       = 'b/{bucket}';
  _Methodid   = 'storage.buckets.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TBucket) as TBucket;
end;


Function TBucketsResource.Get(bucket: string; AQuery : TBucketsgetOptions) : TBucket;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'ifMetagenerationMatch',AQuery.ifMetagenerationMatch);
  AddToQuery(_Q,'ifMetagenerationNotMatch',AQuery.ifMetagenerationNotMatch);
  AddToQuery(_Q,'projection',AQuery.projection);
  Result:=Get(bucket,_Q);
end;

Function TBucketsResource.Insert(aBucket : TBucket; AQuery : string = '') : TBucket;

Const
  _HTTPMethod = 'POST';
  _Path       = 'b';
  _Methodid   = 'storage.buckets.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aBucket,TBucket) as TBucket;
end;


Function TBucketsResource.Insert(aBucket : TBucket; AQuery : TBucketsinsertOptions) : TBucket;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'predefinedAcl',AQuery.predefinedAcl);
  AddToQuery(_Q,'predefinedDefaultObjectAcl',AQuery.predefinedDefaultObjectAcl);
  AddToQuery(_Q,'project',AQuery.project);
  AddToQuery(_Q,'projection',AQuery.projection);
  Result:=Insert(aBucket,_Q);
end;

Function TBucketsResource.List(AQuery : string = '') : TBuckets;

Const
  _HTTPMethod = 'GET';
  _Path       = 'b';
  _Methodid   = 'storage.buckets.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TBuckets) as TBuckets;
end;


Function TBucketsResource.List(AQuery : TBucketslistOptions) : TBuckets;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'prefix',AQuery.prefix);
  AddToQuery(_Q,'project',AQuery.project);
  AddToQuery(_Q,'projection',AQuery.projection);
  Result:=List(_Q);
end;

Function TBucketsResource.Patch(bucket: string; aBucket : TBucket; AQuery : string = '') : TBucket;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'b/{bucket}';
  _Methodid   = 'storage.buckets.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aBucket,TBucket) as TBucket;
end;


Function TBucketsResource.Patch(bucket: string; aBucket : TBucket; AQuery : TBucketspatchOptions) : TBucket;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'ifMetagenerationMatch',AQuery.ifMetagenerationMatch);
  AddToQuery(_Q,'ifMetagenerationNotMatch',AQuery.ifMetagenerationNotMatch);
  AddToQuery(_Q,'predefinedAcl',AQuery.predefinedAcl);
  AddToQuery(_Q,'predefinedDefaultObjectAcl',AQuery.predefinedDefaultObjectAcl);
  AddToQuery(_Q,'projection',AQuery.projection);
  Result:=Patch(bucket,aBucket,_Q);
end;

Function TBucketsResource.Update(bucket: string; aBucket : TBucket; AQuery : string = '') : TBucket;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'b/{bucket}';
  _Methodid   = 'storage.buckets.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aBucket,TBucket) as TBucket;
end;


Function TBucketsResource.Update(bucket: string; aBucket : TBucket; AQuery : TBucketsupdateOptions) : TBucket;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'ifMetagenerationMatch',AQuery.ifMetagenerationMatch);
  AddToQuery(_Q,'ifMetagenerationNotMatch',AQuery.ifMetagenerationNotMatch);
  AddToQuery(_Q,'predefinedAcl',AQuery.predefinedAcl);
  AddToQuery(_Q,'predefinedDefaultObjectAcl',AQuery.predefinedDefaultObjectAcl);
  AddToQuery(_Q,'projection',AQuery.projection);
  Result:=Update(bucket,aBucket,_Q);
end;



{ --------------------------------------------------------------------
  TChannelsResource
  --------------------------------------------------------------------}


Class Function TChannelsResource.ResourceName : String;

begin
  Result:='channels';
end;

Class Function TChannelsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TstorageAPI;
end;

Procedure TChannelsResource.Stop(aChannel : TChannel);

Const
  _HTTPMethod = 'POST';
  _Path       = 'channels/stop';
  _Methodid   = 'storage.channels.stop';

begin
  ServiceCall(_HTTPMethod,_Path,'',aChannel,Nil);
end;



{ --------------------------------------------------------------------
  TDefaultObjectAccessControlsResource
  --------------------------------------------------------------------}


Class Function TDefaultObjectAccessControlsResource.ResourceName : String;

begin
  Result:='defaultObjectAccessControls';
end;

Class Function TDefaultObjectAccessControlsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TstorageAPI;
end;

Procedure TDefaultObjectAccessControlsResource.Delete(bucket: string; entity: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'b/{bucket}/defaultObjectAcl/{entity}';
  _Methodid   = 'storage.defaultObjectAccessControls.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket,'entity',entity]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TDefaultObjectAccessControlsResource.Get(bucket: string; entity: string) : TObjectAccessControl;

Const
  _HTTPMethod = 'GET';
  _Path       = 'b/{bucket}/defaultObjectAcl/{entity}';
  _Methodid   = 'storage.defaultObjectAccessControls.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket,'entity',entity]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TObjectAccessControl) as TObjectAccessControl;
end;

Function TDefaultObjectAccessControlsResource.Insert(bucket: string; aObjectAccessControl : TObjectAccessControl) : TObjectAccessControl;

Const
  _HTTPMethod = 'POST';
  _Path       = 'b/{bucket}/defaultObjectAcl';
  _Methodid   = 'storage.defaultObjectAccessControls.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aObjectAccessControl,TObjectAccessControl) as TObjectAccessControl;
end;

Function TDefaultObjectAccessControlsResource.List(bucket: string; AQuery : string = '') : TObjectAccessControls;

Const
  _HTTPMethod = 'GET';
  _Path       = 'b/{bucket}/defaultObjectAcl';
  _Methodid   = 'storage.defaultObjectAccessControls.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TObjectAccessControls) as TObjectAccessControls;
end;


Function TDefaultObjectAccessControlsResource.List(bucket: string; AQuery : TDefaultObjectAccessControlslistOptions) : TObjectAccessControls;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'ifMetagenerationMatch',AQuery.ifMetagenerationMatch);
  AddToQuery(_Q,'ifMetagenerationNotMatch',AQuery.ifMetagenerationNotMatch);
  Result:=List(bucket,_Q);
end;

Function TDefaultObjectAccessControlsResource.Patch(bucket: string; entity: string; aObjectAccessControl : TObjectAccessControl) : TObjectAccessControl;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'b/{bucket}/defaultObjectAcl/{entity}';
  _Methodid   = 'storage.defaultObjectAccessControls.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket,'entity',entity]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aObjectAccessControl,TObjectAccessControl) as TObjectAccessControl;
end;

Function TDefaultObjectAccessControlsResource.Update(bucket: string; entity: string; aObjectAccessControl : TObjectAccessControl) : TObjectAccessControl;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'b/{bucket}/defaultObjectAcl/{entity}';
  _Methodid   = 'storage.defaultObjectAccessControls.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket,'entity',entity]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aObjectAccessControl,TObjectAccessControl) as TObjectAccessControl;
end;



{ --------------------------------------------------------------------
  TObjectAccessControlsResource
  --------------------------------------------------------------------}


Class Function TObjectAccessControlsResource.ResourceName : String;

begin
  Result:='objectAccessControls';
end;

Class Function TObjectAccessControlsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TstorageAPI;
end;

Procedure TObjectAccessControlsResource.Delete(bucket: string; entity: string; _object: string; AQuery : string = '');

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'b/{bucket}/o/{object}/acl/{entity}';
  _Methodid   = 'storage.objectAccessControls.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket,'entity',entity,'object',_object]);
  ServiceCall(_HTTPMethod,_P,AQuery,Nil,Nil);
end;


Procedure TObjectAccessControlsResource.Delete(bucket: string; entity: string; _object: string; AQuery : TObjectAccessControlsdeleteOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'generation',AQuery.generation);
  Delete(bucket,entity,_object,_Q);
end;

Function TObjectAccessControlsResource.Get(bucket: string; entity: string; _object: string; AQuery : string = '') : TObjectAccessControl;

Const
  _HTTPMethod = 'GET';
  _Path       = 'b/{bucket}/o/{object}/acl/{entity}';
  _Methodid   = 'storage.objectAccessControls.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket,'entity',entity,'object',_object]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TObjectAccessControl) as TObjectAccessControl;
end;


Function TObjectAccessControlsResource.Get(bucket: string; entity: string; _object: string; AQuery : TObjectAccessControlsgetOptions) : TObjectAccessControl;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'generation',AQuery.generation);
  Result:=Get(bucket,entity,_object,_Q);
end;

Function TObjectAccessControlsResource.Insert(bucket: string; _object: string; aObjectAccessControl : TObjectAccessControl; AQuery : string = '') : TObjectAccessControl;

Const
  _HTTPMethod = 'POST';
  _Path       = 'b/{bucket}/o/{object}/acl';
  _Methodid   = 'storage.objectAccessControls.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket,'object',_object]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aObjectAccessControl,TObjectAccessControl) as TObjectAccessControl;
end;


Function TObjectAccessControlsResource.Insert(bucket: string; _object: string; aObjectAccessControl : TObjectAccessControl; AQuery : TObjectAccessControlsinsertOptions) : TObjectAccessControl;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'generation',AQuery.generation);
  Result:=Insert(bucket,_object,aObjectAccessControl,_Q);
end;

Function TObjectAccessControlsResource.List(bucket: string; _object: string; AQuery : string = '') : TObjectAccessControls;

Const
  _HTTPMethod = 'GET';
  _Path       = 'b/{bucket}/o/{object}/acl';
  _Methodid   = 'storage.objectAccessControls.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket,'object',_object]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TObjectAccessControls) as TObjectAccessControls;
end;


Function TObjectAccessControlsResource.List(bucket: string; _object: string; AQuery : TObjectAccessControlslistOptions) : TObjectAccessControls;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'generation',AQuery.generation);
  Result:=List(bucket,_object,_Q);
end;

Function TObjectAccessControlsResource.Patch(bucket: string; entity: string; _object: string; aObjectAccessControl : TObjectAccessControl; AQuery : string = '') : TObjectAccessControl;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'b/{bucket}/o/{object}/acl/{entity}';
  _Methodid   = 'storage.objectAccessControls.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket,'entity',entity,'object',_object]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aObjectAccessControl,TObjectAccessControl) as TObjectAccessControl;
end;


Function TObjectAccessControlsResource.Patch(bucket: string; entity: string; _object: string; aObjectAccessControl : TObjectAccessControl; AQuery : TObjectAccessControlspatchOptions) : TObjectAccessControl;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'generation',AQuery.generation);
  Result:=Patch(bucket,entity,_object,aObjectAccessControl,_Q);
end;

Function TObjectAccessControlsResource.Update(bucket: string; entity: string; _object: string; aObjectAccessControl : TObjectAccessControl; AQuery : string = '') : TObjectAccessControl;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'b/{bucket}/o/{object}/acl/{entity}';
  _Methodid   = 'storage.objectAccessControls.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket,'entity',entity,'object',_object]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aObjectAccessControl,TObjectAccessControl) as TObjectAccessControl;
end;


Function TObjectAccessControlsResource.Update(bucket: string; entity: string; _object: string; aObjectAccessControl : TObjectAccessControl; AQuery : TObjectAccessControlsupdateOptions) : TObjectAccessControl;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'generation',AQuery.generation);
  Result:=Update(bucket,entity,_object,aObjectAccessControl,_Q);
end;



{ --------------------------------------------------------------------
  TObjectsResource
  --------------------------------------------------------------------}


Class Function TObjectsResource.ResourceName : String;

begin
  Result:='objects';
end;

Class Function TObjectsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TstorageAPI;
end;

Function TObjectsResource.Compose(destinationBucket: string; destinationObject: string; aComposeRequest : TComposeRequest; AQuery : string = '') : TObject;

Const
  _HTTPMethod = 'POST';
  _Path       = 'b/{destinationBucket}/o/{destinationObject}/compose';
  _Methodid   = 'storage.objects.compose';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['destinationBucket',destinationBucket,'destinationObject',destinationObject]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aComposeRequest,TObject) as TObject;
end;


Function TObjectsResource.Compose(destinationBucket: string; destinationObject: string; aComposeRequest : TComposeRequest; AQuery : TObjectscomposeOptions) : TObject;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'destinationPredefinedAcl',AQuery.destinationPredefinedAcl);
  AddToQuery(_Q,'ifGenerationMatch',AQuery.ifGenerationMatch);
  AddToQuery(_Q,'ifMetagenerationMatch',AQuery.ifMetagenerationMatch);
  Result:=Compose(destinationBucket,destinationObject,aComposeRequest,_Q);
end;

Function TObjectsResource.Copy(destinationBucket: string; destinationObject: string; sourceBucket: string; sourceObject: string; aObject : TObject; AQuery : string = '') : TObject;

Const
  _HTTPMethod = 'POST';
  _Path       = 'b/{sourceBucket}/o/{sourceObject}/copyTo/b/{destinationBucket}/o/{destinationObject}';
  _Methodid   = 'storage.objects.copy';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['destinationBucket',destinationBucket,'destinationObject',destinationObject,'sourceBucket',sourceBucket,'sourceObject',sourceObject]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aObject,TObject) as TObject;
end;


Function TObjectsResource.Copy(destinationBucket: string; destinationObject: string; sourceBucket: string; sourceObject: string; aObject : TObject; AQuery : TObjectscopyOptions) : TObject;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'destinationPredefinedAcl',AQuery.destinationPredefinedAcl);
  AddToQuery(_Q,'ifGenerationMatch',AQuery.ifGenerationMatch);
  AddToQuery(_Q,'ifGenerationNotMatch',AQuery.ifGenerationNotMatch);
  AddToQuery(_Q,'ifMetagenerationMatch',AQuery.ifMetagenerationMatch);
  AddToQuery(_Q,'ifMetagenerationNotMatch',AQuery.ifMetagenerationNotMatch);
  AddToQuery(_Q,'ifSourceGenerationMatch',AQuery.ifSourceGenerationMatch);
  AddToQuery(_Q,'ifSourceGenerationNotMatch',AQuery.ifSourceGenerationNotMatch);
  AddToQuery(_Q,'ifSourceMetagenerationMatch',AQuery.ifSourceMetagenerationMatch);
  AddToQuery(_Q,'ifSourceMetagenerationNotMatch',AQuery.ifSourceMetagenerationNotMatch);
  AddToQuery(_Q,'projection',AQuery.projection);
  AddToQuery(_Q,'sourceGeneration',AQuery.sourceGeneration);
  Result:=Copy(destinationBucket,destinationObject,sourceBucket,sourceObject,aObject,_Q);
end;

Procedure TObjectsResource.Delete(bucket: string; _object: string; AQuery : string = '');

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'b/{bucket}/o/{object}';
  _Methodid   = 'storage.objects.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket,'object',_object]);
  ServiceCall(_HTTPMethod,_P,AQuery,Nil,Nil);
end;


Procedure TObjectsResource.Delete(bucket: string; _object: string; AQuery : TObjectsdeleteOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'generation',AQuery.generation);
  AddToQuery(_Q,'ifGenerationMatch',AQuery.ifGenerationMatch);
  AddToQuery(_Q,'ifGenerationNotMatch',AQuery.ifGenerationNotMatch);
  AddToQuery(_Q,'ifMetagenerationMatch',AQuery.ifMetagenerationMatch);
  AddToQuery(_Q,'ifMetagenerationNotMatch',AQuery.ifMetagenerationNotMatch);
  Delete(bucket,_object,_Q);
end;

Function TObjectsResource.Get(bucket: string; _object: string; AQuery : string = '') : TObject;

Const
  _HTTPMethod = 'GET';
  _Path       = 'b/{bucket}/o/{object}';
  _Methodid   = 'storage.objects.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket,'object',_object]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TObject) as TObject;
end;


Function TObjectsResource.Get(bucket: string; _object: string; AQuery : TObjectsgetOptions) : TObject;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'generation',AQuery.generation);
  AddToQuery(_Q,'ifGenerationMatch',AQuery.ifGenerationMatch);
  AddToQuery(_Q,'ifGenerationNotMatch',AQuery.ifGenerationNotMatch);
  AddToQuery(_Q,'ifMetagenerationMatch',AQuery.ifMetagenerationMatch);
  AddToQuery(_Q,'ifMetagenerationNotMatch',AQuery.ifMetagenerationNotMatch);
  AddToQuery(_Q,'projection',AQuery.projection);
  Result:=Get(bucket,_object,_Q);
end;

Function TObjectsResource.Insert(bucket: string; aObject : TObject; AQuery : string = '') : TObject;

Const
  _HTTPMethod = 'POST';
  _Path       = 'b/{bucket}/o';
  _Methodid   = 'storage.objects.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aObject,TObject) as TObject;
end;


Function TObjectsResource.Insert(bucket: string; aObject : TObject; AQuery : TObjectsinsertOptions) : TObject;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'contentEncoding',AQuery.contentEncoding);
  AddToQuery(_Q,'ifGenerationMatch',AQuery.ifGenerationMatch);
  AddToQuery(_Q,'ifGenerationNotMatch',AQuery.ifGenerationNotMatch);
  AddToQuery(_Q,'ifMetagenerationMatch',AQuery.ifMetagenerationMatch);
  AddToQuery(_Q,'ifMetagenerationNotMatch',AQuery.ifMetagenerationNotMatch);
  AddToQuery(_Q,'name',AQuery._name);
  AddToQuery(_Q,'predefinedAcl',AQuery.predefinedAcl);
  AddToQuery(_Q,'projection',AQuery.projection);
  Result:=Insert(bucket,aObject,_Q);
end;

Function TObjectsResource.List(bucket: string; AQuery : string = '') : TObjects;

Const
  _HTTPMethod = 'GET';
  _Path       = 'b/{bucket}/o';
  _Methodid   = 'storage.objects.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TObjects) as TObjects;
end;


Function TObjectsResource.List(bucket: string; AQuery : TObjectslistOptions) : TObjects;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'delimiter',AQuery.delimiter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'prefix',AQuery.prefix);
  AddToQuery(_Q,'projection',AQuery.projection);
  AddToQuery(_Q,'versions',AQuery.versions);
  Result:=List(bucket,_Q);
end;

Function TObjectsResource.Patch(bucket: string; _object: string; aObject : TObject; AQuery : string = '') : TObject;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'b/{bucket}/o/{object}';
  _Methodid   = 'storage.objects.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket,'object',_object]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aObject,TObject) as TObject;
end;


Function TObjectsResource.Patch(bucket: string; _object: string; aObject : TObject; AQuery : TObjectspatchOptions) : TObject;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'generation',AQuery.generation);
  AddToQuery(_Q,'ifGenerationMatch',AQuery.ifGenerationMatch);
  AddToQuery(_Q,'ifGenerationNotMatch',AQuery.ifGenerationNotMatch);
  AddToQuery(_Q,'ifMetagenerationMatch',AQuery.ifMetagenerationMatch);
  AddToQuery(_Q,'ifMetagenerationNotMatch',AQuery.ifMetagenerationNotMatch);
  AddToQuery(_Q,'predefinedAcl',AQuery.predefinedAcl);
  AddToQuery(_Q,'projection',AQuery.projection);
  Result:=Patch(bucket,_object,aObject,_Q);
end;

Function TObjectsResource.Rewrite(destinationBucket: string; destinationObject: string; sourceBucket: string; sourceObject: string; aObject : TObject; AQuery : string = '') : TRewriteResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'b/{sourceBucket}/o/{sourceObject}/rewriteTo/b/{destinationBucket}/o/{destinationObject}';
  _Methodid   = 'storage.objects.rewrite';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['destinationBucket',destinationBucket,'destinationObject',destinationObject,'sourceBucket',sourceBucket,'sourceObject',sourceObject]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aObject,TRewriteResponse) as TRewriteResponse;
end;


Function TObjectsResource.Rewrite(destinationBucket: string; destinationObject: string; sourceBucket: string; sourceObject: string; aObject : TObject; AQuery : TObjectsrewriteOptions) : TRewriteResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'destinationPredefinedAcl',AQuery.destinationPredefinedAcl);
  AddToQuery(_Q,'ifGenerationMatch',AQuery.ifGenerationMatch);
  AddToQuery(_Q,'ifGenerationNotMatch',AQuery.ifGenerationNotMatch);
  AddToQuery(_Q,'ifMetagenerationMatch',AQuery.ifMetagenerationMatch);
  AddToQuery(_Q,'ifMetagenerationNotMatch',AQuery.ifMetagenerationNotMatch);
  AddToQuery(_Q,'ifSourceGenerationMatch',AQuery.ifSourceGenerationMatch);
  AddToQuery(_Q,'ifSourceGenerationNotMatch',AQuery.ifSourceGenerationNotMatch);
  AddToQuery(_Q,'ifSourceMetagenerationMatch',AQuery.ifSourceMetagenerationMatch);
  AddToQuery(_Q,'ifSourceMetagenerationNotMatch',AQuery.ifSourceMetagenerationNotMatch);
  AddToQuery(_Q,'maxBytesRewrittenPerCall',AQuery.maxBytesRewrittenPerCall);
  AddToQuery(_Q,'projection',AQuery.projection);
  AddToQuery(_Q,'rewriteToken',AQuery.rewriteToken);
  AddToQuery(_Q,'sourceGeneration',AQuery.sourceGeneration);
  Result:=Rewrite(destinationBucket,destinationObject,sourceBucket,sourceObject,aObject,_Q);
end;

Function TObjectsResource.Update(bucket: string; _object: string; aObject : TObject; AQuery : string = '') : TObject;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'b/{bucket}/o/{object}';
  _Methodid   = 'storage.objects.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket,'object',_object]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aObject,TObject) as TObject;
end;


Function TObjectsResource.Update(bucket: string; _object: string; aObject : TObject; AQuery : TObjectsupdateOptions) : TObject;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'generation',AQuery.generation);
  AddToQuery(_Q,'ifGenerationMatch',AQuery.ifGenerationMatch);
  AddToQuery(_Q,'ifGenerationNotMatch',AQuery.ifGenerationNotMatch);
  AddToQuery(_Q,'ifMetagenerationMatch',AQuery.ifMetagenerationMatch);
  AddToQuery(_Q,'ifMetagenerationNotMatch',AQuery.ifMetagenerationNotMatch);
  AddToQuery(_Q,'predefinedAcl',AQuery.predefinedAcl);
  AddToQuery(_Q,'projection',AQuery.projection);
  Result:=Update(bucket,_object,aObject,_Q);
end;

Function TObjectsResource.WatchAll(bucket: string; aChannel : TChannel; AQuery : string = '') : TChannel;

Const
  _HTTPMethod = 'POST';
  _Path       = 'b/{bucket}/o/watch';
  _Methodid   = 'storage.objects.watchAll';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['bucket',bucket]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aChannel,TChannel) as TChannel;
end;


Function TObjectsResource.WatchAll(bucket: string; aChannel : TChannel; AQuery : TObjectswatchAllOptions) : TChannel;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'delimiter',AQuery.delimiter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'prefix',AQuery.prefix);
  AddToQuery(_Q,'projection',AQuery.projection);
  AddToQuery(_Q,'versions',AQuery.versions);
  Result:=WatchAll(bucket,aChannel,_Q);
end;



{ --------------------------------------------------------------------
  TStorageAPI
  --------------------------------------------------------------------}

Class Function TStorageAPI.APIName : String;

begin
  Result:='storage';
end;

Class Function TStorageAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TStorageAPI.APIRevision : String;

begin
  Result:='20150326';
end;

Class Function TStorageAPI.APIID : String;

begin
  Result:='storage:v1';
end;

Class Function TStorageAPI.APITitle : String;

begin
  Result:='Cloud Storage API';
end;

Class Function TStorageAPI.APIDescription : String;

begin
  Result:='Lets you store and retrieve potentially-large, immutable data objects.';
end;

Class Function TStorageAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TStorageAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TStorageAPI.APIIcon16 : String;

begin
  Result:='https://www.google.com/images/icons/product/cloud_storage-16.png';
end;

Class Function TStorageAPI.APIIcon32 : String;

begin
  Result:='https://www.google.com/images/icons/product/cloud_storage-32.png';
end;

Class Function TStorageAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/storage/docs/json_api/';
end;

Class Function TStorageAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TStorageAPI.APIbasePath : string;

begin
  Result:='/storage/v1/';
end;

Class Function TStorageAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/storage/v1/';
end;

Class Function TStorageAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TStorageAPI.APIservicePath : string;

begin
  Result:='storage/v1/';
end;

Class Function TStorageAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TStorageAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,4);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  Result[1].Name:='https://www.googleapis.com/auth/devstorage.full_control';
  Result[1].Description:='Manage your data and permissions in Google Cloud Storage';
  Result[2].Name:='https://www.googleapis.com/auth/devstorage.read_only';
  Result[2].Description:='View your data in Google Cloud Storage';
  Result[3].Name:='https://www.googleapis.com/auth/devstorage.read_write';
  Result[3].Description:='Manage your data in Google Cloud Storage';
  
end;

Class Function TStorageAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TStorageAPI.RegisterAPIResources;

begin
  TBucket.RegisterObject;
  TBucketacl.RegisterObject;
  TBucketcors.RegisterObject;
  TBucketcorsmethod.RegisterObject;
  TBucketcorsorigin.RegisterObject;
  TBucketcorsresponseHeader.RegisterObject;
  TBucketdefaultObjectAcl.RegisterObject;
  TBucketlifecycle.RegisterObject;
  TBucketlifecyclerule.RegisterObject;
  TBucketlifecycleruleaction.RegisterObject;
  TBucketlifecyclerulecondition.RegisterObject;
  TBucketlogging.RegisterObject;
  TBucketowner.RegisterObject;
  TBucketversioning.RegisterObject;
  TBucketwebsite.RegisterObject;
  TBucketAccessControl.RegisterObject;
  TBucketAccessControlprojectTeam.RegisterObject;
  TBucketAccessControls.RegisterObject;
  TBucketAccessControlsitems.RegisterObject;
  TBuckets.RegisterObject;
  TBucketsitems.RegisterObject;
  TChannel.RegisterObject;
  TChannelparams.RegisterObject;
  TComposeRequest.RegisterObject;
  TComposeRequestsourceObjects.RegisterObject;
  TComposeRequestsourceObjectsobjectPreconditions.RegisterObject;
  TObject.RegisterObject;
  TObjectacl.RegisterObject;
  TObjectmetadata.RegisterObject;
  TObjectowner.RegisterObject;
  TObjectAccessControl.RegisterObject;
  TObjectAccessControlprojectTeam.RegisterObject;
  TObjectAccessControls.RegisterObject;
  TObjectAccessControlsitems.RegisterObject;
  TObjects.RegisterObject;
  TObjectsitems.RegisterObject;
  TObjectsprefixes.RegisterObject;
  TRewriteResponse.RegisterObject;
end;


Function TStorageAPI.GetBucketAccessControlsInstance : TBucketAccessControlsResource;

begin
  if (FBucketAccessControlsInstance=Nil) then
    FBucketAccessControlsInstance:=CreateBucketAccessControlsResource;
  Result:=FBucketAccessControlsInstance;
end;

Function TStorageAPI.CreateBucketAccessControlsResource : TBucketAccessControlsResource;

begin
  Result:=CreateBucketAccessControlsResource(Self);
end;


Function TStorageAPI.CreateBucketAccessControlsResource(AOwner : TComponent) : TBucketAccessControlsResource;

begin
  Result:=TBucketAccessControlsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TStorageAPI.GetBucketsInstance : TBucketsResource;

begin
  if (FBucketsInstance=Nil) then
    FBucketsInstance:=CreateBucketsResource;
  Result:=FBucketsInstance;
end;

Function TStorageAPI.CreateBucketsResource : TBucketsResource;

begin
  Result:=CreateBucketsResource(Self);
end;


Function TStorageAPI.CreateBucketsResource(AOwner : TComponent) : TBucketsResource;

begin
  Result:=TBucketsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TStorageAPI.GetChannelsInstance : TChannelsResource;

begin
  if (FChannelsInstance=Nil) then
    FChannelsInstance:=CreateChannelsResource;
  Result:=FChannelsInstance;
end;

Function TStorageAPI.CreateChannelsResource : TChannelsResource;

begin
  Result:=CreateChannelsResource(Self);
end;


Function TStorageAPI.CreateChannelsResource(AOwner : TComponent) : TChannelsResource;

begin
  Result:=TChannelsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TStorageAPI.GetDefaultObjectAccessControlsInstance : TDefaultObjectAccessControlsResource;

begin
  if (FDefaultObjectAccessControlsInstance=Nil) then
    FDefaultObjectAccessControlsInstance:=CreateDefaultObjectAccessControlsResource;
  Result:=FDefaultObjectAccessControlsInstance;
end;

Function TStorageAPI.CreateDefaultObjectAccessControlsResource : TDefaultObjectAccessControlsResource;

begin
  Result:=CreateDefaultObjectAccessControlsResource(Self);
end;


Function TStorageAPI.CreateDefaultObjectAccessControlsResource(AOwner : TComponent) : TDefaultObjectAccessControlsResource;

begin
  Result:=TDefaultObjectAccessControlsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TStorageAPI.GetObjectAccessControlsInstance : TObjectAccessControlsResource;

begin
  if (FObjectAccessControlsInstance=Nil) then
    FObjectAccessControlsInstance:=CreateObjectAccessControlsResource;
  Result:=FObjectAccessControlsInstance;
end;

Function TStorageAPI.CreateObjectAccessControlsResource : TObjectAccessControlsResource;

begin
  Result:=CreateObjectAccessControlsResource(Self);
end;


Function TStorageAPI.CreateObjectAccessControlsResource(AOwner : TComponent) : TObjectAccessControlsResource;

begin
  Result:=TObjectAccessControlsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TStorageAPI.GetObjectsInstance : TObjectsResource;

begin
  if (FObjectsInstance=Nil) then
    FObjectsInstance:=CreateObjectsResource;
  Result:=FObjectsInstance;
end;

Function TStorageAPI.CreateObjectsResource : TObjectsResource;

begin
  Result:=CreateObjectsResource(Self);
end;


Function TStorageAPI.CreateObjectsResource(AOwner : TComponent) : TObjectsResource;

begin
  Result:=TObjectsResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TStorageAPI.RegisterAPI;
end.
