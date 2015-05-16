unit googlestorage;
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
  TBucket = Class;
  TBucketAccessControl = Class;
  TBucketAccessControls = Class;
  TBuckets = Class;
  TChannel = Class;
  TComposeRequest = Class;
  TObject = Class;
  TObjectAccessControl = Class;
  TObjectAccessControls = Class;
  TObjects = Class;
  TRewriteResponse = Class;
  TBucketArray = Array of TBucket;
  TBucketAccessControlArray = Array of TBucketAccessControl;
  TBucketAccessControlsArray = Array of TBucketAccessControls;
  TBucketsArray = Array of TBuckets;
  TChannelArray = Array of TChannel;
  TComposeRequestArray = Array of TComposeRequest;
  TObjectArray = Array of TObject;
  TObjectAccessControlArray = Array of TObjectAccessControl;
  TObjectAccessControlsArray = Array of TObjectAccessControls;
  TObjectsArray = Array of TObjects;
  TRewriteResponseArray = Array of TRewriteResponse;
  //Anonymous types, using auto-generated names
  TBucketTypecorsItem = Class;
  TBucketTypelifecycleTyperuleItemTypeaction = Class;
  TBucketTypelifecycleTyperuleItemTypecondition = Class;
  TBucketTypelifecycleTyperuleItem = Class;
  TBucketTypelifecycle = Class;
  TBucketTypelogging = Class;
  TBucketTypeowner = Class;
  TBucketTypeversioning = Class;
  TBucketTypewebsite = Class;
  TBucketAccessControlTypeprojectTeam = Class;
  TChannelTypeparams = Class;
  TComposeRequestTypesourceObjectsItemTypeobjectPreconditions = Class;
  TComposeRequestTypesourceObjectsItem = Class;
  TObjectTypemetadata = Class;
  TObjectTypeowner = Class;
  TObjectAccessControlTypeprojectTeam = Class;
  TBucketTypeaclArray = Array of TBucketAccessControl;
  TBucketTypecorsArray = Array of TBucketTypecorsItem;
  TBucketTypedefaultObjectAclArray = Array of TObjectAccessControl;
  TBucketTypelifecycleTyperuleArray = Array of TBucketTypelifecycleTyperuleItem;
  TBucketAccessControlsTypeitemsArray = Array of TBucketAccessControl;
  TBucketsTypeitemsArray = Array of TBucket;
  TComposeRequestTypesourceObjectsArray = Array of TComposeRequestTypesourceObjectsItem;
  TObjectTypeaclArray = Array of TObjectAccessControl;
  TObjectsTypeitemsArray = Array of TObject;
  
  { --------------------------------------------------------------------
    TBucketTypecorsItem
    --------------------------------------------------------------------}
  
  TBucketTypecorsItem = Class(TGoogleBaseObject)
  Private
    FmaxAgeSeconds : integer;
    Fmethod : TStringArray;
    Forigin : TStringArray;
    FresponseHeader : TStringArray;
  Protected
    //Property setters
    Procedure SetmaxAgeSeconds(AIndex : Integer; AValue : integer); virtual;
    Procedure Setmethod(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setorigin(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetresponseHeader(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property maxAgeSeconds : integer Index 0 Read FmaxAgeSeconds Write SetmaxAgeSeconds;
    Property method : TStringArray Index 8 Read Fmethod Write Setmethod;
    Property origin : TStringArray Index 16 Read Forigin Write Setorigin;
    Property responseHeader : TStringArray Index 24 Read FresponseHeader Write SetresponseHeader;
  end;
  TBucketTypecorsItemClass = Class of TBucketTypecorsItem;
  
  { --------------------------------------------------------------------
    TBucketTypelifecycleTyperuleItemTypeaction
    --------------------------------------------------------------------}
  
  TBucketTypelifecycleTyperuleItemTypeaction = Class(TGoogleBaseObject)
  Private
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property _type : String Index 0 Read F_type Write Set_type;
  end;
  TBucketTypelifecycleTyperuleItemTypeactionClass = Class of TBucketTypelifecycleTyperuleItemTypeaction;
  
  { --------------------------------------------------------------------
    TBucketTypelifecycleTyperuleItemTypecondition
    --------------------------------------------------------------------}
  
  TBucketTypelifecycleTyperuleItemTypecondition = Class(TGoogleBaseObject)
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
  TBucketTypelifecycleTyperuleItemTypeconditionClass = Class of TBucketTypelifecycleTyperuleItemTypecondition;
  
  { --------------------------------------------------------------------
    TBucketTypelifecycleTyperuleItem
    --------------------------------------------------------------------}
  
  TBucketTypelifecycleTyperuleItem = Class(TGoogleBaseObject)
  Private
    Faction : TBucketTypelifecycleTyperuleItemTypeaction;
    Fcondition : TBucketTypelifecycleTyperuleItemTypecondition;
  Protected
    //Property setters
    Procedure Setaction(AIndex : Integer; AValue : TBucketTypelifecycleTyperuleItemTypeaction); virtual;
    Procedure Setcondition(AIndex : Integer; AValue : TBucketTypelifecycleTyperuleItemTypecondition); virtual;
  Public
  Published
    Property action : TBucketTypelifecycleTyperuleItemTypeaction Index 0 Read Faction Write Setaction;
    Property condition : TBucketTypelifecycleTyperuleItemTypecondition Index 8 Read Fcondition Write Setcondition;
  end;
  TBucketTypelifecycleTyperuleItemClass = Class of TBucketTypelifecycleTyperuleItem;
  
  { --------------------------------------------------------------------
    TBucketTypelifecycle
    --------------------------------------------------------------------}
  
  TBucketTypelifecycle = Class(TGoogleBaseObject)
  Private
    Frule : TBucketTypelifecycleTyperuleArray;
  Protected
    //Property setters
    Procedure Setrule(AIndex : Integer; AValue : TBucketTypelifecycleTyperuleArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property rule : TBucketTypelifecycleTyperuleArray Index 0 Read Frule Write Setrule;
  end;
  TBucketTypelifecycleClass = Class of TBucketTypelifecycle;
  
  { --------------------------------------------------------------------
    TBucketTypelogging
    --------------------------------------------------------------------}
  
  TBucketTypelogging = Class(TGoogleBaseObject)
  Private
    FlogBucket : String;
    FlogObjectPrefix : String;
  Protected
    //Property setters
    Procedure SetlogBucket(AIndex : Integer; AValue : String); virtual;
    Procedure SetlogObjectPrefix(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property logBucket : String Index 0 Read FlogBucket Write SetlogBucket;
    Property logObjectPrefix : String Index 8 Read FlogObjectPrefix Write SetlogObjectPrefix;
  end;
  TBucketTypeloggingClass = Class of TBucketTypelogging;
  
  { --------------------------------------------------------------------
    TBucketTypeowner
    --------------------------------------------------------------------}
  
  TBucketTypeowner = Class(TGoogleBaseObject)
  Private
    Fentity : String;
    FentityId : String;
  Protected
    //Property setters
    Procedure Setentity(AIndex : Integer; AValue : String); virtual;
    Procedure SetentityId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property entity : String Index 0 Read Fentity Write Setentity;
    Property entityId : String Index 8 Read FentityId Write SetentityId;
  end;
  TBucketTypeownerClass = Class of TBucketTypeowner;
  
  { --------------------------------------------------------------------
    TBucketTypeversioning
    --------------------------------------------------------------------}
  
  TBucketTypeversioning = Class(TGoogleBaseObject)
  Private
    Fenabled : boolean;
  Protected
    //Property setters
    Procedure Setenabled(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property enabled : boolean Index 0 Read Fenabled Write Setenabled;
  end;
  TBucketTypeversioningClass = Class of TBucketTypeversioning;
  
  { --------------------------------------------------------------------
    TBucketTypewebsite
    --------------------------------------------------------------------}
  
  TBucketTypewebsite = Class(TGoogleBaseObject)
  Private
    FmainPageSuffix : String;
    FnotFoundPage : String;
  Protected
    //Property setters
    Procedure SetmainPageSuffix(AIndex : Integer; AValue : String); virtual;
    Procedure SetnotFoundPage(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property mainPageSuffix : String Index 0 Read FmainPageSuffix Write SetmainPageSuffix;
    Property notFoundPage : String Index 8 Read FnotFoundPage Write SetnotFoundPage;
  end;
  TBucketTypewebsiteClass = Class of TBucketTypewebsite;
  
  { --------------------------------------------------------------------
    TBucket
    --------------------------------------------------------------------}
  
  TBucket = Class(TGoogleBaseObject)
  Private
    Facl : TBucketTypeaclArray;
    Fcors : TBucketTypecorsArray;
    FdefaultObjectAcl : TBucketTypedefaultObjectAclArray;
    Fetag : String;
    Fid : String;
    Fkind : String;
    Flifecycle : TBucketTypelifecycle;
    Flocation : String;
    Flogging : TBucketTypelogging;
    Fmetageneration : String;
    Fname : String;
    Fowner : TBucketTypeowner;
    FprojectNumber : String;
    FselfLink : String;
    FstorageClass : String;
    FtimeCreated : TDatetime;
    Fversioning : TBucketTypeversioning;
    Fwebsite : TBucketTypewebsite;
  Protected
    //Property setters
    Procedure Setacl(AIndex : Integer; AValue : TBucketTypeaclArray); virtual;
    Procedure Setcors(AIndex : Integer; AValue : TBucketTypecorsArray); virtual;
    Procedure SetdefaultObjectAcl(AIndex : Integer; AValue : TBucketTypedefaultObjectAclArray); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlifecycle(AIndex : Integer; AValue : TBucketTypelifecycle); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : String); virtual;
    Procedure Setlogging(AIndex : Integer; AValue : TBucketTypelogging); virtual;
    Procedure Setmetageneration(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setowner(AIndex : Integer; AValue : TBucketTypeowner); virtual;
    Procedure SetprojectNumber(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetstorageClass(AIndex : Integer; AValue : String); virtual;
    Procedure SettimeCreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setversioning(AIndex : Integer; AValue : TBucketTypeversioning); virtual;
    Procedure Setwebsite(AIndex : Integer; AValue : TBucketTypewebsite); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property acl : TBucketTypeaclArray Index 0 Read Facl Write Setacl;
    Property cors : TBucketTypecorsArray Index 8 Read Fcors Write Setcors;
    Property defaultObjectAcl : TBucketTypedefaultObjectAclArray Index 16 Read FdefaultObjectAcl Write SetdefaultObjectAcl;
    Property etag : String Index 24 Read Fetag Write Setetag;
    Property id : String Index 32 Read Fid Write Setid;
    Property kind : String Index 40 Read Fkind Write Setkind;
    Property lifecycle : TBucketTypelifecycle Index 48 Read Flifecycle Write Setlifecycle;
    Property location : String Index 56 Read Flocation Write Setlocation;
    Property logging : TBucketTypelogging Index 64 Read Flogging Write Setlogging;
    Property metageneration : String Index 72 Read Fmetageneration Write Setmetageneration;
    Property name : String Index 80 Read Fname Write Setname;
    Property owner : TBucketTypeowner Index 88 Read Fowner Write Setowner;
    Property projectNumber : String Index 96 Read FprojectNumber Write SetprojectNumber;
    Property selfLink : String Index 104 Read FselfLink Write SetselfLink;
    Property storageClass : String Index 112 Read FstorageClass Write SetstorageClass;
    Property timeCreated : TDatetime Index 120 Read FtimeCreated Write SettimeCreated;
    Property versioning : TBucketTypeversioning Index 128 Read Fversioning Write Setversioning;
    Property website : TBucketTypewebsite Index 136 Read Fwebsite Write Setwebsite;
  end;
  TBucketClass = Class of TBucket;
  
  { --------------------------------------------------------------------
    TBucketAccessControlTypeprojectTeam
    --------------------------------------------------------------------}
  
  TBucketAccessControlTypeprojectTeam = Class(TGoogleBaseObject)
  Private
    FprojectNumber : String;
    Fteam : String;
  Protected
    //Property setters
    Procedure SetprojectNumber(AIndex : Integer; AValue : String); virtual;
    Procedure Setteam(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property projectNumber : String Index 0 Read FprojectNumber Write SetprojectNumber;
    Property team : String Index 8 Read Fteam Write Setteam;
  end;
  TBucketAccessControlTypeprojectTeamClass = Class of TBucketAccessControlTypeprojectTeam;
  
  { --------------------------------------------------------------------
    TBucketAccessControl
    --------------------------------------------------------------------}
  
  TBucketAccessControl = Class(TGoogleBaseObject)
  Private
    Fbucket : String;
    Fdomain : String;
    Femail : String;
    Fentity : String;
    FentityId : String;
    Fetag : String;
    Fid : String;
    Fkind : String;
    FprojectTeam : TBucketAccessControlTypeprojectTeam;
    Frole : String;
    FselfLink : String;
  Protected
    //Property setters
    Procedure Setbucket(AIndex : Integer; AValue : String); virtual;
    Procedure Setdomain(AIndex : Integer; AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; AValue : String); virtual;
    Procedure Setentity(AIndex : Integer; AValue : String); virtual;
    Procedure SetentityId(AIndex : Integer; AValue : String); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetprojectTeam(AIndex : Integer; AValue : TBucketAccessControlTypeprojectTeam); virtual;
    Procedure Setrole(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property bucket : String Index 0 Read Fbucket Write Setbucket;
    Property domain : String Index 8 Read Fdomain Write Setdomain;
    Property email : String Index 16 Read Femail Write Setemail;
    Property entity : String Index 24 Read Fentity Write Setentity;
    Property entityId : String Index 32 Read FentityId Write SetentityId;
    Property etag : String Index 40 Read Fetag Write Setetag;
    Property id : String Index 48 Read Fid Write Setid;
    Property kind : String Index 56 Read Fkind Write Setkind;
    Property projectTeam : TBucketAccessControlTypeprojectTeam Index 64 Read FprojectTeam Write SetprojectTeam;
    Property role : String Index 72 Read Frole Write Setrole;
    Property selfLink : String Index 80 Read FselfLink Write SetselfLink;
  end;
  TBucketAccessControlClass = Class of TBucketAccessControl;
  
  { --------------------------------------------------------------------
    TBucketAccessControls
    --------------------------------------------------------------------}
  
  TBucketAccessControls = Class(TGoogleBaseObject)
  Private
    Fitems : TBucketAccessControlsTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TBucketAccessControlsTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TBucketAccessControlsTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TBucketAccessControlsClass = Class of TBucketAccessControls;
  
  { --------------------------------------------------------------------
    TBuckets
    --------------------------------------------------------------------}
  
  TBuckets = Class(TGoogleBaseObject)
  Private
    Fitems : TBucketsTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TBucketsTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TBucketsTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TBucketsClass = Class of TBuckets;
  
  { --------------------------------------------------------------------
    TChannelTypeparams
    --------------------------------------------------------------------}
  
  TChannelTypeparams = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TChannelTypeparamsClass = Class of TChannelTypeparams;
  
  { --------------------------------------------------------------------
    TChannel
    --------------------------------------------------------------------}
  
  TChannel = Class(TGoogleBaseObject)
  Private
    Faddress : String;
    Fexpiration : String;
    Fid : String;
    Fkind : String;
    Fparams : TChannelTypeparams;
    Fpayload : boolean;
    FresourceId : String;
    FresourceUri : String;
    Ftoken : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setaddress(AIndex : Integer; AValue : String); virtual;
    Procedure Setexpiration(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setparams(AIndex : Integer; AValue : TChannelTypeparams); virtual;
    Procedure Setpayload(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetresourceId(AIndex : Integer; AValue : String); virtual;
    Procedure SetresourceUri(AIndex : Integer; AValue : String); virtual;
    Procedure Settoken(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property address : String Index 0 Read Faddress Write Setaddress;
    Property expiration : String Index 8 Read Fexpiration Write Setexpiration;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property params : TChannelTypeparams Index 32 Read Fparams Write Setparams;
    Property payload : boolean Index 40 Read Fpayload Write Setpayload;
    Property resourceId : String Index 48 Read FresourceId Write SetresourceId;
    Property resourceUri : String Index 56 Read FresourceUri Write SetresourceUri;
    Property token : String Index 64 Read Ftoken Write Settoken;
    Property _type : String Index 72 Read F_type Write Set_type;
  end;
  TChannelClass = Class of TChannel;
  
  { --------------------------------------------------------------------
    TComposeRequestTypesourceObjectsItemTypeobjectPreconditions
    --------------------------------------------------------------------}
  
  TComposeRequestTypesourceObjectsItemTypeobjectPreconditions = Class(TGoogleBaseObject)
  Private
    FifGenerationMatch : String;
  Protected
    //Property setters
    Procedure SetifGenerationMatch(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property ifGenerationMatch : String Index 0 Read FifGenerationMatch Write SetifGenerationMatch;
  end;
  TComposeRequestTypesourceObjectsItemTypeobjectPreconditionsClass = Class of TComposeRequestTypesourceObjectsItemTypeobjectPreconditions;
  
  { --------------------------------------------------------------------
    TComposeRequestTypesourceObjectsItem
    --------------------------------------------------------------------}
  
  TComposeRequestTypesourceObjectsItem = Class(TGoogleBaseObject)
  Private
    Fgeneration : String;
    Fname : String;
    FobjectPreconditions : TComposeRequestTypesourceObjectsItemTypeobjectPreconditions;
  Protected
    //Property setters
    Procedure Setgeneration(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetobjectPreconditions(AIndex : Integer; AValue : TComposeRequestTypesourceObjectsItemTypeobjectPreconditions); virtual;
  Public
  Published
    Property generation : String Index 0 Read Fgeneration Write Setgeneration;
    Property name : String Index 8 Read Fname Write Setname;
    Property objectPreconditions : TComposeRequestTypesourceObjectsItemTypeobjectPreconditions Index 16 Read FobjectPreconditions Write SetobjectPreconditions;
  end;
  TComposeRequestTypesourceObjectsItemClass = Class of TComposeRequestTypesourceObjectsItem;
  
  { --------------------------------------------------------------------
    TComposeRequest
    --------------------------------------------------------------------}
  
  TComposeRequest = Class(TGoogleBaseObject)
  Private
    Fdestination : TObject;
    Fkind : String;
    FsourceObjects : TComposeRequestTypesourceObjectsArray;
  Protected
    //Property setters
    Procedure Setdestination(AIndex : Integer; AValue : TObject); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetsourceObjects(AIndex : Integer; AValue : TComposeRequestTypesourceObjectsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property destination : TObject Index 0 Read Fdestination Write Setdestination;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property sourceObjects : TComposeRequestTypesourceObjectsArray Index 16 Read FsourceObjects Write SetsourceObjects;
  end;
  TComposeRequestClass = Class of TComposeRequest;
  
  { --------------------------------------------------------------------
    TObjectTypemetadata
    --------------------------------------------------------------------}
  
  TObjectTypemetadata = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TObjectTypemetadataClass = Class of TObjectTypemetadata;
  
  { --------------------------------------------------------------------
    TObjectTypeowner
    --------------------------------------------------------------------}
  
  TObjectTypeowner = Class(TGoogleBaseObject)
  Private
    Fentity : String;
    FentityId : String;
  Protected
    //Property setters
    Procedure Setentity(AIndex : Integer; AValue : String); virtual;
    Procedure SetentityId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property entity : String Index 0 Read Fentity Write Setentity;
    Property entityId : String Index 8 Read FentityId Write SetentityId;
  end;
  TObjectTypeownerClass = Class of TObjectTypeowner;
  
  { --------------------------------------------------------------------
    TObject
    --------------------------------------------------------------------}
  
  TObject = Class(TGoogleBaseObject)
  Private
    Facl : TObjectTypeaclArray;
    Fbucket : String;
    FcacheControl : String;
    FcomponentCount : integer;
    FcontentDisposition : String;
    FcontentEncoding : String;
    FcontentLanguage : String;
    FcontentType : String;
    Fcrc32c : String;
    Fetag : String;
    Fgeneration : String;
    Fid : String;
    Fkind : String;
    Fmd5Hash : String;
    FmediaLink : String;
    Fmetadata : TObjectTypemetadata;
    Fmetageneration : String;
    Fname : String;
    Fowner : TObjectTypeowner;
    FselfLink : String;
    Fsize : String;
    FstorageClass : String;
    FtimeDeleted : TDatetime;
    Fupdated : TDatetime;
  Protected
    //Property setters
    Procedure Setacl(AIndex : Integer; AValue : TObjectTypeaclArray); virtual;
    Procedure Setbucket(AIndex : Integer; AValue : String); virtual;
    Procedure SetcacheControl(AIndex : Integer; AValue : String); virtual;
    Procedure SetcomponentCount(AIndex : Integer; AValue : integer); virtual;
    Procedure SetcontentDisposition(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontentEncoding(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontentLanguage(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontentType(AIndex : Integer; AValue : String); virtual;
    Procedure Setcrc32c(AIndex : Integer; AValue : String); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setgeneration(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setmd5Hash(AIndex : Integer; AValue : String); virtual;
    Procedure SetmediaLink(AIndex : Integer; AValue : String); virtual;
    Procedure Setmetadata(AIndex : Integer; AValue : TObjectTypemetadata); virtual;
    Procedure Setmetageneration(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setowner(AIndex : Integer; AValue : TObjectTypeowner); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure Setsize(AIndex : Integer; AValue : String); virtual;
    Procedure SetstorageClass(AIndex : Integer; AValue : String); virtual;
    Procedure SettimeDeleted(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property acl : TObjectTypeaclArray Index 0 Read Facl Write Setacl;
    Property bucket : String Index 8 Read Fbucket Write Setbucket;
    Property cacheControl : String Index 16 Read FcacheControl Write SetcacheControl;
    Property componentCount : integer Index 24 Read FcomponentCount Write SetcomponentCount;
    Property contentDisposition : String Index 32 Read FcontentDisposition Write SetcontentDisposition;
    Property contentEncoding : String Index 40 Read FcontentEncoding Write SetcontentEncoding;
    Property contentLanguage : String Index 48 Read FcontentLanguage Write SetcontentLanguage;
    Property contentType : String Index 56 Read FcontentType Write SetcontentType;
    Property crc32c : String Index 64 Read Fcrc32c Write Setcrc32c;
    Property etag : String Index 72 Read Fetag Write Setetag;
    Property generation : String Index 80 Read Fgeneration Write Setgeneration;
    Property id : String Index 88 Read Fid Write Setid;
    Property kind : String Index 96 Read Fkind Write Setkind;
    Property md5Hash : String Index 104 Read Fmd5Hash Write Setmd5Hash;
    Property mediaLink : String Index 112 Read FmediaLink Write SetmediaLink;
    Property metadata : TObjectTypemetadata Index 120 Read Fmetadata Write Setmetadata;
    Property metageneration : String Index 128 Read Fmetageneration Write Setmetageneration;
    Property name : String Index 136 Read Fname Write Setname;
    Property owner : TObjectTypeowner Index 144 Read Fowner Write Setowner;
    Property selfLink : String Index 152 Read FselfLink Write SetselfLink;
    Property size : String Index 160 Read Fsize Write Setsize;
    Property storageClass : String Index 168 Read FstorageClass Write SetstorageClass;
    Property timeDeleted : TDatetime Index 176 Read FtimeDeleted Write SettimeDeleted;
    Property updated : TDatetime Index 184 Read Fupdated Write Setupdated;
  end;
  TObjectClass = Class of TObject;
  
  { --------------------------------------------------------------------
    TObjectAccessControlTypeprojectTeam
    --------------------------------------------------------------------}
  
  TObjectAccessControlTypeprojectTeam = Class(TGoogleBaseObject)
  Private
    FprojectNumber : String;
    Fteam : String;
  Protected
    //Property setters
    Procedure SetprojectNumber(AIndex : Integer; AValue : String); virtual;
    Procedure Setteam(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property projectNumber : String Index 0 Read FprojectNumber Write SetprojectNumber;
    Property team : String Index 8 Read Fteam Write Setteam;
  end;
  TObjectAccessControlTypeprojectTeamClass = Class of TObjectAccessControlTypeprojectTeam;
  
  { --------------------------------------------------------------------
    TObjectAccessControl
    --------------------------------------------------------------------}
  
  TObjectAccessControl = Class(TGoogleBaseObject)
  Private
    Fbucket : String;
    Fdomain : String;
    Femail : String;
    Fentity : String;
    FentityId : String;
    Fetag : String;
    Fgeneration : String;
    Fid : String;
    Fkind : String;
    F_object : String;
    FprojectTeam : TObjectAccessControlTypeprojectTeam;
    Frole : String;
    FselfLink : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setbucket(AIndex : Integer; AValue : String); virtual;
    Procedure Setdomain(AIndex : Integer; AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; AValue : String); virtual;
    Procedure Setentity(AIndex : Integer; AValue : String); virtual;
    Procedure SetentityId(AIndex : Integer; AValue : String); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setgeneration(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Set_object(AIndex : Integer; AValue : String); virtual;
    Procedure SetprojectTeam(AIndex : Integer; AValue : TObjectAccessControlTypeprojectTeam); virtual;
    Procedure Setrole(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property bucket : String Index 0 Read Fbucket Write Setbucket;
    Property domain : String Index 8 Read Fdomain Write Setdomain;
    Property email : String Index 16 Read Femail Write Setemail;
    Property entity : String Index 24 Read Fentity Write Setentity;
    Property entityId : String Index 32 Read FentityId Write SetentityId;
    Property etag : String Index 40 Read Fetag Write Setetag;
    Property generation : String Index 48 Read Fgeneration Write Setgeneration;
    Property id : String Index 56 Read Fid Write Setid;
    Property kind : String Index 64 Read Fkind Write Setkind;
    Property _object : String Index 72 Read F_object Write Set_object;
    Property projectTeam : TObjectAccessControlTypeprojectTeam Index 80 Read FprojectTeam Write SetprojectTeam;
    Property role : String Index 88 Read Frole Write Setrole;
    Property selfLink : String Index 96 Read FselfLink Write SetselfLink;
  end;
  TObjectAccessControlClass = Class of TObjectAccessControl;
  
  { --------------------------------------------------------------------
    TObjectAccessControls
    --------------------------------------------------------------------}
  
  TObjectAccessControls = Class(TGoogleBaseObject)
  Private
    Fitems : TTJSONSchemaArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TTJSONSchemaArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TTJSONSchemaArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TObjectAccessControlsClass = Class of TObjectAccessControls;
  
  { --------------------------------------------------------------------
    TObjects
    --------------------------------------------------------------------}
  
  TObjects = Class(TGoogleBaseObject)
  Private
    Fitems : TObjectsTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    Fprefixes : TStringArray;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TObjectsTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setprefixes(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TObjectsTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
    Property prefixes : TStringArray Index 24 Read Fprefixes Write Setprefixes;
  end;
  TObjectsClass = Class of TObjects;
  
  { --------------------------------------------------------------------
    TRewriteResponse
    --------------------------------------------------------------------}
  
  TRewriteResponse = Class(TGoogleBaseObject)
  Private
    Fdone : boolean;
    Fkind : String;
    FobjectSize : String;
    Fresource : TObject;
    FrewriteToken : String;
    FtotalBytesRewritten : String;
  Protected
    //Property setters
    Procedure Setdone(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetobjectSize(AIndex : Integer; AValue : String); virtual;
    Procedure Setresource(AIndex : Integer; AValue : TObject); virtual;
    Procedure SetrewriteToken(AIndex : Integer; AValue : String); virtual;
    Procedure SettotalBytesRewritten(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property done : boolean Index 0 Read Fdone Write Setdone;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property objectSize : String Index 16 Read FobjectSize Write SetobjectSize;
    Property resource : TObject Index 24 Read Fresource Write Setresource;
    Property rewriteToken : String Index 32 Read FrewriteToken Write SetrewriteToken;
    Property totalBytesRewritten : String Index 40 Read FtotalBytesRewritten Write SettotalBytesRewritten;
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
    projection : String;
  end;
  
  
  //Optional query Options for TBucketsResource, method Insert
  
  TBucketsInsertOptions = Record
    predefinedAcl : String;
    predefinedDefaultObjectAcl : String;
    project : String;
    projection : String;
  end;
  
  
  //Optional query Options for TBucketsResource, method List
  
  TBucketsListOptions = Record
    maxResults : integer;
    pageToken : String;
    prefix : String;
    project : String;
    projection : String;
  end;
  
  
  //Optional query Options for TBucketsResource, method Patch
  
  TBucketsPatchOptions = Record
    ifMetagenerationMatch : int64;
    ifMetagenerationNotMatch : int64;
    predefinedAcl : String;
    predefinedDefaultObjectAcl : String;
    projection : String;
  end;
  
  
  //Optional query Options for TBucketsResource, method Update
  
  TBucketsUpdateOptions = Record
    ifMetagenerationMatch : int64;
    ifMetagenerationNotMatch : int64;
    predefinedAcl : String;
    predefinedDefaultObjectAcl : String;
    projection : String;
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
    destinationPredefinedAcl : String;
    ifGenerationMatch : int64;
    ifMetagenerationMatch : int64;
  end;
  
  
  //Optional query Options for TObjectsResource, method Copy
  
  TObjectsCopyOptions = Record
    destinationPredefinedAcl : String;
    ifGenerationMatch : int64;
    ifGenerationNotMatch : int64;
    ifMetagenerationMatch : int64;
    ifMetagenerationNotMatch : int64;
    ifSourceGenerationMatch : int64;
    ifSourceGenerationNotMatch : int64;
    ifSourceMetagenerationMatch : int64;
    ifSourceMetagenerationNotMatch : int64;
    projection : String;
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
    projection : String;
  end;
  
  
  //Optional query Options for TObjectsResource, method Insert
  
  TObjectsInsertOptions = Record
    contentEncoding : String;
    ifGenerationMatch : int64;
    ifGenerationNotMatch : int64;
    ifMetagenerationMatch : int64;
    ifMetagenerationNotMatch : int64;
    _name : String;
    predefinedAcl : String;
    projection : String;
  end;
  
  
  //Optional query Options for TObjectsResource, method List
  
  TObjectsListOptions = Record
    delimiter : String;
    maxResults : integer;
    pageToken : String;
    prefix : String;
    projection : String;
    versions : boolean;
  end;
  
  
  //Optional query Options for TObjectsResource, method Patch
  
  TObjectsPatchOptions = Record
    generation : int64;
    ifGenerationMatch : int64;
    ifGenerationNotMatch : int64;
    ifMetagenerationMatch : int64;
    ifMetagenerationNotMatch : int64;
    predefinedAcl : String;
    projection : String;
  end;
  
  
  //Optional query Options for TObjectsResource, method Rewrite
  
  TObjectsRewriteOptions = Record
    destinationPredefinedAcl : String;
    ifGenerationMatch : int64;
    ifGenerationNotMatch : int64;
    ifMetagenerationMatch : int64;
    ifMetagenerationNotMatch : int64;
    ifSourceGenerationMatch : int64;
    ifSourceGenerationNotMatch : int64;
    ifSourceMetagenerationMatch : int64;
    ifSourceMetagenerationNotMatch : int64;
    maxBytesRewrittenPerCall : int64;
    projection : String;
    rewriteToken : String;
    sourceGeneration : int64;
  end;
  
  
  //Optional query Options for TObjectsResource, method Update
  
  TObjectsUpdateOptions = Record
    generation : int64;
    ifGenerationMatch : int64;
    ifGenerationNotMatch : int64;
    ifMetagenerationMatch : int64;
    ifMetagenerationNotMatch : int64;
    predefinedAcl : String;
    projection : String;
  end;
  
  
  //Optional query Options for TObjectsResource, method WatchAll
  
  TObjectsWatchAllOptions = Record
    delimiter : String;
    maxResults : integer;
    pageToken : String;
    prefix : String;
    projection : String;
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
  TBucketTypecorsItem
  --------------------------------------------------------------------}


Procedure TBucketTypecorsItem.SetmaxAgeSeconds(AIndex : Integer; AValue : integer); 

begin
  If (FmaxAgeSeconds=AValue) then exit;
  FmaxAgeSeconds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketTypecorsItem.Setmethod(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fmethod=AValue) then exit;
  Fmethod:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketTypecorsItem.Setorigin(AIndex : Integer; AValue : TStringArray); 

begin
  If (Forigin=AValue) then exit;
  Forigin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketTypecorsItem.SetresponseHeader(AIndex : Integer; AValue : TStringArray); 

begin
  If (FresponseHeader=AValue) then exit;
  FresponseHeader:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBucketTypecorsItem.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'method' : SetLength(Fmethod,ALength);
  'origin' : SetLength(Forigin,ALength);
  'responseheader' : SetLength(FresponseHeader,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBucketTypelifecycleTyperuleItemTypeaction
  --------------------------------------------------------------------}


Procedure TBucketTypelifecycleTyperuleItemTypeaction.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TBucketTypelifecycleTyperuleItemTypeaction.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TBucketTypelifecycleTyperuleItemTypecondition
  --------------------------------------------------------------------}


Procedure TBucketTypelifecycleTyperuleItemTypecondition.Setage(AIndex : Integer; AValue : integer); 

begin
  If (Fage=AValue) then exit;
  Fage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketTypelifecycleTyperuleItemTypecondition.SetcreatedBefore(AIndex : Integer; AValue : TDate); 

begin
  If (FcreatedBefore=AValue) then exit;
  FcreatedBefore:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketTypelifecycleTyperuleItemTypecondition.SetisLive(AIndex : Integer; AValue : boolean); 

begin
  If (FisLive=AValue) then exit;
  FisLive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketTypelifecycleTyperuleItemTypecondition.SetnumNewerVersions(AIndex : Integer; AValue : integer); 

begin
  If (FnumNewerVersions=AValue) then exit;
  FnumNewerVersions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBucketTypelifecycleTyperuleItem
  --------------------------------------------------------------------}


Procedure TBucketTypelifecycleTyperuleItem.Setaction(AIndex : Integer; AValue : TBucketTypelifecycleTyperuleItemTypeaction); 

begin
  If (Faction=AValue) then exit;
  Faction:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketTypelifecycleTyperuleItem.Setcondition(AIndex : Integer; AValue : TBucketTypelifecycleTyperuleItemTypecondition); 

begin
  If (Fcondition=AValue) then exit;
  Fcondition:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBucketTypelifecycle
  --------------------------------------------------------------------}


Procedure TBucketTypelifecycle.Setrule(AIndex : Integer; AValue : TBucketTypelifecycleTyperuleArray); 

begin
  If (Frule=AValue) then exit;
  Frule:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBucketTypelifecycle.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'rule' : SetLength(Frule,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBucketTypelogging
  --------------------------------------------------------------------}


Procedure TBucketTypelogging.SetlogBucket(AIndex : Integer; AValue : String); 

begin
  If (FlogBucket=AValue) then exit;
  FlogBucket:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketTypelogging.SetlogObjectPrefix(AIndex : Integer; AValue : String); 

begin
  If (FlogObjectPrefix=AValue) then exit;
  FlogObjectPrefix:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBucketTypeowner
  --------------------------------------------------------------------}


Procedure TBucketTypeowner.Setentity(AIndex : Integer; AValue : String); 

begin
  If (Fentity=AValue) then exit;
  Fentity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketTypeowner.SetentityId(AIndex : Integer; AValue : String); 

begin
  If (FentityId=AValue) then exit;
  FentityId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBucketTypeversioning
  --------------------------------------------------------------------}


Procedure TBucketTypeversioning.Setenabled(AIndex : Integer; AValue : boolean); 

begin
  If (Fenabled=AValue) then exit;
  Fenabled:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBucketTypewebsite
  --------------------------------------------------------------------}


Procedure TBucketTypewebsite.SetmainPageSuffix(AIndex : Integer; AValue : String); 

begin
  If (FmainPageSuffix=AValue) then exit;
  FmainPageSuffix:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketTypewebsite.SetnotFoundPage(AIndex : Integer; AValue : String); 

begin
  If (FnotFoundPage=AValue) then exit;
  FnotFoundPage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBucket
  --------------------------------------------------------------------}


Procedure TBucket.Setacl(AIndex : Integer; AValue : TBucketTypeaclArray); 

begin
  If (Facl=AValue) then exit;
  Facl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Setcors(AIndex : Integer; AValue : TBucketTypecorsArray); 

begin
  If (Fcors=AValue) then exit;
  Fcors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.SetdefaultObjectAcl(AIndex : Integer; AValue : TBucketTypedefaultObjectAclArray); 

begin
  If (FdefaultObjectAcl=AValue) then exit;
  FdefaultObjectAcl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Setlifecycle(AIndex : Integer; AValue : TBucketTypelifecycle); 

begin
  If (Flifecycle=AValue) then exit;
  Flifecycle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Setlocation(AIndex : Integer; AValue : String); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Setlogging(AIndex : Integer; AValue : TBucketTypelogging); 

begin
  If (Flogging=AValue) then exit;
  Flogging:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Setmetageneration(AIndex : Integer; AValue : String); 

begin
  If (Fmetageneration=AValue) then exit;
  Fmetageneration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Setowner(AIndex : Integer; AValue : TBucketTypeowner); 

begin
  If (Fowner=AValue) then exit;
  Fowner:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.SetprojectNumber(AIndex : Integer; AValue : String); 

begin
  If (FprojectNumber=AValue) then exit;
  FprojectNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.SetstorageClass(AIndex : Integer; AValue : String); 

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



Procedure TBucket.Setversioning(AIndex : Integer; AValue : TBucketTypeversioning); 

begin
  If (Fversioning=AValue) then exit;
  Fversioning:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucket.Setwebsite(AIndex : Integer; AValue : TBucketTypewebsite); 

begin
  If (Fwebsite=AValue) then exit;
  Fwebsite:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBucket.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'acl' : SetLength(Facl,ALength);
  'cors' : SetLength(Fcors,ALength);
  'defaultobjectacl' : SetLength(FdefaultObjectAcl,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBucketAccessControlTypeprojectTeam
  --------------------------------------------------------------------}


Procedure TBucketAccessControlTypeprojectTeam.SetprojectNumber(AIndex : Integer; AValue : String); 

begin
  If (FprojectNumber=AValue) then exit;
  FprojectNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketAccessControlTypeprojectTeam.Setteam(AIndex : Integer; AValue : String); 

begin
  If (Fteam=AValue) then exit;
  Fteam:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBucketAccessControl
  --------------------------------------------------------------------}


Procedure TBucketAccessControl.Setbucket(AIndex : Integer; AValue : String); 

begin
  If (Fbucket=AValue) then exit;
  Fbucket:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketAccessControl.Setdomain(AIndex : Integer; AValue : String); 

begin
  If (Fdomain=AValue) then exit;
  Fdomain:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketAccessControl.Setemail(AIndex : Integer; AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketAccessControl.Setentity(AIndex : Integer; AValue : String); 

begin
  If (Fentity=AValue) then exit;
  Fentity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketAccessControl.SetentityId(AIndex : Integer; AValue : String); 

begin
  If (FentityId=AValue) then exit;
  FentityId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketAccessControl.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketAccessControl.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketAccessControl.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketAccessControl.SetprojectTeam(AIndex : Integer; AValue : TBucketAccessControlTypeprojectTeam); 

begin
  If (FprojectTeam=AValue) then exit;
  FprojectTeam:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketAccessControl.Setrole(AIndex : Integer; AValue : String); 

begin
  If (Frole=AValue) then exit;
  Frole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketAccessControl.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBucketAccessControls
  --------------------------------------------------------------------}


Procedure TBucketAccessControls.Setitems(AIndex : Integer; AValue : TBucketAccessControlsTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBucketAccessControls.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBucketAccessControls.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBuckets
  --------------------------------------------------------------------}


Procedure TBuckets.Setitems(AIndex : Integer; AValue : TBucketsTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBuckets.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBuckets.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBuckets.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TChannelTypeparams
  --------------------------------------------------------------------}


Class Function TChannelTypeparams.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TChannel
  --------------------------------------------------------------------}


Procedure TChannel.Setaddress(AIndex : Integer; AValue : String); 

begin
  If (Faddress=AValue) then exit;
  Faddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Setexpiration(AIndex : Integer; AValue : String); 

begin
  If (Fexpiration=AValue) then exit;
  Fexpiration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Setparams(AIndex : Integer; AValue : TChannelTypeparams); 

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



Procedure TChannel.SetresourceId(AIndex : Integer; AValue : String); 

begin
  If (FresourceId=AValue) then exit;
  FresourceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.SetresourceUri(AIndex : Integer; AValue : String); 

begin
  If (FresourceUri=AValue) then exit;
  FresourceUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Settoken(AIndex : Integer; AValue : String); 

begin
  If (Ftoken=AValue) then exit;
  Ftoken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Set_type(AIndex : Integer; AValue : String); 

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
  TComposeRequestTypesourceObjectsItemTypeobjectPreconditions
  --------------------------------------------------------------------}


Procedure TComposeRequestTypesourceObjectsItemTypeobjectPreconditions.SetifGenerationMatch(AIndex : Integer; AValue : String); 

begin
  If (FifGenerationMatch=AValue) then exit;
  FifGenerationMatch:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TComposeRequestTypesourceObjectsItem
  --------------------------------------------------------------------}


Procedure TComposeRequestTypesourceObjectsItem.Setgeneration(AIndex : Integer; AValue : String); 

begin
  If (Fgeneration=AValue) then exit;
  Fgeneration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComposeRequestTypesourceObjectsItem.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComposeRequestTypesourceObjectsItem.SetobjectPreconditions(AIndex : Integer; AValue : TComposeRequestTypesourceObjectsItemTypeobjectPreconditions); 

begin
  If (FobjectPreconditions=AValue) then exit;
  FobjectPreconditions:=AValue;
  MarkPropertyChanged(AIndex);
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



Procedure TComposeRequest.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComposeRequest.SetsourceObjects(AIndex : Integer; AValue : TComposeRequestTypesourceObjectsArray); 

begin
  If (FsourceObjects=AValue) then exit;
  FsourceObjects:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TComposeRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'sourceobjects' : SetLength(FsourceObjects,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TObjectTypemetadata
  --------------------------------------------------------------------}


Class Function TObjectTypemetadata.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TObjectTypeowner
  --------------------------------------------------------------------}


Procedure TObjectTypeowner.Setentity(AIndex : Integer; AValue : String); 

begin
  If (Fentity=AValue) then exit;
  Fentity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectTypeowner.SetentityId(AIndex : Integer; AValue : String); 

begin
  If (FentityId=AValue) then exit;
  FentityId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TObject
  --------------------------------------------------------------------}


Procedure TObject.Setacl(AIndex : Integer; AValue : TObjectTypeaclArray); 

begin
  If (Facl=AValue) then exit;
  Facl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.Setbucket(AIndex : Integer; AValue : String); 

begin
  If (Fbucket=AValue) then exit;
  Fbucket:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.SetcacheControl(AIndex : Integer; AValue : String); 

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



Procedure TObject.SetcontentDisposition(AIndex : Integer; AValue : String); 

begin
  If (FcontentDisposition=AValue) then exit;
  FcontentDisposition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.SetcontentEncoding(AIndex : Integer; AValue : String); 

begin
  If (FcontentEncoding=AValue) then exit;
  FcontentEncoding:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.SetcontentLanguage(AIndex : Integer; AValue : String); 

begin
  If (FcontentLanguage=AValue) then exit;
  FcontentLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.SetcontentType(AIndex : Integer; AValue : String); 

begin
  If (FcontentType=AValue) then exit;
  FcontentType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.Setcrc32c(AIndex : Integer; AValue : String); 

begin
  If (Fcrc32c=AValue) then exit;
  Fcrc32c:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.Setgeneration(AIndex : Integer; AValue : String); 

begin
  If (Fgeneration=AValue) then exit;
  Fgeneration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.Setmd5Hash(AIndex : Integer; AValue : String); 

begin
  If (Fmd5Hash=AValue) then exit;
  Fmd5Hash:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.SetmediaLink(AIndex : Integer; AValue : String); 

begin
  If (FmediaLink=AValue) then exit;
  FmediaLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.Setmetadata(AIndex : Integer; AValue : TObjectTypemetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.Setmetageneration(AIndex : Integer; AValue : String); 

begin
  If (Fmetageneration=AValue) then exit;
  Fmetageneration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.Setowner(AIndex : Integer; AValue : TObjectTypeowner); 

begin
  If (Fowner=AValue) then exit;
  Fowner:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.Setsize(AIndex : Integer; AValue : String); 

begin
  If (Fsize=AValue) then exit;
  Fsize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObject.SetstorageClass(AIndex : Integer; AValue : String); 

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


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TObject.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'acl' : SetLength(Facl,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TObjectAccessControlTypeprojectTeam
  --------------------------------------------------------------------}


Procedure TObjectAccessControlTypeprojectTeam.SetprojectNumber(AIndex : Integer; AValue : String); 

begin
  If (FprojectNumber=AValue) then exit;
  FprojectNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectAccessControlTypeprojectTeam.Setteam(AIndex : Integer; AValue : String); 

begin
  If (Fteam=AValue) then exit;
  Fteam:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TObjectAccessControl
  --------------------------------------------------------------------}


Procedure TObjectAccessControl.Setbucket(AIndex : Integer; AValue : String); 

begin
  If (Fbucket=AValue) then exit;
  Fbucket:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectAccessControl.Setdomain(AIndex : Integer; AValue : String); 

begin
  If (Fdomain=AValue) then exit;
  Fdomain:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectAccessControl.Setemail(AIndex : Integer; AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectAccessControl.Setentity(AIndex : Integer; AValue : String); 

begin
  If (Fentity=AValue) then exit;
  Fentity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectAccessControl.SetentityId(AIndex : Integer; AValue : String); 

begin
  If (FentityId=AValue) then exit;
  FentityId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectAccessControl.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectAccessControl.Setgeneration(AIndex : Integer; AValue : String); 

begin
  If (Fgeneration=AValue) then exit;
  Fgeneration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectAccessControl.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectAccessControl.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectAccessControl.Set_object(AIndex : Integer; AValue : String); 

begin
  If (F_object=AValue) then exit;
  F_object:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectAccessControl.SetprojectTeam(AIndex : Integer; AValue : TObjectAccessControlTypeprojectTeam); 

begin
  If (FprojectTeam=AValue) then exit;
  FprojectTeam:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectAccessControl.Setrole(AIndex : Integer; AValue : String); 

begin
  If (Frole=AValue) then exit;
  Frole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectAccessControl.SetselfLink(AIndex : Integer; AValue : String); 

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
  TObjectAccessControls
  --------------------------------------------------------------------}


Procedure TObjectAccessControls.Setitems(AIndex : Integer; AValue : TTJSONSchemaArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjectAccessControls.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TObjectAccessControls.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TObjects
  --------------------------------------------------------------------}


Procedure TObjects.Setitems(AIndex : Integer; AValue : TObjectsTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjects.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjects.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObjects.Setprefixes(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fprefixes=AValue) then exit;
  Fprefixes:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TObjects.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  'prefixes' : SetLength(Fprefixes,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TRewriteResponse
  --------------------------------------------------------------------}


Procedure TRewriteResponse.Setdone(AIndex : Integer; AValue : boolean); 

begin
  If (Fdone=AValue) then exit;
  Fdone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRewriteResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRewriteResponse.SetobjectSize(AIndex : Integer; AValue : String); 

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



Procedure TRewriteResponse.SetrewriteToken(AIndex : Integer; AValue : String); 

begin
  If (FrewriteToken=AValue) then exit;
  FrewriteToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRewriteResponse.SettotalBytesRewritten(AIndex : Integer; AValue : String); 

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
  Result:='https://www.googleapis.com:443/';
end;

Class Function TStorageAPI.APIbasePath : string;

begin
  Result:='/storage/v1/';
end;

Class Function TStorageAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/storage/v1/';
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
  TBucketTypecorsItem.RegisterObject;
  TBucketTypelifecycleTyperuleItemTypeaction.RegisterObject;
  TBucketTypelifecycleTyperuleItemTypecondition.RegisterObject;
  TBucketTypelifecycleTyperuleItem.RegisterObject;
  TBucketTypelifecycle.RegisterObject;
  TBucketTypelogging.RegisterObject;
  TBucketTypeowner.RegisterObject;
  TBucketTypeversioning.RegisterObject;
  TBucketTypewebsite.RegisterObject;
  TBucket.RegisterObject;
  TBucketAccessControlTypeprojectTeam.RegisterObject;
  TBucketAccessControl.RegisterObject;
  TBucketAccessControls.RegisterObject;
  TBuckets.RegisterObject;
  TChannelTypeparams.RegisterObject;
  TChannel.RegisterObject;
  TComposeRequestTypesourceObjectsItemTypeobjectPreconditions.RegisterObject;
  TComposeRequestTypesourceObjectsItem.RegisterObject;
  TComposeRequest.RegisterObject;
  TObjectTypemetadata.RegisterObject;
  TObjectTypeowner.RegisterObject;
  TObject.RegisterObject;
  TObjectAccessControlTypeprojectTeam.RegisterObject;
  TObjectAccessControl.RegisterObject;
  TObjectAccessControls.RegisterObject;
  TObjects.RegisterObject;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
end;



initialization
  TStorageAPI.RegisterAPI;
end.
