unit googledrive;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAbout = Class;
  TChange = Class;
  TChangeList = Class;
  TChannel = Class;
  TComment = Class;
  TCommentList = Class;
  TFile = Class;
  TFileList = Class;
  TGeneratedIds = Class;
  TPermission = Class;
  TPermissionList = Class;
  TReply = Class;
  TReplyList = Class;
  TRevision = Class;
  TRevisionList = Class;
  TStartPageToken = Class;
  TUser = Class;
  TAboutArray = Array of TAbout;
  TChangeArray = Array of TChange;
  TChangeListArray = Array of TChangeList;
  TChannelArray = Array of TChannel;
  TCommentArray = Array of TComment;
  TCommentListArray = Array of TCommentList;
  TFileArray = Array of TFile;
  TFileListArray = Array of TFileList;
  TGeneratedIdsArray = Array of TGeneratedIds;
  TPermissionArray = Array of TPermission;
  TPermissionListArray = Array of TPermissionList;
  TReplyArray = Array of TReply;
  TReplyListArray = Array of TReplyList;
  TRevisionArray = Array of TRevision;
  TRevisionListArray = Array of TRevisionList;
  TStartPageTokenArray = Array of TStartPageToken;
  TUserArray = Array of TUser;
  //Anonymous types, using auto-generated names
  TAboutTypeexportFormats = Class;
  TAboutTypeimportFormats = Class;
  TAboutTypemaxImportSizes = Class;
  TAboutTypestorageQuota = Class;
  TChannelTypeparams = Class;
  TCommentTypequotedFileContent = Class;
  TFileTypeappProperties = Class;
  TFileTypecapabilities = Class;
  TFileTypecontentHintsTypethumbnail = Class;
  TFileTypecontentHints = Class;
  TFileTypeimageMediaMetadataTypelocation = Class;
  TFileTypeimageMediaMetadata = Class;
  TFileTypeproperties = Class;
  TFileTypevideoMediaMetadata = Class;
  TChangeListTypechangesArray = Array of TChange;
  TCommentTyperepliesArray = Array of TReply;
  TCommentListTypecommentsArray = Array of TComment;
  TFileTypeownersArray = Array of TUser;
  TFileTypepermissionsArray = Array of TPermission;
  TFileListTypefilesArray = Array of TFile;
  TPermissionListTypepermissionsArray = Array of TPermission;
  TReplyListTyperepliesArray = Array of TReply;
  TRevisionListTyperevisionsArray = Array of TRevision;
  
  { --------------------------------------------------------------------
    TAboutTypeexportFormats
    --------------------------------------------------------------------}
  
  TAboutTypeexportFormats = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TAboutTypeexportFormatsClass = Class of TAboutTypeexportFormats;
  
  { --------------------------------------------------------------------
    TAboutTypeimportFormats
    --------------------------------------------------------------------}
  
  TAboutTypeimportFormats = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TAboutTypeimportFormatsClass = Class of TAboutTypeimportFormats;
  
  { --------------------------------------------------------------------
    TAboutTypemaxImportSizes
    --------------------------------------------------------------------}
  
  TAboutTypemaxImportSizes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TAboutTypemaxImportSizesClass = Class of TAboutTypemaxImportSizes;
  
  { --------------------------------------------------------------------
    TAboutTypestorageQuota
    --------------------------------------------------------------------}
  
  TAboutTypestorageQuota = Class(TGoogleBaseObject)
  Private
    Flimit : String;
    Fusage : String;
    FusageInDrive : String;
    FusageInDriveTrash : String;
  Protected
    //Property setters
    Procedure Setlimit(AIndex : Integer; const AValue : String); virtual;
    Procedure Setusage(AIndex : Integer; const AValue : String); virtual;
    Procedure SetusageInDrive(AIndex : Integer; const AValue : String); virtual;
    Procedure SetusageInDriveTrash(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property limit : String Index 0 Read Flimit Write Setlimit;
    Property usage : String Index 8 Read Fusage Write Setusage;
    Property usageInDrive : String Index 16 Read FusageInDrive Write SetusageInDrive;
    Property usageInDriveTrash : String Index 24 Read FusageInDriveTrash Write SetusageInDriveTrash;
  end;
  TAboutTypestorageQuotaClass = Class of TAboutTypestorageQuota;
  
  { --------------------------------------------------------------------
    TAbout
    --------------------------------------------------------------------}
  
  TAbout = Class(TGoogleBaseObject)
  Private
    FappInstalled : boolean;
    FexportFormats : TAboutTypeexportFormats;
    FfolderColorPalette : TStringArray;
    FimportFormats : TAboutTypeimportFormats;
    Fkind : String;
    FmaxImportSizes : TAboutTypemaxImportSizes;
    FmaxUploadSize : String;
    FstorageQuota : TAboutTypestorageQuota;
    Fuser : TUser;
  Protected
    //Property setters
    Procedure SetappInstalled(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetexportFormats(AIndex : Integer; const AValue : TAboutTypeexportFormats); virtual;
    Procedure SetfolderColorPalette(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetimportFormats(AIndex : Integer; const AValue : TAboutTypeimportFormats); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmaxImportSizes(AIndex : Integer; const AValue : TAboutTypemaxImportSizes); virtual;
    Procedure SetmaxUploadSize(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstorageQuota(AIndex : Integer; const AValue : TAboutTypestorageQuota); virtual;
    Procedure Setuser(AIndex : Integer; const AValue : TUser); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property appInstalled : boolean Index 0 Read FappInstalled Write SetappInstalled;
    Property exportFormats : TAboutTypeexportFormats Index 8 Read FexportFormats Write SetexportFormats;
    Property folderColorPalette : TStringArray Index 16 Read FfolderColorPalette Write SetfolderColorPalette;
    Property importFormats : TAboutTypeimportFormats Index 24 Read FimportFormats Write SetimportFormats;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property maxImportSizes : TAboutTypemaxImportSizes Index 40 Read FmaxImportSizes Write SetmaxImportSizes;
    Property maxUploadSize : String Index 48 Read FmaxUploadSize Write SetmaxUploadSize;
    Property storageQuota : TAboutTypestorageQuota Index 56 Read FstorageQuota Write SetstorageQuota;
    Property user : TUser Index 64 Read Fuser Write Setuser;
  end;
  TAboutClass = Class of TAbout;
  
  { --------------------------------------------------------------------
    TChange
    --------------------------------------------------------------------}
  
  TChange = Class(TGoogleBaseObject)
  Private
    F_file : TFile;
    FfileId : String;
    Fkind : String;
    Fremoved : boolean;
    Ftime : TDatetime;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_file(AIndex : Integer; const AValue : TFile); virtual;
    Procedure SetfileId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setremoved(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Settime(AIndex : Integer; const AValue : TDatetime); virtual;
  Public
  Published
    Property _file : TFile Index 0 Read F_file Write Set_file;
    Property fileId : String Index 8 Read FfileId Write SetfileId;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property removed : boolean Index 24 Read Fremoved Write Setremoved;
    Property time : TDatetime Index 32 Read Ftime Write Settime;
  end;
  TChangeClass = Class of TChange;
  
  { --------------------------------------------------------------------
    TChangeList
    --------------------------------------------------------------------}
  
  TChangeList = Class(TGoogleBaseObject)
  Private
    Fchanges : TChangeListTypechangesArray;
    Fkind : String;
    FnewStartPageToken : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setchanges(AIndex : Integer; const AValue : TChangeListTypechangesArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnewStartPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property changes : TChangeListTypechangesArray Index 0 Read Fchanges Write Setchanges;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property newStartPageToken : String Index 16 Read FnewStartPageToken Write SetnewStartPageToken;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TChangeListClass = Class of TChangeList;
  
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
    Procedure Setaddress(AIndex : Integer; const AValue : String); virtual;
    Procedure Setexpiration(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setparams(AIndex : Integer; const AValue : TChannelTypeparams); virtual;
    Procedure Setpayload(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetresourceId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetresourceUri(AIndex : Integer; const AValue : String); virtual;
    Procedure Settoken(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
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
    TCommentTypequotedFileContent
    --------------------------------------------------------------------}
  
  TCommentTypequotedFileContent = Class(TGoogleBaseObject)
  Private
    FmimeType : String;
    Fvalue : String;
  Protected
    //Property setters
    Procedure SetmimeType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property mimeType : String Index 0 Read FmimeType Write SetmimeType;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TCommentTypequotedFileContentClass = Class of TCommentTypequotedFileContent;
  
  { --------------------------------------------------------------------
    TComment
    --------------------------------------------------------------------}
  
  TComment = Class(TGoogleBaseObject)
  Private
    Fanchor : String;
    Fauthor : TUser;
    Fcontent : String;
    FcreatedTime : TDatetime;
    Fdeleted : boolean;
    FhtmlContent : String;
    Fid : String;
    Fkind : String;
    FmodifiedTime : TDatetime;
    FquotedFileContent : TCommentTypequotedFileContent;
    Freplies : TCommentTyperepliesArray;
    Fresolved : boolean;
  Protected
    //Property setters
    Procedure Setanchor(AIndex : Integer; const AValue : String); virtual;
    Procedure Setauthor(AIndex : Integer; const AValue : TUser); virtual;
    Procedure Setcontent(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcreatedTime(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure Setdeleted(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SethtmlContent(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmodifiedTime(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure SetquotedFileContent(AIndex : Integer; const AValue : TCommentTypequotedFileContent); virtual;
    Procedure Setreplies(AIndex : Integer; const AValue : TCommentTyperepliesArray); virtual;
    Procedure Setresolved(AIndex : Integer; const AValue : boolean); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property anchor : String Index 0 Read Fanchor Write Setanchor;
    Property author : TUser Index 8 Read Fauthor Write Setauthor;
    Property content : String Index 16 Read Fcontent Write Setcontent;
    Property createdTime : TDatetime Index 24 Read FcreatedTime Write SetcreatedTime;
    Property deleted : boolean Index 32 Read Fdeleted Write Setdeleted;
    Property htmlContent : String Index 40 Read FhtmlContent Write SethtmlContent;
    Property id : String Index 48 Read Fid Write Setid;
    Property kind : String Index 56 Read Fkind Write Setkind;
    Property modifiedTime : TDatetime Index 64 Read FmodifiedTime Write SetmodifiedTime;
    Property quotedFileContent : TCommentTypequotedFileContent Index 72 Read FquotedFileContent Write SetquotedFileContent;
    Property replies : TCommentTyperepliesArray Index 80 Read Freplies Write Setreplies;
    Property resolved : boolean Index 88 Read Fresolved Write Setresolved;
  end;
  TCommentClass = Class of TComment;
  
  { --------------------------------------------------------------------
    TCommentList
    --------------------------------------------------------------------}
  
  TCommentList = Class(TGoogleBaseObject)
  Private
    Fcomments : TCommentListTypecommentsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setcomments(AIndex : Integer; const AValue : TCommentListTypecommentsArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property comments : TCommentListTypecommentsArray Index 0 Read Fcomments Write Setcomments;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TCommentListClass = Class of TCommentList;
  
  { --------------------------------------------------------------------
    TFileTypeappProperties
    --------------------------------------------------------------------}
  
  TFileTypeappProperties = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TFileTypeappPropertiesClass = Class of TFileTypeappProperties;
  
  { --------------------------------------------------------------------
    TFileTypecapabilities
    --------------------------------------------------------------------}
  
  TFileTypecapabilities = Class(TGoogleBaseObject)
  Private
    FcanComment : boolean;
    FcanCopy : boolean;
    FcanEdit : boolean;
    FcanReadRevisions : boolean;
    FcanShare : boolean;
  Protected
    //Property setters
    Procedure SetcanComment(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetcanCopy(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetcanEdit(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetcanReadRevisions(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetcanShare(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property canComment : boolean Index 0 Read FcanComment Write SetcanComment;
    Property canCopy : boolean Index 8 Read FcanCopy Write SetcanCopy;
    Property canEdit : boolean Index 16 Read FcanEdit Write SetcanEdit;
    Property canReadRevisions : boolean Index 24 Read FcanReadRevisions Write SetcanReadRevisions;
    Property canShare : boolean Index 32 Read FcanShare Write SetcanShare;
  end;
  TFileTypecapabilitiesClass = Class of TFileTypecapabilities;
  
  { --------------------------------------------------------------------
    TFileTypecontentHintsTypethumbnail
    --------------------------------------------------------------------}
  
  TFileTypecontentHintsTypethumbnail = Class(TGoogleBaseObject)
  Private
    Fimage : String;
    FmimeType : String;
  Protected
    //Property setters
    Procedure Setimage(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmimeType(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property image : String Index 0 Read Fimage Write Setimage;
    Property mimeType : String Index 8 Read FmimeType Write SetmimeType;
  end;
  TFileTypecontentHintsTypethumbnailClass = Class of TFileTypecontentHintsTypethumbnail;
  
  { --------------------------------------------------------------------
    TFileTypecontentHints
    --------------------------------------------------------------------}
  
  TFileTypecontentHints = Class(TGoogleBaseObject)
  Private
    FindexableText : String;
    Fthumbnail : TFileTypecontentHintsTypethumbnail;
  Protected
    //Property setters
    Procedure SetindexableText(AIndex : Integer; const AValue : String); virtual;
    Procedure Setthumbnail(AIndex : Integer; const AValue : TFileTypecontentHintsTypethumbnail); virtual;
  Public
  Published
    Property indexableText : String Index 0 Read FindexableText Write SetindexableText;
    Property thumbnail : TFileTypecontentHintsTypethumbnail Index 8 Read Fthumbnail Write Setthumbnail;
  end;
  TFileTypecontentHintsClass = Class of TFileTypecontentHints;
  
  { --------------------------------------------------------------------
    TFileTypeimageMediaMetadataTypelocation
    --------------------------------------------------------------------}
  
  TFileTypeimageMediaMetadataTypelocation = Class(TGoogleBaseObject)
  Private
    Faltitude : double;
    Flatitude : double;
    Flongitude : double;
  Protected
    //Property setters
    Procedure Setaltitude(AIndex : Integer; const AValue : double); virtual;
    Procedure Setlatitude(AIndex : Integer; const AValue : double); virtual;
    Procedure Setlongitude(AIndex : Integer; const AValue : double); virtual;
  Public
  Published
    Property altitude : double Index 0 Read Faltitude Write Setaltitude;
    Property latitude : double Index 8 Read Flatitude Write Setlatitude;
    Property longitude : double Index 16 Read Flongitude Write Setlongitude;
  end;
  TFileTypeimageMediaMetadataTypelocationClass = Class of TFileTypeimageMediaMetadataTypelocation;
  
  { --------------------------------------------------------------------
    TFileTypeimageMediaMetadata
    --------------------------------------------------------------------}
  
  TFileTypeimageMediaMetadata = Class(TGoogleBaseObject)
  Private
    Faperture : integer;
    FcameraMake : String;
    FcameraModel : String;
    FcolorSpace : String;
    FexposureBias : integer;
    FexposureMode : String;
    FexposureTime : integer;
    FflashUsed : boolean;
    FfocalLength : integer;
    Fheight : integer;
    FisoSpeed : integer;
    Flens : String;
    Flocation : TFileTypeimageMediaMetadataTypelocation;
    FmaxApertureValue : integer;
    FmeteringMode : String;
    Frotation : integer;
    Fsensor : String;
    FsubjectDistance : integer;
    Ftime : String;
    FwhiteBalance : String;
    Fwidth : integer;
  Protected
    //Property setters
    Procedure Setaperture(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetcameraMake(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcameraModel(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcolorSpace(AIndex : Integer; const AValue : String); virtual;
    Procedure SetexposureBias(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetexposureMode(AIndex : Integer; const AValue : String); virtual;
    Procedure SetexposureTime(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetflashUsed(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetfocalLength(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setheight(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetisoSpeed(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setlens(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlocation(AIndex : Integer; const AValue : TFileTypeimageMediaMetadataTypelocation); virtual;
    Procedure SetmaxApertureValue(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetmeteringMode(AIndex : Integer; const AValue : String); virtual;
    Procedure Setrotation(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setsensor(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsubjectDistance(AIndex : Integer; const AValue : integer); virtual;
    Procedure Settime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetwhiteBalance(AIndex : Integer; const AValue : String); virtual;
    Procedure Setwidth(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property aperture : integer Index 0 Read Faperture Write Setaperture;
    Property cameraMake : String Index 8 Read FcameraMake Write SetcameraMake;
    Property cameraModel : String Index 16 Read FcameraModel Write SetcameraModel;
    Property colorSpace : String Index 24 Read FcolorSpace Write SetcolorSpace;
    Property exposureBias : integer Index 32 Read FexposureBias Write SetexposureBias;
    Property exposureMode : String Index 40 Read FexposureMode Write SetexposureMode;
    Property exposureTime : integer Index 48 Read FexposureTime Write SetexposureTime;
    Property flashUsed : boolean Index 56 Read FflashUsed Write SetflashUsed;
    Property focalLength : integer Index 64 Read FfocalLength Write SetfocalLength;
    Property height : integer Index 72 Read Fheight Write Setheight;
    Property isoSpeed : integer Index 80 Read FisoSpeed Write SetisoSpeed;
    Property lens : String Index 88 Read Flens Write Setlens;
    Property location : TFileTypeimageMediaMetadataTypelocation Index 96 Read Flocation Write Setlocation;
    Property maxApertureValue : integer Index 104 Read FmaxApertureValue Write SetmaxApertureValue;
    Property meteringMode : String Index 112 Read FmeteringMode Write SetmeteringMode;
    Property rotation : integer Index 120 Read Frotation Write Setrotation;
    Property sensor : String Index 128 Read Fsensor Write Setsensor;
    Property subjectDistance : integer Index 136 Read FsubjectDistance Write SetsubjectDistance;
    Property time : String Index 144 Read Ftime Write Settime;
    Property whiteBalance : String Index 152 Read FwhiteBalance Write SetwhiteBalance;
    Property width : integer Index 160 Read Fwidth Write Setwidth;
  end;
  TFileTypeimageMediaMetadataClass = Class of TFileTypeimageMediaMetadata;
  
  { --------------------------------------------------------------------
    TFileTypeproperties
    --------------------------------------------------------------------}
  
  TFileTypeproperties = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TFileTypepropertiesClass = Class of TFileTypeproperties;
  
  { --------------------------------------------------------------------
    TFileTypevideoMediaMetadata
    --------------------------------------------------------------------}
  
  TFileTypevideoMediaMetadata = Class(TGoogleBaseObject)
  Private
    FdurationMillis : String;
    Fheight : integer;
    Fwidth : integer;
  Protected
    //Property setters
    Procedure SetdurationMillis(AIndex : Integer; const AValue : String); virtual;
    Procedure Setheight(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setwidth(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property durationMillis : String Index 0 Read FdurationMillis Write SetdurationMillis;
    Property height : integer Index 8 Read Fheight Write Setheight;
    Property width : integer Index 16 Read Fwidth Write Setwidth;
  end;
  TFileTypevideoMediaMetadataClass = Class of TFileTypevideoMediaMetadata;
  
  { --------------------------------------------------------------------
    TFile
    --------------------------------------------------------------------}
  
  TFile = Class(TGoogleBaseObject)
  Private
    FappProperties : TFileTypeappProperties;
    Fcapabilities : TFileTypecapabilities;
    FcontentHints : TFileTypecontentHints;
    FcreatedTime : TDatetime;
    Fdescription : String;
    FexplicitlyTrashed : boolean;
    FfileExtension : String;
    FfolderColorRgb : String;
    FfullFileExtension : String;
    FheadRevisionId : String;
    FiconLink : String;
    Fid : String;
    FimageMediaMetadata : TFileTypeimageMediaMetadata;
    FisAppAuthorized : boolean;
    Fkind : String;
    FlastModifyingUser : TUser;
    Fmd5Checksum : String;
    FmimeType : String;
    FmodifiedByMeTime : TDatetime;
    FmodifiedTime : TDatetime;
    Fname : String;
    ForiginalFilename : String;
    FownedByMe : boolean;
    Fowners : TFileTypeownersArray;
    Fparents : TStringArray;
    Fpermissions : TFileTypepermissionsArray;
    Fproperties : TFileTypeproperties;
    FquotaBytesUsed : String;
    Fshared : boolean;
    FsharedWithMeTime : TDatetime;
    FsharingUser : TUser;
    Fsize : String;
    Fspaces : TStringArray;
    Fstarred : boolean;
    FthumbnailLink : String;
    Ftrashed : boolean;
    Fversion : String;
    FvideoMediaMetadata : TFileTypevideoMediaMetadata;
    FviewedByMe : boolean;
    FviewedByMeTime : TDatetime;
    FviewersCanCopyContent : boolean;
    FwebContentLink : String;
    FwebViewLink : String;
    FwritersCanShare : boolean;
  Protected
    //Property setters
    Procedure SetappProperties(AIndex : Integer; const AValue : TFileTypeappProperties); virtual;
    Procedure Setcapabilities(AIndex : Integer; const AValue : TFileTypecapabilities); virtual;
    Procedure SetcontentHints(AIndex : Integer; const AValue : TFileTypecontentHints); virtual;
    Procedure SetcreatedTime(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure SetexplicitlyTrashed(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetfileExtension(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfolderColorRgb(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfullFileExtension(AIndex : Integer; const AValue : String); virtual;
    Procedure SetheadRevisionId(AIndex : Integer; const AValue : String); virtual;
    Procedure SeticonLink(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure SetimageMediaMetadata(AIndex : Integer; const AValue : TFileTypeimageMediaMetadata); virtual;
    Procedure SetisAppAuthorized(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlastModifyingUser(AIndex : Integer; const AValue : TUser); virtual;
    Procedure Setmd5Checksum(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmimeType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmodifiedByMeTime(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure SetmodifiedTime(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoriginalFilename(AIndex : Integer; const AValue : String); virtual;
    Procedure SetownedByMe(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setowners(AIndex : Integer; const AValue : TFileTypeownersArray); virtual;
    Procedure Setparents(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setpermissions(AIndex : Integer; const AValue : TFileTypepermissionsArray); virtual;
    Procedure Setproperties(AIndex : Integer; const AValue : TFileTypeproperties); virtual;
    Procedure SetquotaBytesUsed(AIndex : Integer; const AValue : String); virtual;
    Procedure Setshared(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetsharedWithMeTime(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure SetsharingUser(AIndex : Integer; const AValue : TUser); virtual;
    Procedure Setsize(AIndex : Integer; const AValue : String); virtual;
    Procedure Setspaces(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setstarred(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetthumbnailLink(AIndex : Integer; const AValue : String); virtual;
    Procedure Settrashed(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setversion(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvideoMediaMetadata(AIndex : Integer; const AValue : TFileTypevideoMediaMetadata); virtual;
    Procedure SetviewedByMe(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetviewedByMeTime(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure SetviewersCanCopyContent(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetwebContentLink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetwebViewLink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetwritersCanShare(AIndex : Integer; const AValue : boolean); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property appProperties : TFileTypeappProperties Index 0 Read FappProperties Write SetappProperties;
    Property capabilities : TFileTypecapabilities Index 8 Read Fcapabilities Write Setcapabilities;
    Property contentHints : TFileTypecontentHints Index 16 Read FcontentHints Write SetcontentHints;
    Property createdTime : TDatetime Index 24 Read FcreatedTime Write SetcreatedTime;
    Property description : String Index 32 Read Fdescription Write Setdescription;
    Property explicitlyTrashed : boolean Index 40 Read FexplicitlyTrashed Write SetexplicitlyTrashed;
    Property fileExtension : String Index 48 Read FfileExtension Write SetfileExtension;
    Property folderColorRgb : String Index 56 Read FfolderColorRgb Write SetfolderColorRgb;
    Property fullFileExtension : String Index 64 Read FfullFileExtension Write SetfullFileExtension;
    Property headRevisionId : String Index 72 Read FheadRevisionId Write SetheadRevisionId;
    Property iconLink : String Index 80 Read FiconLink Write SeticonLink;
    Property id : String Index 88 Read Fid Write Setid;
    Property imageMediaMetadata : TFileTypeimageMediaMetadata Index 96 Read FimageMediaMetadata Write SetimageMediaMetadata;
    Property isAppAuthorized : boolean Index 104 Read FisAppAuthorized Write SetisAppAuthorized;
    Property kind : String Index 112 Read Fkind Write Setkind;
    Property lastModifyingUser : TUser Index 120 Read FlastModifyingUser Write SetlastModifyingUser;
    Property md5Checksum : String Index 128 Read Fmd5Checksum Write Setmd5Checksum;
    Property mimeType : String Index 136 Read FmimeType Write SetmimeType;
    Property modifiedByMeTime : TDatetime Index 144 Read FmodifiedByMeTime Write SetmodifiedByMeTime;
    Property modifiedTime : TDatetime Index 152 Read FmodifiedTime Write SetmodifiedTime;
    Property name : String Index 160 Read Fname Write Setname;
    Property originalFilename : String Index 168 Read ForiginalFilename Write SetoriginalFilename;
    Property ownedByMe : boolean Index 176 Read FownedByMe Write SetownedByMe;
    Property owners : TFileTypeownersArray Index 184 Read Fowners Write Setowners;
    Property parents : TStringArray Index 192 Read Fparents Write Setparents;
    Property permissions : TFileTypepermissionsArray Index 200 Read Fpermissions Write Setpermissions;
    Property properties : TFileTypeproperties Index 208 Read Fproperties Write Setproperties;
    Property quotaBytesUsed : String Index 216 Read FquotaBytesUsed Write SetquotaBytesUsed;
    Property shared : boolean Index 224 Read Fshared Write Setshared;
    Property sharedWithMeTime : TDatetime Index 232 Read FsharedWithMeTime Write SetsharedWithMeTime;
    Property sharingUser : TUser Index 240 Read FsharingUser Write SetsharingUser;
    Property size : String Index 248 Read Fsize Write Setsize;
    Property spaces : TStringArray Index 256 Read Fspaces Write Setspaces;
    Property starred : boolean Index 264 Read Fstarred Write Setstarred;
    Property thumbnailLink : String Index 272 Read FthumbnailLink Write SetthumbnailLink;
    Property trashed : boolean Index 280 Read Ftrashed Write Settrashed;
    Property version : String Index 288 Read Fversion Write Setversion;
    Property videoMediaMetadata : TFileTypevideoMediaMetadata Index 296 Read FvideoMediaMetadata Write SetvideoMediaMetadata;
    Property viewedByMe : boolean Index 304 Read FviewedByMe Write SetviewedByMe;
    Property viewedByMeTime : TDatetime Index 312 Read FviewedByMeTime Write SetviewedByMeTime;
    Property viewersCanCopyContent : boolean Index 320 Read FviewersCanCopyContent Write SetviewersCanCopyContent;
    Property webContentLink : String Index 328 Read FwebContentLink Write SetwebContentLink;
    Property webViewLink : String Index 336 Read FwebViewLink Write SetwebViewLink;
    Property writersCanShare : boolean Index 344 Read FwritersCanShare Write SetwritersCanShare;
  end;
  TFileClass = Class of TFile;
  
  { --------------------------------------------------------------------
    TFileList
    --------------------------------------------------------------------}
  
  TFileList = Class(TGoogleBaseObject)
  Private
    Ffiles : TFileListTypefilesArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setfiles(AIndex : Integer; const AValue : TFileListTypefilesArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property files : TFileListTypefilesArray Index 0 Read Ffiles Write Setfiles;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TFileListClass = Class of TFileList;
  
  { --------------------------------------------------------------------
    TGeneratedIds
    --------------------------------------------------------------------}
  
  TGeneratedIds = Class(TGoogleBaseObject)
  Private
    Fids : TStringArray;
    Fkind : String;
    Fspace : String;
  Protected
    //Property setters
    Procedure Setids(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setspace(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property ids : TStringArray Index 0 Read Fids Write Setids;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property space : String Index 16 Read Fspace Write Setspace;
  end;
  TGeneratedIdsClass = Class of TGeneratedIds;
  
  { --------------------------------------------------------------------
    TPermission
    --------------------------------------------------------------------}
  
  TPermission = Class(TGoogleBaseObject)
  Private
    FallowFileDiscovery : boolean;
    FdisplayName : String;
    Fdomain : String;
    FemailAddress : String;
    Fid : String;
    Fkind : String;
    FphotoLink : String;
    Frole : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetallowFileDiscovery(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdomain(AIndex : Integer; const AValue : String); virtual;
    Procedure SetemailAddress(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetphotoLink(AIndex : Integer; const AValue : String); virtual;
    Procedure Setrole(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property allowFileDiscovery : boolean Index 0 Read FallowFileDiscovery Write SetallowFileDiscovery;
    Property displayName : String Index 8 Read FdisplayName Write SetdisplayName;
    Property domain : String Index 16 Read Fdomain Write Setdomain;
    Property emailAddress : String Index 24 Read FemailAddress Write SetemailAddress;
    Property id : String Index 32 Read Fid Write Setid;
    Property kind : String Index 40 Read Fkind Write Setkind;
    Property photoLink : String Index 48 Read FphotoLink Write SetphotoLink;
    Property role : String Index 56 Read Frole Write Setrole;
    Property _type : String Index 64 Read F_type Write Set_type;
  end;
  TPermissionClass = Class of TPermission;
  
  { --------------------------------------------------------------------
    TPermissionList
    --------------------------------------------------------------------}
  
  TPermissionList = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fpermissions : TPermissionListTypepermissionsArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setpermissions(AIndex : Integer; const AValue : TPermissionListTypepermissionsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property permissions : TPermissionListTypepermissionsArray Index 8 Read Fpermissions Write Setpermissions;
  end;
  TPermissionListClass = Class of TPermissionList;
  
  { --------------------------------------------------------------------
    TReply
    --------------------------------------------------------------------}
  
  TReply = Class(TGoogleBaseObject)
  Private
    Faction : String;
    Fauthor : TUser;
    Fcontent : String;
    FcreatedTime : TDatetime;
    Fdeleted : boolean;
    FhtmlContent : String;
    Fid : String;
    Fkind : String;
    FmodifiedTime : TDatetime;
  Protected
    //Property setters
    Procedure Setaction(AIndex : Integer; const AValue : String); virtual;
    Procedure Setauthor(AIndex : Integer; const AValue : TUser); virtual;
    Procedure Setcontent(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcreatedTime(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure Setdeleted(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SethtmlContent(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmodifiedTime(AIndex : Integer; const AValue : TDatetime); virtual;
  Public
  Published
    Property action : String Index 0 Read Faction Write Setaction;
    Property author : TUser Index 8 Read Fauthor Write Setauthor;
    Property content : String Index 16 Read Fcontent Write Setcontent;
    Property createdTime : TDatetime Index 24 Read FcreatedTime Write SetcreatedTime;
    Property deleted : boolean Index 32 Read Fdeleted Write Setdeleted;
    Property htmlContent : String Index 40 Read FhtmlContent Write SethtmlContent;
    Property id : String Index 48 Read Fid Write Setid;
    Property kind : String Index 56 Read Fkind Write Setkind;
    Property modifiedTime : TDatetime Index 64 Read FmodifiedTime Write SetmodifiedTime;
  end;
  TReplyClass = Class of TReply;
  
  { --------------------------------------------------------------------
    TReplyList
    --------------------------------------------------------------------}
  
  TReplyList = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnextPageToken : String;
    Freplies : TReplyListTyperepliesArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure Setreplies(AIndex : Integer; const AValue : TReplyListTyperepliesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property replies : TReplyListTyperepliesArray Index 16 Read Freplies Write Setreplies;
  end;
  TReplyListClass = Class of TReplyList;
  
  { --------------------------------------------------------------------
    TRevision
    --------------------------------------------------------------------}
  
  TRevision = Class(TGoogleBaseObject)
  Private
    Fid : String;
    FkeepForever : boolean;
    Fkind : String;
    FlastModifyingUser : TUser;
    Fmd5Checksum : String;
    FmimeType : String;
    FmodifiedTime : TDatetime;
    ForiginalFilename : String;
    FpublishAuto : boolean;
    F_published : boolean;
    FpublishedOutsideDomain : boolean;
    Fsize : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure SetkeepForever(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlastModifyingUser(AIndex : Integer; const AValue : TUser); virtual;
    Procedure Setmd5Checksum(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmimeType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmodifiedTime(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure SetoriginalFilename(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpublishAuto(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Set_published(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetpublishedOutsideDomain(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setsize(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property keepForever : boolean Index 8 Read FkeepForever Write SetkeepForever;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property lastModifyingUser : TUser Index 24 Read FlastModifyingUser Write SetlastModifyingUser;
    Property md5Checksum : String Index 32 Read Fmd5Checksum Write Setmd5Checksum;
    Property mimeType : String Index 40 Read FmimeType Write SetmimeType;
    Property modifiedTime : TDatetime Index 48 Read FmodifiedTime Write SetmodifiedTime;
    Property originalFilename : String Index 56 Read ForiginalFilename Write SetoriginalFilename;
    Property publishAuto : boolean Index 64 Read FpublishAuto Write SetpublishAuto;
    Property _published : boolean Index 72 Read F_published Write Set_published;
    Property publishedOutsideDomain : boolean Index 80 Read FpublishedOutsideDomain Write SetpublishedOutsideDomain;
    Property size : String Index 88 Read Fsize Write Setsize;
  end;
  TRevisionClass = Class of TRevision;
  
  { --------------------------------------------------------------------
    TRevisionList
    --------------------------------------------------------------------}
  
  TRevisionList = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Frevisions : TRevisionListTyperevisionsArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setrevisions(AIndex : Integer; const AValue : TRevisionListTyperevisionsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property revisions : TRevisionListTyperevisionsArray Index 8 Read Frevisions Write Setrevisions;
  end;
  TRevisionListClass = Class of TRevisionList;
  
  { --------------------------------------------------------------------
    TStartPageToken
    --------------------------------------------------------------------}
  
  TStartPageToken = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FstartPageToken : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstartPageToken(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property startPageToken : String Index 8 Read FstartPageToken Write SetstartPageToken;
  end;
  TStartPageTokenClass = Class of TStartPageToken;
  
  { --------------------------------------------------------------------
    TUser
    --------------------------------------------------------------------}
  
  TUser = Class(TGoogleBaseObject)
  Private
    FdisplayName : String;
    FemailAddress : String;
    Fkind : String;
    Fme : boolean;
    FpermissionId : String;
    FphotoLink : String;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetemailAddress(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setme(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetpermissionId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetphotoLink(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property displayName : String Index 0 Read FdisplayName Write SetdisplayName;
    Property emailAddress : String Index 8 Read FemailAddress Write SetemailAddress;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property me : boolean Index 24 Read Fme Write Setme;
    Property permissionId : String Index 32 Read FpermissionId Write SetpermissionId;
    Property photoLink : String Index 40 Read FphotoLink Write SetphotoLink;
  end;
  TUserClass = Class of TUser;
  
  { --------------------------------------------------------------------
    TAboutResource
    --------------------------------------------------------------------}
  
  TAboutResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get : TAbout;
  end;
  
  
  { --------------------------------------------------------------------
    TChangesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TChangesResource, method List
  
  TChangesListOptions = Record
    includeRemoved : boolean;
    pageSize : integer;
    pageToken : String;
    restrictToMyDrive : boolean;
    spaces : String;
  end;
  
  
  //Optional query Options for TChangesResource, method Watch
  
  TChangesWatchOptions = Record
    includeRemoved : boolean;
    pageSize : integer;
    pageToken : String;
    restrictToMyDrive : boolean;
    spaces : String;
  end;
  
  TChangesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function GetStartPageToken : TStartPageToken;
    Function List(AQuery : string  = '') : TChangeList;
    Function List(AQuery : TChangeslistOptions) : TChangeList;
    Function Watch(aChannel : TChannel; AQuery : string  = '') : TChannel;
    Function Watch(aChannel : TChannel; AQuery : TChangeswatchOptions) : TChannel;
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
    TCommentsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCommentsResource, method Get
  
  TCommentsGetOptions = Record
    includeDeleted : boolean;
  end;
  
  
  //Optional query Options for TCommentsResource, method List
  
  TCommentsListOptions = Record
    includeDeleted : boolean;
    pageSize : integer;
    pageToken : String;
    startModifiedTime : String;
  end;
  
  TCommentsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(fileId: string; aComment : TComment) : TComment;overload;
    Procedure Delete(commentId: string; fileId: string);
    Function Get(commentId: string; fileId: string; AQuery : string  = '') : TComment;
    Function Get(commentId: string; fileId: string; AQuery : TCommentsgetOptions) : TComment;
    Function List(fileId: string; AQuery : string  = '') : TCommentList;
    Function List(fileId: string; AQuery : TCommentslistOptions) : TCommentList;
    Function Update(commentId: string; fileId: string; aComment : TComment) : TComment;
  end;
  
  
  { --------------------------------------------------------------------
    TFilesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TFilesResource, method Copy
  
  TFilesCopyOptions = Record
    ignoreDefaultVisibility : boolean;
    keepRevisionForever : boolean;
    ocrLanguage : String;
  end;
  
  
  //Optional query Options for TFilesResource, method Create
  
  TFilesCreateOptions = Record
    ignoreDefaultVisibility : boolean;
    keepRevisionForever : boolean;
    ocrLanguage : String;
    useContentAsIndexableText : boolean;
  end;
  
  
  //Optional query Options for TFilesResource, method Export
  
  TFilesExportOptions = Record
    mimeType : String;
  end;
  
  
  //Optional query Options for TFilesResource, method GenerateIds
  
  TFilesGenerateIdsOptions = Record
    count : integer;
    space : String;
  end;
  
  
  //Optional query Options for TFilesResource, method Get
  
  TFilesGetOptions = Record
    acknowledgeAbuse : boolean;
  end;
  
  
  //Optional query Options for TFilesResource, method List
  
  TFilesListOptions = Record
    corpus : String;
    orderBy : String;
    pageSize : integer;
    pageToken : String;
    q : String;
    spaces : String;
    fields: String;
  end;
  
  
  //Optional query Options for TFilesResource, method Update
  
  TFilesUpdateOptions = Record
    addParents : String;
    keepRevisionForever : boolean;
    ocrLanguage : String;
    removeParents : String;
    useContentAsIndexableText : boolean;
  end;
  
  
  //Optional query Options for TFilesResource, method Watch
  
  TFilesWatchOptions = Record
    acknowledgeAbuse : boolean;
  end;
  
  TFilesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Copy(fileId: string; aFile : TFile; AQuery : string  = '') : TFile;
    Function Copy(fileId: string; aFile : TFile; AQuery : TFilescopyOptions) : TFile;
    Function Create(aFile : TFile; AQuery : string  = '') : TFile;overload;
    Function Create(aFile : TFile; AQuery : TFilescreateOptions) : TFile;overload;
    Procedure Delete(fileId: string);
    Procedure EmptyTrash;
    Procedure Export(fileId: string; AQuery : string  = '');
    Procedure Export(fileId: string; AQuery : TFilesexportOptions);
    Function GenerateIds(AQuery : string  = '') : TGeneratedIds;
    Function GenerateIds(AQuery : TFilesgenerateIdsOptions) : TGeneratedIds;
    Function Get(fileId: string; AQuery : string  = '') : TFile;
    Function Get(fileId: string; AQuery : TFilesgetOptions) : TFile;
    Function List(AQuery : string  = '') : TFileList;
    Function List(AQuery : TFileslistOptions) : TFileList;
    Function Update(fileId: string; aFile : TFile; AQuery : string  = '') : TFile;
    Function Update(fileId: string; aFile : TFile; AQuery : TFilesupdateOptions) : TFile;
    Function Watch(fileId: string; aChannel : TChannel; AQuery : string  = '') : TChannel;
    Function Watch(fileId: string; aChannel : TChannel; AQuery : TFileswatchOptions) : TChannel;
  end;
  
  
  { --------------------------------------------------------------------
    TPermissionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TPermissionsResource, method Create
  
  TPermissionsCreateOptions = Record
    emailMessage : String;
    sendNotificationEmail : boolean;
    transferOwnership : boolean;
  end;
  
  
  //Optional query Options for TPermissionsResource, method Update
  
  TPermissionsUpdateOptions = Record
    transferOwnership : boolean;
  end;
  
  TPermissionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(fileId: string; aPermission : TPermission; AQuery : string  = '') : TPermission;overload;
    Function Create(fileId: string; aPermission : TPermission; AQuery : TPermissionscreateOptions) : TPermission;overload;
    Procedure Delete(fileId: string; permissionId: string);
    Function Get(fileId: string; permissionId: string) : TPermission;
    Function List(fileId: string) : TPermissionList;
    Function Update(fileId: string; permissionId: string; aPermission : TPermission; AQuery : string  = '') : TPermission;
    Function Update(fileId: string; permissionId: string; aPermission : TPermission; AQuery : TPermissionsupdateOptions) : TPermission;
  end;
  
  
  { --------------------------------------------------------------------
    TRepliesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TRepliesResource, method Get
  
  TRepliesGetOptions = Record
    includeDeleted : boolean;
  end;
  
  
  //Optional query Options for TRepliesResource, method List
  
  TRepliesListOptions = Record
    includeDeleted : boolean;
    pageSize : integer;
    pageToken : String;
  end;
  
  TRepliesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(commentId: string; fileId: string; aReply : TReply) : TReply;overload;
    Procedure Delete(commentId: string; fileId: string; replyId: string);
    Function Get(commentId: string; fileId: string; replyId: string; AQuery : string  = '') : TReply;
    Function Get(commentId: string; fileId: string; replyId: string; AQuery : TRepliesgetOptions) : TReply;
    Function List(commentId: string; fileId: string; AQuery : string  = '') : TReplyList;
    Function List(commentId: string; fileId: string; AQuery : TReplieslistOptions) : TReplyList;
    Function Update(commentId: string; fileId: string; replyId: string; aReply : TReply) : TReply;
  end;
  
  
  { --------------------------------------------------------------------
    TRevisionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TRevisionsResource, method Get
  
  TRevisionsGetOptions = Record
    acknowledgeAbuse : boolean;
  end;
  
  TRevisionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(fileId: string; revisionId: string);
    Function Get(fileId: string; revisionId: string; AQuery : string  = '') : TRevision;
    Function Get(fileId: string; revisionId: string; AQuery : TRevisionsgetOptions) : TRevision;
    Function List(fileId: string) : TRevisionList;
    Function Update(fileId: string; revisionId: string; aRevision : TRevision) : TRevision;
  end;
  
  
  { --------------------------------------------------------------------
    TDriveAPI
    --------------------------------------------------------------------}
  
  TDriveAPI = Class(TGoogleAPI)
  Private
    FAboutInstance : TAboutResource;
    FChangesInstance : TChangesResource;
    FChannelsInstance : TChannelsResource;
    FCommentsInstance : TCommentsResource;
    FFilesInstance : TFilesResource;
    FPermissionsInstance : TPermissionsResource;
    FRepliesInstance : TRepliesResource;
    FRevisionsInstance : TRevisionsResource;
    Function GetAboutInstance : TAboutResource;virtual;
    Function GetChangesInstance : TChangesResource;virtual;
    Function GetChannelsInstance : TChannelsResource;virtual;
    Function GetCommentsInstance : TCommentsResource;virtual;
    Function GetFilesInstance : TFilesResource;virtual;
    Function GetPermissionsInstance : TPermissionsResource;virtual;
    Function GetRepliesInstance : TRepliesResource;virtual;
    Function GetRevisionsInstance : TRevisionsResource;virtual;
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
    Function CreateAboutResource(AOwner : TComponent) : TAboutResource;virtual;overload;
    Function CreateAboutResource : TAboutResource;virtual;overload;
    Function CreateChangesResource(AOwner : TComponent) : TChangesResource;virtual;overload;
    Function CreateChangesResource : TChangesResource;virtual;overload;
    Function CreateChannelsResource(AOwner : TComponent) : TChannelsResource;virtual;overload;
    Function CreateChannelsResource : TChannelsResource;virtual;overload;
    Function CreateCommentsResource(AOwner : TComponent) : TCommentsResource;virtual;overload;
    Function CreateCommentsResource : TCommentsResource;virtual;overload;
    Function CreateFilesResource(AOwner : TComponent) : TFilesResource;virtual;overload;
    Function CreateFilesResource : TFilesResource;virtual;overload;
    Function CreatePermissionsResource(AOwner : TComponent) : TPermissionsResource;virtual;overload;
    Function CreatePermissionsResource : TPermissionsResource;virtual;overload;
    Function CreateRepliesResource(AOwner : TComponent) : TRepliesResource;virtual;overload;
    Function CreateRepliesResource : TRepliesResource;virtual;overload;
    Function CreateRevisionsResource(AOwner : TComponent) : TRevisionsResource;virtual;overload;
    Function CreateRevisionsResource : TRevisionsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AboutResource : TAboutResource Read GetAboutInstance;
    Property ChangesResource : TChangesResource Read GetChangesInstance;
    Property ChannelsResource : TChannelsResource Read GetChannelsInstance;
    Property CommentsResource : TCommentsResource Read GetCommentsInstance;
    Property FilesResource : TFilesResource Read GetFilesInstance;
    Property PermissionsResource : TPermissionsResource Read GetPermissionsInstance;
    Property RepliesResource : TRepliesResource Read GetRepliesInstance;
    Property RevisionsResource : TRevisionsResource Read GetRevisionsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAboutTypeexportFormats
  --------------------------------------------------------------------}


Class Function TAboutTypeexportFormats.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TAboutTypeimportFormats
  --------------------------------------------------------------------}


Class Function TAboutTypeimportFormats.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TAboutTypemaxImportSizes
  --------------------------------------------------------------------}


Class Function TAboutTypemaxImportSizes.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TAboutTypestorageQuota
  --------------------------------------------------------------------}


Procedure TAboutTypestorageQuota.Setlimit(AIndex : Integer; const AValue : String); 

begin
  If (Flimit=AValue) then exit;
  Flimit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAboutTypestorageQuota.Setusage(AIndex : Integer; const AValue : String); 

begin
  If (Fusage=AValue) then exit;
  Fusage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAboutTypestorageQuota.SetusageInDrive(AIndex : Integer; const AValue : String); 

begin
  If (FusageInDrive=AValue) then exit;
  FusageInDrive:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAboutTypestorageQuota.SetusageInDriveTrash(AIndex : Integer; const AValue : String); 

begin
  If (FusageInDriveTrash=AValue) then exit;
  FusageInDriveTrash:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAbout
  --------------------------------------------------------------------}


Procedure TAbout.SetappInstalled(AIndex : Integer; const AValue : boolean); 

begin
  If (FappInstalled=AValue) then exit;
  FappInstalled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.SetexportFormats(AIndex : Integer; const AValue : TAboutTypeexportFormats); 

begin
  If (FexportFormats=AValue) then exit;
  FexportFormats:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.SetfolderColorPalette(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FfolderColorPalette=AValue) then exit;
  FfolderColorPalette:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.SetimportFormats(AIndex : Integer; const AValue : TAboutTypeimportFormats); 

begin
  If (FimportFormats=AValue) then exit;
  FimportFormats:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.SetmaxImportSizes(AIndex : Integer; const AValue : TAboutTypemaxImportSizes); 

begin
  If (FmaxImportSizes=AValue) then exit;
  FmaxImportSizes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.SetmaxUploadSize(AIndex : Integer; const AValue : String); 

begin
  If (FmaxUploadSize=AValue) then exit;
  FmaxUploadSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.SetstorageQuota(AIndex : Integer; const AValue : TAboutTypestorageQuota); 

begin
  If (FstorageQuota=AValue) then exit;
  FstorageQuota:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.Setuser(AIndex : Integer; const AValue : TUser); 

begin
  If (Fuser=AValue) then exit;
  Fuser:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAbout.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'foldercolorpalette' : SetLength(FfolderColorPalette,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TChange
  --------------------------------------------------------------------}


Procedure TChange.Set_file(AIndex : Integer; const AValue : TFile); 

begin
  If (F_file=AValue) then exit;
  F_file:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChange.SetfileId(AIndex : Integer; const AValue : String); 

begin
  If (FfileId=AValue) then exit;
  FfileId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChange.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChange.Setremoved(AIndex : Integer; const AValue : boolean); 

begin
  If (Fremoved=AValue) then exit;
  Fremoved:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChange.Settime(AIndex : Integer; const AValue : TDatetime); 

begin
  If (Ftime=AValue) then exit;
  Ftime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TChange.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_file' : Result:='file';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TChangeList
  --------------------------------------------------------------------}


Procedure TChangeList.Setchanges(AIndex : Integer; const AValue : TChangeListTypechangesArray); 

begin
  If (Fchanges=AValue) then exit;
  Fchanges:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangeList.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangeList.SetnewStartPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnewStartPageToken=AValue) then exit;
  FnewStartPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangeList.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TChangeList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'changes' : SetLength(Fchanges,ALength);
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


Procedure TChannel.Setaddress(AIndex : Integer; const AValue : String); 

begin
  If (Faddress=AValue) then exit;
  Faddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Setexpiration(AIndex : Integer; const AValue : String); 

begin
  If (Fexpiration=AValue) then exit;
  Fexpiration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Setparams(AIndex : Integer; const AValue : TChannelTypeparams); 

begin
  If (Fparams=AValue) then exit;
  Fparams:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Setpayload(AIndex : Integer; const AValue : boolean); 

begin
  If (Fpayload=AValue) then exit;
  Fpayload:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.SetresourceId(AIndex : Integer; const AValue : String); 

begin
  If (FresourceId=AValue) then exit;
  FresourceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.SetresourceUri(AIndex : Integer; const AValue : String); 

begin
  If (FresourceUri=AValue) then exit;
  FresourceUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Settoken(AIndex : Integer; const AValue : String); 

begin
  If (Ftoken=AValue) then exit;
  Ftoken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Set_type(AIndex : Integer; const AValue : String); 

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
  TCommentTypequotedFileContent
  --------------------------------------------------------------------}


Procedure TCommentTypequotedFileContent.SetmimeType(AIndex : Integer; const AValue : String); 

begin
  If (FmimeType=AValue) then exit;
  FmimeType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentTypequotedFileContent.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TComment
  --------------------------------------------------------------------}


Procedure TComment.Setanchor(AIndex : Integer; const AValue : String); 

begin
  If (Fanchor=AValue) then exit;
  Fanchor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setauthor(AIndex : Integer; const AValue : TUser); 

begin
  If (Fauthor=AValue) then exit;
  Fauthor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setcontent(AIndex : Integer; const AValue : String); 

begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.SetcreatedTime(AIndex : Integer; const AValue : TDatetime); 

begin
  If (FcreatedTime=AValue) then exit;
  FcreatedTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setdeleted(AIndex : Integer; const AValue : boolean); 

begin
  If (Fdeleted=AValue) then exit;
  Fdeleted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.SethtmlContent(AIndex : Integer; const AValue : String); 

begin
  If (FhtmlContent=AValue) then exit;
  FhtmlContent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.SetmodifiedTime(AIndex : Integer; const AValue : TDatetime); 

begin
  If (FmodifiedTime=AValue) then exit;
  FmodifiedTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.SetquotedFileContent(AIndex : Integer; const AValue : TCommentTypequotedFileContent); 

begin
  If (FquotedFileContent=AValue) then exit;
  FquotedFileContent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setreplies(AIndex : Integer; const AValue : TCommentTyperepliesArray); 

begin
  If (Freplies=AValue) then exit;
  Freplies:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setresolved(AIndex : Integer; const AValue : boolean); 

begin
  If (Fresolved=AValue) then exit;
  Fresolved:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TComment.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'replies' : SetLength(Freplies,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCommentList
  --------------------------------------------------------------------}


Procedure TCommentList.Setcomments(AIndex : Integer; const AValue : TCommentListTypecommentsArray); 

begin
  If (Fcomments=AValue) then exit;
  Fcomments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentList.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentList.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCommentList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'comments' : SetLength(Fcomments,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TFileTypeappProperties
  --------------------------------------------------------------------}


Class Function TFileTypeappProperties.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TFileTypecapabilities
  --------------------------------------------------------------------}


Procedure TFileTypecapabilities.SetcanComment(AIndex : Integer; const AValue : boolean); 

begin
  If (FcanComment=AValue) then exit;
  FcanComment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypecapabilities.SetcanCopy(AIndex : Integer; const AValue : boolean); 

begin
  If (FcanCopy=AValue) then exit;
  FcanCopy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypecapabilities.SetcanEdit(AIndex : Integer; const AValue : boolean); 

begin
  If (FcanEdit=AValue) then exit;
  FcanEdit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypecapabilities.SetcanReadRevisions(AIndex : Integer; const AValue : boolean); 

begin
  If (FcanReadRevisions=AValue) then exit;
  FcanReadRevisions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypecapabilities.SetcanShare(AIndex : Integer; const AValue : boolean); 

begin
  If (FcanShare=AValue) then exit;
  FcanShare:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFileTypecontentHintsTypethumbnail
  --------------------------------------------------------------------}


Procedure TFileTypecontentHintsTypethumbnail.Setimage(AIndex : Integer; const AValue : String); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypecontentHintsTypethumbnail.SetmimeType(AIndex : Integer; const AValue : String); 

begin
  If (FmimeType=AValue) then exit;
  FmimeType:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFileTypecontentHints
  --------------------------------------------------------------------}


Procedure TFileTypecontentHints.SetindexableText(AIndex : Integer; const AValue : String); 

begin
  If (FindexableText=AValue) then exit;
  FindexableText:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypecontentHints.Setthumbnail(AIndex : Integer; const AValue : TFileTypecontentHintsTypethumbnail); 

begin
  If (Fthumbnail=AValue) then exit;
  Fthumbnail:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFileTypeimageMediaMetadataTypelocation
  --------------------------------------------------------------------}


Procedure TFileTypeimageMediaMetadataTypelocation.Setaltitude(AIndex : Integer; const AValue : double); 

begin
  If (Faltitude=AValue) then exit;
  Faltitude:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypeimageMediaMetadataTypelocation.Setlatitude(AIndex : Integer; const AValue : double); 

begin
  If (Flatitude=AValue) then exit;
  Flatitude:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypeimageMediaMetadataTypelocation.Setlongitude(AIndex : Integer; const AValue : double); 

begin
  If (Flongitude=AValue) then exit;
  Flongitude:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFileTypeimageMediaMetadata
  --------------------------------------------------------------------}


Procedure TFileTypeimageMediaMetadata.Setaperture(AIndex : Integer; const AValue : integer); 

begin
  If (Faperture=AValue) then exit;
  Faperture:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypeimageMediaMetadata.SetcameraMake(AIndex : Integer; const AValue : String); 

begin
  If (FcameraMake=AValue) then exit;
  FcameraMake:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypeimageMediaMetadata.SetcameraModel(AIndex : Integer; const AValue : String); 

begin
  If (FcameraModel=AValue) then exit;
  FcameraModel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypeimageMediaMetadata.SetcolorSpace(AIndex : Integer; const AValue : String); 

begin
  If (FcolorSpace=AValue) then exit;
  FcolorSpace:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypeimageMediaMetadata.SetexposureBias(AIndex : Integer; const AValue : integer); 

begin
  If (FexposureBias=AValue) then exit;
  FexposureBias:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypeimageMediaMetadata.SetexposureMode(AIndex : Integer; const AValue : String); 

begin
  If (FexposureMode=AValue) then exit;
  FexposureMode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypeimageMediaMetadata.SetexposureTime(AIndex : Integer; const AValue : integer); 

begin
  If (FexposureTime=AValue) then exit;
  FexposureTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypeimageMediaMetadata.SetflashUsed(AIndex : Integer; const AValue : boolean); 

begin
  If (FflashUsed=AValue) then exit;
  FflashUsed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypeimageMediaMetadata.SetfocalLength(AIndex : Integer; const AValue : integer); 

begin
  If (FfocalLength=AValue) then exit;
  FfocalLength:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypeimageMediaMetadata.Setheight(AIndex : Integer; const AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypeimageMediaMetadata.SetisoSpeed(AIndex : Integer; const AValue : integer); 

begin
  If (FisoSpeed=AValue) then exit;
  FisoSpeed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypeimageMediaMetadata.Setlens(AIndex : Integer; const AValue : String); 

begin
  If (Flens=AValue) then exit;
  Flens:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypeimageMediaMetadata.Setlocation(AIndex : Integer; const AValue : TFileTypeimageMediaMetadataTypelocation); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypeimageMediaMetadata.SetmaxApertureValue(AIndex : Integer; const AValue : integer); 

begin
  If (FmaxApertureValue=AValue) then exit;
  FmaxApertureValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypeimageMediaMetadata.SetmeteringMode(AIndex : Integer; const AValue : String); 

begin
  If (FmeteringMode=AValue) then exit;
  FmeteringMode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypeimageMediaMetadata.Setrotation(AIndex : Integer; const AValue : integer); 

begin
  If (Frotation=AValue) then exit;
  Frotation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypeimageMediaMetadata.Setsensor(AIndex : Integer; const AValue : String); 

begin
  If (Fsensor=AValue) then exit;
  Fsensor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypeimageMediaMetadata.SetsubjectDistance(AIndex : Integer; const AValue : integer); 

begin
  If (FsubjectDistance=AValue) then exit;
  FsubjectDistance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypeimageMediaMetadata.Settime(AIndex : Integer; const AValue : String); 

begin
  If (Ftime=AValue) then exit;
  Ftime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypeimageMediaMetadata.SetwhiteBalance(AIndex : Integer; const AValue : String); 

begin
  If (FwhiteBalance=AValue) then exit;
  FwhiteBalance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypeimageMediaMetadata.Setwidth(AIndex : Integer; const AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFileTypeproperties
  --------------------------------------------------------------------}


Class Function TFileTypeproperties.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TFileTypevideoMediaMetadata
  --------------------------------------------------------------------}


Procedure TFileTypevideoMediaMetadata.SetdurationMillis(AIndex : Integer; const AValue : String); 

begin
  If (FdurationMillis=AValue) then exit;
  FdurationMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypevideoMediaMetadata.Setheight(AIndex : Integer; const AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileTypevideoMediaMetadata.Setwidth(AIndex : Integer; const AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFile
  --------------------------------------------------------------------}


Procedure TFile.SetappProperties(AIndex : Integer; const AValue : TFileTypeappProperties); 

begin
  If (FappProperties=AValue) then exit;
  FappProperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setcapabilities(AIndex : Integer; const AValue : TFileTypecapabilities); 

begin
  If (Fcapabilities=AValue) then exit;
  Fcapabilities:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetcontentHints(AIndex : Integer; const AValue : TFileTypecontentHints); 

begin
  If (FcontentHints=AValue) then exit;
  FcontentHints:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetcreatedTime(AIndex : Integer; const AValue : TDatetime); 

begin
  If (FcreatedTime=AValue) then exit;
  FcreatedTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetexplicitlyTrashed(AIndex : Integer; const AValue : boolean); 

begin
  If (FexplicitlyTrashed=AValue) then exit;
  FexplicitlyTrashed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetfileExtension(AIndex : Integer; const AValue : String); 

begin
  If (FfileExtension=AValue) then exit;
  FfileExtension:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetfolderColorRgb(AIndex : Integer; const AValue : String); 

begin
  If (FfolderColorRgb=AValue) then exit;
  FfolderColorRgb:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetfullFileExtension(AIndex : Integer; const AValue : String); 

begin
  If (FfullFileExtension=AValue) then exit;
  FfullFileExtension:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetheadRevisionId(AIndex : Integer; const AValue : String); 

begin
  If (FheadRevisionId=AValue) then exit;
  FheadRevisionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SeticonLink(AIndex : Integer; const AValue : String); 

begin
  If (FiconLink=AValue) then exit;
  FiconLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetimageMediaMetadata(AIndex : Integer; const AValue : TFileTypeimageMediaMetadata); 

begin
  If (FimageMediaMetadata=AValue) then exit;
  FimageMediaMetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetisAppAuthorized(AIndex : Integer; const AValue : boolean); 

begin
  If (FisAppAuthorized=AValue) then exit;
  FisAppAuthorized:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetlastModifyingUser(AIndex : Integer; const AValue : TUser); 

begin
  If (FlastModifyingUser=AValue) then exit;
  FlastModifyingUser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setmd5Checksum(AIndex : Integer; const AValue : String); 

begin
  If (Fmd5Checksum=AValue) then exit;
  Fmd5Checksum:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetmimeType(AIndex : Integer; const AValue : String); 

begin
  If (FmimeType=AValue) then exit;
  FmimeType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetmodifiedByMeTime(AIndex : Integer; const AValue : TDatetime); 

begin
  If (FmodifiedByMeTime=AValue) then exit;
  FmodifiedByMeTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetmodifiedTime(AIndex : Integer; const AValue : TDatetime); 

begin
  If (FmodifiedTime=AValue) then exit;
  FmodifiedTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetoriginalFilename(AIndex : Integer; const AValue : String); 

begin
  If (ForiginalFilename=AValue) then exit;
  ForiginalFilename:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetownedByMe(AIndex : Integer; const AValue : boolean); 

begin
  If (FownedByMe=AValue) then exit;
  FownedByMe:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setowners(AIndex : Integer; const AValue : TFileTypeownersArray); 

begin
  If (Fowners=AValue) then exit;
  Fowners:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setparents(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fparents=AValue) then exit;
  Fparents:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setpermissions(AIndex : Integer; const AValue : TFileTypepermissionsArray); 

begin
  If (Fpermissions=AValue) then exit;
  Fpermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setproperties(AIndex : Integer; const AValue : TFileTypeproperties); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetquotaBytesUsed(AIndex : Integer; const AValue : String); 

begin
  If (FquotaBytesUsed=AValue) then exit;
  FquotaBytesUsed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setshared(AIndex : Integer; const AValue : boolean); 

begin
  If (Fshared=AValue) then exit;
  Fshared:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetsharedWithMeTime(AIndex : Integer; const AValue : TDatetime); 

begin
  If (FsharedWithMeTime=AValue) then exit;
  FsharedWithMeTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetsharingUser(AIndex : Integer; const AValue : TUser); 

begin
  If (FsharingUser=AValue) then exit;
  FsharingUser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setsize(AIndex : Integer; const AValue : String); 

begin
  If (Fsize=AValue) then exit;
  Fsize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setspaces(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fspaces=AValue) then exit;
  Fspaces:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setstarred(AIndex : Integer; const AValue : boolean); 

begin
  If (Fstarred=AValue) then exit;
  Fstarred:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetthumbnailLink(AIndex : Integer; const AValue : String); 

begin
  If (FthumbnailLink=AValue) then exit;
  FthumbnailLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Settrashed(AIndex : Integer; const AValue : boolean); 

begin
  If (Ftrashed=AValue) then exit;
  Ftrashed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setversion(AIndex : Integer; const AValue : String); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetvideoMediaMetadata(AIndex : Integer; const AValue : TFileTypevideoMediaMetadata); 

begin
  If (FvideoMediaMetadata=AValue) then exit;
  FvideoMediaMetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetviewedByMe(AIndex : Integer; const AValue : boolean); 

begin
  If (FviewedByMe=AValue) then exit;
  FviewedByMe:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetviewedByMeTime(AIndex : Integer; const AValue : TDatetime); 

begin
  If (FviewedByMeTime=AValue) then exit;
  FviewedByMeTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetviewersCanCopyContent(AIndex : Integer; const AValue : boolean); 

begin
  If (FviewersCanCopyContent=AValue) then exit;
  FviewersCanCopyContent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetwebContentLink(AIndex : Integer; const AValue : String); 

begin
  If (FwebContentLink=AValue) then exit;
  FwebContentLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetwebViewLink(AIndex : Integer; const AValue : String); 

begin
  If (FwebViewLink=AValue) then exit;
  FwebViewLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetwritersCanShare(AIndex : Integer; const AValue : boolean); 

begin
  If (FwritersCanShare=AValue) then exit;
  FwritersCanShare:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TFile.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'owners' : SetLength(Fowners,ALength);
  'parents' : SetLength(Fparents,ALength);
  'permissions' : SetLength(Fpermissions,ALength);
  'spaces' : SetLength(Fspaces,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TFileList
  --------------------------------------------------------------------}


Procedure TFileList.Setfiles(AIndex : Integer; const AValue : TFileListTypefilesArray); 

begin
  If (Ffiles=AValue) then exit;
  Ffiles:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileList.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileList.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TFileList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'files' : SetLength(Ffiles,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGeneratedIds
  --------------------------------------------------------------------}


Procedure TGeneratedIds.Setids(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fids=AValue) then exit;
  Fids:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeneratedIds.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeneratedIds.Setspace(AIndex : Integer; const AValue : String); 

begin
  If (Fspace=AValue) then exit;
  Fspace:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGeneratedIds.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'ids' : SetLength(Fids,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPermission
  --------------------------------------------------------------------}


Procedure TPermission.SetallowFileDiscovery(AIndex : Integer; const AValue : boolean); 

begin
  If (FallowFileDiscovery=AValue) then exit;
  FallowFileDiscovery:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.Setdomain(AIndex : Integer; const AValue : String); 

begin
  If (Fdomain=AValue) then exit;
  Fdomain:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.SetemailAddress(AIndex : Integer; const AValue : String); 

begin
  If (FemailAddress=AValue) then exit;
  FemailAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.SetphotoLink(AIndex : Integer; const AValue : String); 

begin
  If (FphotoLink=AValue) then exit;
  FphotoLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.Setrole(AIndex : Integer; const AValue : String); 

begin
  If (Frole=AValue) then exit;
  Frole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPermission.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPermissionList
  --------------------------------------------------------------------}


Procedure TPermissionList.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermissionList.Setpermissions(AIndex : Integer; const AValue : TPermissionListTypepermissionsArray); 

begin
  If (Fpermissions=AValue) then exit;
  Fpermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPermissionList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'permissions' : SetLength(Fpermissions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TReply
  --------------------------------------------------------------------}


Procedure TReply.Setaction(AIndex : Integer; const AValue : String); 

begin
  If (Faction=AValue) then exit;
  Faction:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReply.Setauthor(AIndex : Integer; const AValue : TUser); 

begin
  If (Fauthor=AValue) then exit;
  Fauthor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReply.Setcontent(AIndex : Integer; const AValue : String); 

begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReply.SetcreatedTime(AIndex : Integer; const AValue : TDatetime); 

begin
  If (FcreatedTime=AValue) then exit;
  FcreatedTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReply.Setdeleted(AIndex : Integer; const AValue : boolean); 

begin
  If (Fdeleted=AValue) then exit;
  Fdeleted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReply.SethtmlContent(AIndex : Integer; const AValue : String); 

begin
  If (FhtmlContent=AValue) then exit;
  FhtmlContent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReply.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReply.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReply.SetmodifiedTime(AIndex : Integer; const AValue : TDatetime); 

begin
  If (FmodifiedTime=AValue) then exit;
  FmodifiedTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReplyList
  --------------------------------------------------------------------}


Procedure TReplyList.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplyList.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplyList.Setreplies(AIndex : Integer; const AValue : TReplyListTyperepliesArray); 

begin
  If (Freplies=AValue) then exit;
  Freplies:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TReplyList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'replies' : SetLength(Freplies,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TRevision
  --------------------------------------------------------------------}


Procedure TRevision.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevision.SetkeepForever(AIndex : Integer; const AValue : boolean); 

begin
  If (FkeepForever=AValue) then exit;
  FkeepForever:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevision.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevision.SetlastModifyingUser(AIndex : Integer; const AValue : TUser); 

begin
  If (FlastModifyingUser=AValue) then exit;
  FlastModifyingUser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevision.Setmd5Checksum(AIndex : Integer; const AValue : String); 

begin
  If (Fmd5Checksum=AValue) then exit;
  Fmd5Checksum:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevision.SetmimeType(AIndex : Integer; const AValue : String); 

begin
  If (FmimeType=AValue) then exit;
  FmimeType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevision.SetmodifiedTime(AIndex : Integer; const AValue : TDatetime); 

begin
  If (FmodifiedTime=AValue) then exit;
  FmodifiedTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevision.SetoriginalFilename(AIndex : Integer; const AValue : String); 

begin
  If (ForiginalFilename=AValue) then exit;
  ForiginalFilename:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevision.SetpublishAuto(AIndex : Integer; const AValue : boolean); 

begin
  If (FpublishAuto=AValue) then exit;
  FpublishAuto:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevision.Set_published(AIndex : Integer; const AValue : boolean); 

begin
  If (F_published=AValue) then exit;
  F_published:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevision.SetpublishedOutsideDomain(AIndex : Integer; const AValue : boolean); 

begin
  If (FpublishedOutsideDomain=AValue) then exit;
  FpublishedOutsideDomain:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevision.Setsize(AIndex : Integer; const AValue : String); 

begin
  If (Fsize=AValue) then exit;
  Fsize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TRevision.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_published' : Result:='published';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TRevisionList
  --------------------------------------------------------------------}


Procedure TRevisionList.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevisionList.Setrevisions(AIndex : Integer; const AValue : TRevisionListTyperevisionsArray); 

begin
  If (Frevisions=AValue) then exit;
  Frevisions:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TRevisionList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'revisions' : SetLength(Frevisions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TStartPageToken
  --------------------------------------------------------------------}


Procedure TStartPageToken.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStartPageToken.SetstartPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FstartPageToken=AValue) then exit;
  FstartPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUser
  --------------------------------------------------------------------}


Procedure TUser.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.SetemailAddress(AIndex : Integer; const AValue : String); 

begin
  If (FemailAddress=AValue) then exit;
  FemailAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setme(AIndex : Integer; const AValue : boolean); 

begin
  If (Fme=AValue) then exit;
  Fme:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.SetpermissionId(AIndex : Integer; const AValue : String); 

begin
  If (FpermissionId=AValue) then exit;
  FpermissionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.SetphotoLink(AIndex : Integer; const AValue : String); 

begin
  If (FphotoLink=AValue) then exit;
  FphotoLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAboutResource
  --------------------------------------------------------------------}


Class Function TAboutResource.ResourceName : String;

begin
  Result:='about';
end;

Class Function TAboutResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdriveAPI;
end;

Function TAboutResource.Get : TAbout;

Const
  _HTTPMethod = 'GET';
  _Path       = 'about';
  _Methodid   = 'drive.about.get';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TAbout) as TAbout;
end;



{ --------------------------------------------------------------------
  TChangesResource
  --------------------------------------------------------------------}


Class Function TChangesResource.ResourceName : String;

begin
  Result:='changes';
end;

Class Function TChangesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdriveAPI;
end;

Function TChangesResource.GetStartPageToken : TStartPageToken;

Const
  _HTTPMethod = 'GET';
  _Path       = 'changes/startPageToken';
  _Methodid   = 'drive.changes.getStartPageToken';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TStartPageToken) as TStartPageToken;
end;

Function TChangesResource.List(AQuery : string = '') : TChangeList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'changes';
  _Methodid   = 'drive.changes.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TChangeList) as TChangeList;
end;


Function TChangesResource.List(AQuery : TChangeslistOptions) : TChangeList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'includeRemoved',AQuery.includeRemoved);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'restrictToMyDrive',AQuery.restrictToMyDrive);
  AddToQuery(_Q,'spaces',AQuery.spaces);
  Result:=List(_Q);
end;

Function TChangesResource.Watch(aChannel : TChannel; AQuery : string = '') : TChannel;

Const
  _HTTPMethod = 'POST';
  _Path       = 'changes/watch';
  _Methodid   = 'drive.changes.watch';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aChannel,TChannel) as TChannel;
end;


Function TChangesResource.Watch(aChannel : TChannel; AQuery : TChangeswatchOptions) : TChannel;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'includeRemoved',AQuery.includeRemoved);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'restrictToMyDrive',AQuery.restrictToMyDrive);
  AddToQuery(_Q,'spaces',AQuery.spaces);
  Result:=Watch(aChannel,_Q);
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
  Result:=TdriveAPI;
end;

Procedure TChannelsResource.Stop(aChannel : TChannel);

Const
  _HTTPMethod = 'POST';
  _Path       = 'channels/stop';
  _Methodid   = 'drive.channels.stop';

begin
  ServiceCall(_HTTPMethod,_Path,'',aChannel,Nil);
end;



{ --------------------------------------------------------------------
  TCommentsResource
  --------------------------------------------------------------------}


Class Function TCommentsResource.ResourceName : String;

begin
  Result:='comments';
end;

Class Function TCommentsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdriveAPI;
end;

Function TCommentsResource.Create(fileId: string; aComment : TComment) : TComment;

Const
  _HTTPMethod = 'POST';
  _Path       = 'files/{fileId}/comments';
  _Methodid   = 'drive.comments.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aComment,TComment) as TComment;
end;

Procedure TCommentsResource.Delete(commentId: string; fileId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'files/{fileId}/comments/{commentId}';
  _Methodid   = 'drive.comments.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['commentId',commentId,'fileId',fileId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TCommentsResource.Get(commentId: string; fileId: string; AQuery : string = '') : TComment;

Const
  _HTTPMethod = 'GET';
  _Path       = 'files/{fileId}/comments/{commentId}';
  _Methodid   = 'drive.comments.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['commentId',commentId,'fileId',fileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TComment) as TComment;
end;


Function TCommentsResource.Get(commentId: string; fileId: string; AQuery : TCommentsgetOptions) : TComment;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'includeDeleted',AQuery.includeDeleted);
  Result:=Get(commentId,fileId,_Q);
end;

Function TCommentsResource.List(fileId: string; AQuery : string = '') : TCommentList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'files/{fileId}/comments';
  _Methodid   = 'drive.comments.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TCommentList) as TCommentList;
end;


Function TCommentsResource.List(fileId: string; AQuery : TCommentslistOptions) : TCommentList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'includeDeleted',AQuery.includeDeleted);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'startModifiedTime',AQuery.startModifiedTime);
  Result:=List(fileId,_Q);
end;

Function TCommentsResource.Update(commentId: string; fileId: string; aComment : TComment) : TComment;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'files/{fileId}/comments/{commentId}';
  _Methodid   = 'drive.comments.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['commentId',commentId,'fileId',fileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aComment,TComment) as TComment;
end;



{ --------------------------------------------------------------------
  TFilesResource
  --------------------------------------------------------------------}


Class Function TFilesResource.ResourceName : String;

begin
  Result:='files';
end;

Class Function TFilesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdriveAPI;
end;

Function TFilesResource.Copy(fileId: string; aFile : TFile; AQuery : string = '') : TFile;

Const
  _HTTPMethod = 'POST';
  _Path       = 'files/{fileId}/copy';
  _Methodid   = 'drive.files.copy';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aFile,TFile) as TFile;
end;


Function TFilesResource.Copy(fileId: string; aFile : TFile; AQuery : TFilescopyOptions) : TFile;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'ignoreDefaultVisibility',AQuery.ignoreDefaultVisibility);
  AddToQuery(_Q,'keepRevisionForever',AQuery.keepRevisionForever);
  AddToQuery(_Q,'ocrLanguage',AQuery.ocrLanguage);
  Result:=Copy(fileId,aFile,_Q);
end;

Function TFilesResource.Create(aFile : TFile; AQuery : string = '') : TFile;

Const
  _HTTPMethod = 'POST';
  _Path       = 'files';
  _Methodid   = 'drive.files.create';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aFile,TFile) as TFile;
end;


Function TFilesResource.Create(aFile : TFile; AQuery : TFilescreateOptions) : TFile;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'ignoreDefaultVisibility',AQuery.ignoreDefaultVisibility);
  AddToQuery(_Q,'keepRevisionForever',AQuery.keepRevisionForever);
  AddToQuery(_Q,'ocrLanguage',AQuery.ocrLanguage);
  AddToQuery(_Q,'useContentAsIndexableText',AQuery.useContentAsIndexableText);
  Result:=Create(aFile,_Q);
end;

Procedure TFilesResource.Delete(fileId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'files/{fileId}';
  _Methodid   = 'drive.files.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Procedure TFilesResource.EmptyTrash;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'files/trash';
  _Methodid   = 'drive.files.emptyTrash';

begin
  ServiceCall(_HTTPMethod,_Path,'',Nil,Nil);
end;

Procedure TFilesResource.Export(fileId: string; AQuery : string = '');

Const
  _HTTPMethod = 'GET';
  _Path       = 'files/{fileId}/export';
  _Methodid   = 'drive.files.export';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId]);
  ServiceCall(_HTTPMethod,_P,AQuery,Nil,Nil);
end;


Procedure TFilesResource.Export(fileId: string; AQuery : TFilesexportOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'mimeType',AQuery.mimeType);
  Export(fileId,_Q);
end;

Function TFilesResource.GenerateIds(AQuery : string = '') : TGeneratedIds;

Const
  _HTTPMethod = 'GET';
  _Path       = 'files/generateIds';
  _Methodid   = 'drive.files.generateIds';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TGeneratedIds) as TGeneratedIds;
end;


Function TFilesResource.GenerateIds(AQuery : TFilesgenerateIdsOptions) : TGeneratedIds;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'count',AQuery.count);
  AddToQuery(_Q,'space',AQuery.space);
  Result:=GenerateIds(_Q);
end;

Function TFilesResource.Get(fileId: string; AQuery : string = '') : TFile;

Const
  _HTTPMethod = 'GET';
  _Path       = 'files/{fileId}';
  _Methodid   = 'drive.files.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TFile) as TFile;
end;


Function TFilesResource.Get(fileId: string; AQuery : TFilesgetOptions) : TFile;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'acknowledgeAbuse',AQuery.acknowledgeAbuse);
  Result:=Get(fileId,_Q);
end;

Function TFilesResource.List(AQuery : string = '') : TFileList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'files';
  _Methodid   = 'drive.files.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TFileList) as TFileList;
end;


Function TFilesResource.List(AQuery : TFileslistOptions) : TFileList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'corpus',AQuery.corpus);
  AddToQuery(_Q,'orderBy',AQuery.orderBy);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'q',AQuery.q);
  AddToQuery(_Q,'spaces',AQuery.spaces);
  AddToQuery(_Q,'fields',AQuery.fields);
  Result:=List(_Q);
end;

Function TFilesResource.Update(fileId: string; aFile : TFile; AQuery : string = '') : TFile;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'files/{fileId}';
  _Methodid   = 'drive.files.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aFile,TFile) as TFile;
end;


Function TFilesResource.Update(fileId: string; aFile : TFile; AQuery : TFilesupdateOptions) : TFile;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'addParents',AQuery.addParents);
  AddToQuery(_Q,'keepRevisionForever',AQuery.keepRevisionForever);
  AddToQuery(_Q,'ocrLanguage',AQuery.ocrLanguage);
  AddToQuery(_Q,'removeParents',AQuery.removeParents);
  AddToQuery(_Q,'useContentAsIndexableText',AQuery.useContentAsIndexableText);
  Result:=Update(fileId,aFile,_Q);
end;

Function TFilesResource.Watch(fileId: string; aChannel : TChannel; AQuery : string = '') : TChannel;

Const
  _HTTPMethod = 'POST';
  _Path       = 'files/{fileId}/watch';
  _Methodid   = 'drive.files.watch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aChannel,TChannel) as TChannel;
end;


Function TFilesResource.Watch(fileId: string; aChannel : TChannel; AQuery : TFileswatchOptions) : TChannel;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'acknowledgeAbuse',AQuery.acknowledgeAbuse);
  Result:=Watch(fileId,aChannel,_Q);
end;



{ --------------------------------------------------------------------
  TPermissionsResource
  --------------------------------------------------------------------}


Class Function TPermissionsResource.ResourceName : String;

begin
  Result:='permissions';
end;

Class Function TPermissionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdriveAPI;
end;

Function TPermissionsResource.Create(fileId: string; aPermission : TPermission; AQuery : string = '') : TPermission;

Const
  _HTTPMethod = 'POST';
  _Path       = 'files/{fileId}/permissions';
  _Methodid   = 'drive.permissions.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aPermission,TPermission) as TPermission;
end;


Function TPermissionsResource.Create(fileId: string; aPermission : TPermission; AQuery : TPermissionscreateOptions) : TPermission;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'emailMessage',AQuery.emailMessage);
  AddToQuery(_Q,'sendNotificationEmail',AQuery.sendNotificationEmail);
  AddToQuery(_Q,'transferOwnership',AQuery.transferOwnership);
  Result:=Create(fileId,aPermission,_Q);
end;

Procedure TPermissionsResource.Delete(fileId: string; permissionId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'files/{fileId}/permissions/{permissionId}';
  _Methodid   = 'drive.permissions.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId,'permissionId',permissionId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TPermissionsResource.Get(fileId: string; permissionId: string) : TPermission;

Const
  _HTTPMethod = 'GET';
  _Path       = 'files/{fileId}/permissions/{permissionId}';
  _Methodid   = 'drive.permissions.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId,'permissionId',permissionId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPermission) as TPermission;
end;

Function TPermissionsResource.List(fileId: string) : TPermissionList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'files/{fileId}/permissions';
  _Methodid   = 'drive.permissions.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPermissionList) as TPermissionList;
end;

Function TPermissionsResource.Update(fileId: string; permissionId: string; aPermission : TPermission; AQuery : string = '') : TPermission;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'files/{fileId}/permissions/{permissionId}';
  _Methodid   = 'drive.permissions.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId,'permissionId',permissionId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aPermission,TPermission) as TPermission;
end;


Function TPermissionsResource.Update(fileId: string; permissionId: string; aPermission : TPermission; AQuery : TPermissionsupdateOptions) : TPermission;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'transferOwnership',AQuery.transferOwnership);
  Result:=Update(fileId,permissionId,aPermission,_Q);
end;



{ --------------------------------------------------------------------
  TRepliesResource
  --------------------------------------------------------------------}


Class Function TRepliesResource.ResourceName : String;

begin
  Result:='replies';
end;

Class Function TRepliesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdriveAPI;
end;

Function TRepliesResource.Create(commentId: string; fileId: string; aReply : TReply) : TReply;

Const
  _HTTPMethod = 'POST';
  _Path       = 'files/{fileId}/comments/{commentId}/replies';
  _Methodid   = 'drive.replies.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['commentId',commentId,'fileId',fileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aReply,TReply) as TReply;
end;

Procedure TRepliesResource.Delete(commentId: string; fileId: string; replyId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'files/{fileId}/comments/{commentId}/replies/{replyId}';
  _Methodid   = 'drive.replies.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['commentId',commentId,'fileId',fileId,'replyId',replyId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TRepliesResource.Get(commentId: string; fileId: string; replyId: string; AQuery : string = '') : TReply;

Const
  _HTTPMethod = 'GET';
  _Path       = 'files/{fileId}/comments/{commentId}/replies/{replyId}';
  _Methodid   = 'drive.replies.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['commentId',commentId,'fileId',fileId,'replyId',replyId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TReply) as TReply;
end;


Function TRepliesResource.Get(commentId: string; fileId: string; replyId: string; AQuery : TRepliesgetOptions) : TReply;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'includeDeleted',AQuery.includeDeleted);
  Result:=Get(commentId,fileId,replyId,_Q);
end;

Function TRepliesResource.List(commentId: string; fileId: string; AQuery : string = '') : TReplyList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'files/{fileId}/comments/{commentId}/replies';
  _Methodid   = 'drive.replies.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['commentId',commentId,'fileId',fileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TReplyList) as TReplyList;
end;


Function TRepliesResource.List(commentId: string; fileId: string; AQuery : TReplieslistOptions) : TReplyList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'includeDeleted',AQuery.includeDeleted);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(commentId,fileId,_Q);
end;

Function TRepliesResource.Update(commentId: string; fileId: string; replyId: string; aReply : TReply) : TReply;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'files/{fileId}/comments/{commentId}/replies/{replyId}';
  _Methodid   = 'drive.replies.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['commentId',commentId,'fileId',fileId,'replyId',replyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aReply,TReply) as TReply;
end;



{ --------------------------------------------------------------------
  TRevisionsResource
  --------------------------------------------------------------------}


Class Function TRevisionsResource.ResourceName : String;

begin
  Result:='revisions';
end;

Class Function TRevisionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdriveAPI;
end;

Procedure TRevisionsResource.Delete(fileId: string; revisionId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'files/{fileId}/revisions/{revisionId}';
  _Methodid   = 'drive.revisions.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId,'revisionId',revisionId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TRevisionsResource.Get(fileId: string; revisionId: string; AQuery : string = '') : TRevision;

Const
  _HTTPMethod = 'GET';
  _Path       = 'files/{fileId}/revisions/{revisionId}';
  _Methodid   = 'drive.revisions.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId,'revisionId',revisionId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TRevision) as TRevision;
end;


Function TRevisionsResource.Get(fileId: string; revisionId: string; AQuery : TRevisionsgetOptions) : TRevision;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'acknowledgeAbuse',AQuery.acknowledgeAbuse);
  Result:=Get(fileId,revisionId,_Q);
end;

Function TRevisionsResource.List(fileId: string) : TRevisionList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'files/{fileId}/revisions';
  _Methodid   = 'drive.revisions.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TRevisionList) as TRevisionList;
end;

Function TRevisionsResource.Update(fileId: string; revisionId: string; aRevision : TRevision) : TRevision;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'files/{fileId}/revisions/{revisionId}';
  _Methodid   = 'drive.revisions.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId,'revisionId',revisionId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aRevision,TRevision) as TRevision;
end;



{ --------------------------------------------------------------------
  TDriveAPI
  --------------------------------------------------------------------}

Class Function TDriveAPI.APIName : String;

begin
  Result:='drive';
end;

Class Function TDriveAPI.APIVersion : String;

begin
  Result:='v3';
end;

Class Function TDriveAPI.APIRevision : String;

begin
  Result:='20160513';
end;

Class Function TDriveAPI.APIID : String;

begin
  Result:='drive:v3';
end;

Class Function TDriveAPI.APITitle : String;

begin
  Result:='Drive API';
end;

Class Function TDriveAPI.APIDescription : String;

begin
  Result:='Manages files in Drive including uploading, downloading, searching, detecting changes, and updating sharing permissions.';
end;

Class Function TDriveAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TDriveAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TDriveAPI.APIIcon16 : String;

begin
  Result:='https://ssl.gstatic.com/docs/doclist/images/drive_icon_16.png';
end;

Class Function TDriveAPI.APIIcon32 : String;

begin
  Result:='https://ssl.gstatic.com/docs/doclist/images/drive_icon_32.png';
end;

Class Function TDriveAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/drive/';
end;

Class Function TDriveAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TDriveAPI.APIbasePath : string;

begin
  Result:='/drive/v3/';
end;

Class Function TDriveAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/drive/v3/';
end;

Class Function TDriveAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TDriveAPI.APIservicePath : string;

begin
  Result:='drive/v3/';
end;

Class Function TDriveAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TDriveAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,8);
  Result[0].Name:='https://www.googleapis.com/auth/drive';
  Result[0].Description:='View and manage the files in your Google Drive';
  Result[1].Name:='https://www.googleapis.com/auth/drive.appdata';
  Result[1].Description:='View and manage its own configuration data in your Google Drive';
  Result[2].Name:='https://www.googleapis.com/auth/drive.file';
  Result[2].Description:='View and manage Google Drive files and folders that you have opened or created with this app';
  Result[3].Name:='https://www.googleapis.com/auth/drive.metadata';
  Result[3].Description:='View and manage metadata of files in your Google Drive';
  Result[4].Name:='https://www.googleapis.com/auth/drive.metadata.readonly';
  Result[4].Description:='View metadata for files in your Google Drive';
  Result[5].Name:='https://www.googleapis.com/auth/drive.photos.readonly';
  Result[5].Description:='View the photos, videos and albums in your Google Photos';
  Result[6].Name:='https://www.googleapis.com/auth/drive.readonly';
  Result[6].Description:='View the files in your Google Drive';
  Result[7].Name:='https://www.googleapis.com/auth/drive.scripts';
  Result[7].Description:='Modify your Google Apps Script scripts'' behavior';
  
end;

Class Function TDriveAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TDriveAPI.RegisterAPIResources;

begin
  TAboutTypeexportFormats.RegisterObject;
  TAboutTypeimportFormats.RegisterObject;
  TAboutTypemaxImportSizes.RegisterObject;
  TAboutTypestorageQuota.RegisterObject;
  TAbout.RegisterObject;
  TChange.RegisterObject;
  TChangeList.RegisterObject;
  TChannelTypeparams.RegisterObject;
  TChannel.RegisterObject;
  TCommentTypequotedFileContent.RegisterObject;
  TComment.RegisterObject;
  TCommentList.RegisterObject;
  TFileTypeappProperties.RegisterObject;
  TFileTypecapabilities.RegisterObject;
  TFileTypecontentHintsTypethumbnail.RegisterObject;
  TFileTypecontentHints.RegisterObject;
  TFileTypeimageMediaMetadataTypelocation.RegisterObject;
  TFileTypeimageMediaMetadata.RegisterObject;
  TFileTypeproperties.RegisterObject;
  TFileTypevideoMediaMetadata.RegisterObject;
  TFile.RegisterObject;
  TFileList.RegisterObject;
  TGeneratedIds.RegisterObject;
  TPermission.RegisterObject;
  TPermissionList.RegisterObject;
  TReply.RegisterObject;
  TReplyList.RegisterObject;
  TRevision.RegisterObject;
  TRevisionList.RegisterObject;
  TStartPageToken.RegisterObject;
  TUser.RegisterObject;
end;


Function TDriveAPI.GetAboutInstance : TAboutResource;

begin
  if (FAboutInstance=Nil) then
    FAboutInstance:=CreateAboutResource;
  Result:=FAboutInstance;
end;

Function TDriveAPI.CreateAboutResource : TAboutResource;

begin
  Result:=CreateAboutResource(Self);
end;


Function TDriveAPI.CreateAboutResource(AOwner : TComponent) : TAboutResource;

begin
  Result:=TAboutResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TDriveAPI.GetChangesInstance : TChangesResource;

begin
  if (FChangesInstance=Nil) then
    FChangesInstance:=CreateChangesResource;
  Result:=FChangesInstance;
end;

Function TDriveAPI.CreateChangesResource : TChangesResource;

begin
  Result:=CreateChangesResource(Self);
end;


Function TDriveAPI.CreateChangesResource(AOwner : TComponent) : TChangesResource;

begin
  Result:=TChangesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TDriveAPI.GetChannelsInstance : TChannelsResource;

begin
  if (FChannelsInstance=Nil) then
    FChannelsInstance:=CreateChannelsResource;
  Result:=FChannelsInstance;
end;

Function TDriveAPI.CreateChannelsResource : TChannelsResource;

begin
  Result:=CreateChannelsResource(Self);
end;


Function TDriveAPI.CreateChannelsResource(AOwner : TComponent) : TChannelsResource;

begin
  Result:=TChannelsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TDriveAPI.GetCommentsInstance : TCommentsResource;

begin
  if (FCommentsInstance=Nil) then
    FCommentsInstance:=CreateCommentsResource;
  Result:=FCommentsInstance;
end;

Function TDriveAPI.CreateCommentsResource : TCommentsResource;

begin
  Result:=CreateCommentsResource(Self);
end;


Function TDriveAPI.CreateCommentsResource(AOwner : TComponent) : TCommentsResource;

begin
  Result:=TCommentsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TDriveAPI.GetFilesInstance : TFilesResource;

begin
  if (FFilesInstance=Nil) then
    FFilesInstance:=CreateFilesResource;
  Result:=FFilesInstance;
end;

Function TDriveAPI.CreateFilesResource : TFilesResource;

begin
  Result:=CreateFilesResource(Self);
end;


Function TDriveAPI.CreateFilesResource(AOwner : TComponent) : TFilesResource;

begin
  Result:=TFilesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TDriveAPI.GetPermissionsInstance : TPermissionsResource;

begin
  if (FPermissionsInstance=Nil) then
    FPermissionsInstance:=CreatePermissionsResource;
  Result:=FPermissionsInstance;
end;

Function TDriveAPI.CreatePermissionsResource : TPermissionsResource;

begin
  Result:=CreatePermissionsResource(Self);
end;


Function TDriveAPI.CreatePermissionsResource(AOwner : TComponent) : TPermissionsResource;

begin
  Result:=TPermissionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TDriveAPI.GetRepliesInstance : TRepliesResource;

begin
  if (FRepliesInstance=Nil) then
    FRepliesInstance:=CreateRepliesResource;
  Result:=FRepliesInstance;
end;

Function TDriveAPI.CreateRepliesResource : TRepliesResource;

begin
  Result:=CreateRepliesResource(Self);
end;


Function TDriveAPI.CreateRepliesResource(AOwner : TComponent) : TRepliesResource;

begin
  Result:=TRepliesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TDriveAPI.GetRevisionsInstance : TRevisionsResource;

begin
  if (FRevisionsInstance=Nil) then
    FRevisionsInstance:=CreateRevisionsResource;
  Result:=FRevisionsInstance;
end;

Function TDriveAPI.CreateRevisionsResource : TRevisionsResource;

begin
  Result:=CreateRevisionsResource(Self);
end;


Function TDriveAPI.CreateRevisionsResource(AOwner : TComponent) : TRevisionsResource;

begin
  Result:=TRevisionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TDriveAPI.RegisterAPI;
end.
