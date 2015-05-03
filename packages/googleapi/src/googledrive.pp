unit googledrive;
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
  TAbout = class;
  TAboutArray = Array of TAbout;
  TAboutadditionalRoleInfo = class;
  TAboutadditionalRoleInfoArray = Array of TAboutadditionalRoleInfo;
  TAboutadditionalRoleInforoleSets = class;
  TAboutadditionalRoleInforoleSetsArray = Array of TAboutadditionalRoleInforoleSets;
  TAboutadditionalRoleInforoleSetsadditionalRoles = class;
  TAboutadditionalRoleInforoleSetsadditionalRolesArray = Array of TAboutadditionalRoleInforoleSetsadditionalRoles;
  TAboutexportFormats = class;
  TAboutexportFormatsArray = Array of TAboutexportFormats;
  TAboutexportFormatstargets = class;
  TAboutexportFormatstargetsArray = Array of TAboutexportFormatstargets;
  TAboutfeatures = class;
  TAboutfeaturesArray = Array of TAboutfeatures;
  TAboutfolderColorPalette = class;
  TAboutfolderColorPaletteArray = Array of TAboutfolderColorPalette;
  TAboutimportFormats = class;
  TAboutimportFormatsArray = Array of TAboutimportFormats;
  TAboutimportFormatstargets = class;
  TAboutimportFormatstargetsArray = Array of TAboutimportFormatstargets;
  TAboutmaxUploadSizes = class;
  TAboutmaxUploadSizesArray = Array of TAboutmaxUploadSizes;
  TAboutquotaBytesByService = class;
  TAboutquotaBytesByServiceArray = Array of TAboutquotaBytesByService;
  TApp = class;
  TAppArray = Array of TApp;
  TAppicons = class;
  TAppiconsArray = Array of TAppicons;
  TAppprimaryFileExtensions = class;
  TAppprimaryFileExtensionsArray = Array of TAppprimaryFileExtensions;
  TAppprimaryMimeTypes = class;
  TAppprimaryMimeTypesArray = Array of TAppprimaryMimeTypes;
  TAppsecondaryFileExtensions = class;
  TAppsecondaryFileExtensionsArray = Array of TAppsecondaryFileExtensions;
  TAppsecondaryMimeTypes = class;
  TAppsecondaryMimeTypesArray = Array of TAppsecondaryMimeTypes;
  TAppList = class;
  TAppListArray = Array of TAppList;
  TAppListdefaultAppIds = class;
  TAppListdefaultAppIdsArray = Array of TAppListdefaultAppIds;
  TAppListitems = class;
  TAppListitemsArray = Array of TAppListitems;
  TChange = class;
  TChangeArray = Array of TChange;
  TChangeList = class;
  TChangeListArray = Array of TChangeList;
  TChangeListitems = class;
  TChangeListitemsArray = Array of TChangeListitems;
  TChannel = class;
  TChannelArray = Array of TChannel;
  TChannelparams = class;
  TChannelparamsArray = Array of TChannelparams;
  TChildList = class;
  TChildListArray = Array of TChildList;
  TChildListitems = class;
  TChildListitemsArray = Array of TChildListitems;
  TChildReference = class;
  TChildReferenceArray = Array of TChildReference;
  TComment = class;
  TCommentArray = Array of TComment;
  TCommentcontext = class;
  TCommentcontextArray = Array of TCommentcontext;
  TCommentreplies = class;
  TCommentrepliesArray = Array of TCommentreplies;
  TCommentList = class;
  TCommentListArray = Array of TCommentList;
  TCommentListitems = class;
  TCommentListitemsArray = Array of TCommentListitems;
  TCommentReply = class;
  TCommentReplyArray = Array of TCommentReply;
  TCommentReplyList = class;
  TCommentReplyListArray = Array of TCommentReplyList;
  TCommentReplyListitems = class;
  TCommentReplyListitemsArray = Array of TCommentReplyListitems;
  TFile = class;
  TFileArray = Array of TFile;
  TFileexportLinks = class;
  TFileexportLinksArray = Array of TFileexportLinks;
  TFileimageMediaMetadata = class;
  TFileimageMediaMetadataArray = Array of TFileimageMediaMetadata;
  TFileimageMediaMetadatalocation = class;
  TFileimageMediaMetadatalocationArray = Array of TFileimageMediaMetadatalocation;
  TFileindexableText = class;
  TFileindexableTextArray = Array of TFileindexableText;
  TFilelabels = class;
  TFilelabelsArray = Array of TFilelabels;
  TFileopenWithLinks = class;
  TFileopenWithLinksArray = Array of TFileopenWithLinks;
  TFileownerNames = class;
  TFileownerNamesArray = Array of TFileownerNames;
  TFileowners = class;
  TFileownersArray = Array of TFileowners;
  TFileparents = class;
  TFileparentsArray = Array of TFileparents;
  TFilepermissions = class;
  TFilepermissionsArray = Array of TFilepermissions;
  TFileproperties = class;
  TFilepropertiesArray = Array of TFileproperties;
  TFilethumbnail = class;
  TFilethumbnailArray = Array of TFilethumbnail;
  TFilevideoMediaMetadata = class;
  TFilevideoMediaMetadataArray = Array of TFilevideoMediaMetadata;
  TFileList = class;
  TFileListArray = Array of TFileList;
  TFileListitems = class;
  TFileListitemsArray = Array of TFileListitems;
  TParentList = class;
  TParentListArray = Array of TParentList;
  TParentListitems = class;
  TParentListitemsArray = Array of TParentListitems;
  TParentReference = class;
  TParentReferenceArray = Array of TParentReference;
  TPermission = class;
  TPermissionArray = Array of TPermission;
  TPermissionadditionalRoles = class;
  TPermissionadditionalRolesArray = Array of TPermissionadditionalRoles;
  TPermissionId = class;
  TPermissionIdArray = Array of TPermissionId;
  TPermissionList = class;
  TPermissionListArray = Array of TPermissionList;
  TPermissionListitems = class;
  TPermissionListitemsArray = Array of TPermissionListitems;
  TProperty = class;
  TPropertyArray = Array of TProperty;
  TPropertyList = class;
  TPropertyListArray = Array of TPropertyList;
  TPropertyListitems = class;
  TPropertyListitemsArray = Array of TPropertyListitems;
  TRevision = class;
  TRevisionArray = Array of TRevision;
  TRevisionexportLinks = class;
  TRevisionexportLinksArray = Array of TRevisionexportLinks;
  TRevisionList = class;
  TRevisionListArray = Array of TRevisionList;
  TRevisionListitems = class;
  TRevisionListitemsArray = Array of TRevisionListitems;
  TUser = class;
  TUserArray = Array of TUser;
  TUserpicture = class;
  TUserpictureArray = Array of TUserpicture;
  
  { --------------------------------------------------------------------
    TAbout
    --------------------------------------------------------------------}
  
  TAbout = Class(TGoogleBaseObject)
  Private
    FadditionalRoleInfo : TAboutadditionalRoleInfo;
    FdomainSharingPolicy : string;
    Fetag : string;
    FexportFormats : TAboutexportFormats;
    Ffeatures : TAboutfeatures;
    FfolderColorPalette : TAboutfolderColorPalette;
    FimportFormats : TAboutimportFormats;
    FisCurrentAppInstalled : boolean;
    Fkind : string;
    FlanguageCode : string;
    FlargestChangeId : string;
    FmaxUploadSizes : TAboutmaxUploadSizes;
    Fname : string;
    FpermissionId : string;
    FquotaBytesByService : TAboutquotaBytesByService;
    FquotaBytesTotal : string;
    FquotaBytesUsed : string;
    FquotaBytesUsedAggregate : string;
    FquotaBytesUsedInTrash : string;
    FquotaType : string;
    FremainingChangeIds : string;
    FrootFolderId : string;
    FselfLink : string;
    Fuser : TUser;
  Protected
    //Property setters
    Procedure SetadditionalRoleInfo(AIndex : Integer; AValue : TAboutadditionalRoleInfo); virtual;
    Procedure SetdomainSharingPolicy(AIndex : Integer; AValue : string); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure SetexportFormats(AIndex : Integer; AValue : TAboutexportFormats); virtual;
    Procedure Setfeatures(AIndex : Integer; AValue : TAboutfeatures); virtual;
    Procedure SetfolderColorPalette(AIndex : Integer; AValue : TAboutfolderColorPalette); virtual;
    Procedure SetimportFormats(AIndex : Integer; AValue : TAboutimportFormats); virtual;
    Procedure SetisCurrentAppInstalled(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetlanguageCode(AIndex : Integer; AValue : string); virtual;
    Procedure SetlargestChangeId(AIndex : Integer; AValue : string); virtual;
    Procedure SetmaxUploadSizes(AIndex : Integer; AValue : TAboutmaxUploadSizes); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetpermissionId(AIndex : Integer; AValue : string); virtual;
    Procedure SetquotaBytesByService(AIndex : Integer; AValue : TAboutquotaBytesByService); virtual;
    Procedure SetquotaBytesTotal(AIndex : Integer; AValue : string); virtual;
    Procedure SetquotaBytesUsed(AIndex : Integer; AValue : string); virtual;
    Procedure SetquotaBytesUsedAggregate(AIndex : Integer; AValue : string); virtual;
    Procedure SetquotaBytesUsedInTrash(AIndex : Integer; AValue : string); virtual;
    Procedure SetquotaType(AIndex : Integer; AValue : string); virtual;
    Procedure SetremainingChangeIds(AIndex : Integer; AValue : string); virtual;
    Procedure SetrootFolderId(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setuser(AIndex : Integer; AValue : TUser); virtual;
  Public
  Published
    Property additionalRoleInfo : TAboutadditionalRoleInfo Index 0 Read FadditionalRoleInfo Write SetadditionalRoleInfo;
    Property domainSharingPolicy : string Index 8 Read FdomainSharingPolicy Write SetdomainSharingPolicy;
    Property etag : string Index 16 Read Fetag Write Setetag;
    Property exportFormats : TAboutexportFormats Index 24 Read FexportFormats Write SetexportFormats;
    Property features : TAboutfeatures Index 32 Read Ffeatures Write Setfeatures;
    Property folderColorPalette : TAboutfolderColorPalette Index 40 Read FfolderColorPalette Write SetfolderColorPalette;
    Property importFormats : TAboutimportFormats Index 48 Read FimportFormats Write SetimportFormats;
    Property isCurrentAppInstalled : boolean Index 56 Read FisCurrentAppInstalled Write SetisCurrentAppInstalled;
    Property kind : string Index 64 Read Fkind Write Setkind;
    Property languageCode : string Index 72 Read FlanguageCode Write SetlanguageCode;
    Property largestChangeId : string Index 80 Read FlargestChangeId Write SetlargestChangeId;
    Property maxUploadSizes : TAboutmaxUploadSizes Index 88 Read FmaxUploadSizes Write SetmaxUploadSizes;
    Property name : string Index 96 Read Fname Write Setname;
    Property permissionId : string Index 104 Read FpermissionId Write SetpermissionId;
    Property quotaBytesByService : TAboutquotaBytesByService Index 112 Read FquotaBytesByService Write SetquotaBytesByService;
    Property quotaBytesTotal : string Index 120 Read FquotaBytesTotal Write SetquotaBytesTotal;
    Property quotaBytesUsed : string Index 128 Read FquotaBytesUsed Write SetquotaBytesUsed;
    Property quotaBytesUsedAggregate : string Index 136 Read FquotaBytesUsedAggregate Write SetquotaBytesUsedAggregate;
    Property quotaBytesUsedInTrash : string Index 144 Read FquotaBytesUsedInTrash Write SetquotaBytesUsedInTrash;
    Property quotaType : string Index 152 Read FquotaType Write SetquotaType;
    Property remainingChangeIds : string Index 160 Read FremainingChangeIds Write SetremainingChangeIds;
    Property rootFolderId : string Index 168 Read FrootFolderId Write SetrootFolderId;
    Property selfLink : string Index 176 Read FselfLink Write SetselfLink;
    Property user : TUser Index 184 Read Fuser Write Setuser;
  end;
  TAboutClass = Class of TAbout;
  
  { --------------------------------------------------------------------
    TAboutadditionalRoleInfo
    --------------------------------------------------------------------}
  
  TAboutadditionalRoleInfo = Class(TGoogleBaseObject)
  Private
    FroleSets : TAboutadditionalRoleInforoleSets;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetroleSets(AIndex : Integer; AValue : TAboutadditionalRoleInforoleSets); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property roleSets : TAboutadditionalRoleInforoleSets Index 0 Read FroleSets Write SetroleSets;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TAboutadditionalRoleInfoClass = Class of TAboutadditionalRoleInfo;
  
  { --------------------------------------------------------------------
    TAboutadditionalRoleInforoleSets
    --------------------------------------------------------------------}
  
  TAboutadditionalRoleInforoleSets = Class(TGoogleBaseObject)
  Private
    FadditionalRoles : TAboutadditionalRoleInforoleSetsadditionalRoles;
    FprimaryRole : string;
  Protected
    //Property setters
    Procedure SetadditionalRoles(AIndex : Integer; AValue : TAboutadditionalRoleInforoleSetsadditionalRoles); virtual;
    Procedure SetprimaryRole(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property additionalRoles : TAboutadditionalRoleInforoleSetsadditionalRoles Index 0 Read FadditionalRoles Write SetadditionalRoles;
    Property primaryRole : string Index 8 Read FprimaryRole Write SetprimaryRole;
  end;
  TAboutadditionalRoleInforoleSetsClass = Class of TAboutadditionalRoleInforoleSets;
  
  { --------------------------------------------------------------------
    TAboutadditionalRoleInforoleSetsadditionalRoles
    --------------------------------------------------------------------}
  
  TAboutadditionalRoleInforoleSetsadditionalRoles = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAboutadditionalRoleInforoleSetsadditionalRolesClass = Class of TAboutadditionalRoleInforoleSetsadditionalRoles;
  
  { --------------------------------------------------------------------
    TAboutexportFormats
    --------------------------------------------------------------------}
  
  TAboutexportFormats = Class(TGoogleBaseObject)
  Private
    Fsource : string;
    Ftargets : TAboutexportFormatstargets;
  Protected
    //Property setters
    Procedure Setsource(AIndex : Integer; AValue : string); virtual;
    Procedure Settargets(AIndex : Integer; AValue : TAboutexportFormatstargets); virtual;
  Public
  Published
    Property source : string Index 0 Read Fsource Write Setsource;
    Property targets : TAboutexportFormatstargets Index 8 Read Ftargets Write Settargets;
  end;
  TAboutexportFormatsClass = Class of TAboutexportFormats;
  
  { --------------------------------------------------------------------
    TAboutexportFormatstargets
    --------------------------------------------------------------------}
  
  TAboutexportFormatstargets = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAboutexportFormatstargetsClass = Class of TAboutexportFormatstargets;
  
  { --------------------------------------------------------------------
    TAboutfeatures
    --------------------------------------------------------------------}
  
  TAboutfeatures = Class(TGoogleBaseObject)
  Private
    FfeatureName : string;
    FfeatureRate : double;
  Protected
    //Property setters
    Procedure SetfeatureName(AIndex : Integer; AValue : string); virtual;
    Procedure SetfeatureRate(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property featureName : string Index 0 Read FfeatureName Write SetfeatureName;
    Property featureRate : double Index 8 Read FfeatureRate Write SetfeatureRate;
  end;
  TAboutfeaturesClass = Class of TAboutfeatures;
  
  { --------------------------------------------------------------------
    TAboutfolderColorPalette
    --------------------------------------------------------------------}
  
  TAboutfolderColorPalette = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAboutfolderColorPaletteClass = Class of TAboutfolderColorPalette;
  
  { --------------------------------------------------------------------
    TAboutimportFormats
    --------------------------------------------------------------------}
  
  TAboutimportFormats = Class(TGoogleBaseObject)
  Private
    Fsource : string;
    Ftargets : TAboutimportFormatstargets;
  Protected
    //Property setters
    Procedure Setsource(AIndex : Integer; AValue : string); virtual;
    Procedure Settargets(AIndex : Integer; AValue : TAboutimportFormatstargets); virtual;
  Public
  Published
    Property source : string Index 0 Read Fsource Write Setsource;
    Property targets : TAboutimportFormatstargets Index 8 Read Ftargets Write Settargets;
  end;
  TAboutimportFormatsClass = Class of TAboutimportFormats;
  
  { --------------------------------------------------------------------
    TAboutimportFormatstargets
    --------------------------------------------------------------------}
  
  TAboutimportFormatstargets = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAboutimportFormatstargetsClass = Class of TAboutimportFormatstargets;
  
  { --------------------------------------------------------------------
    TAboutmaxUploadSizes
    --------------------------------------------------------------------}
  
  TAboutmaxUploadSizes = Class(TGoogleBaseObject)
  Private
    Fsize : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setsize(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property size : string Index 0 Read Fsize Write Setsize;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TAboutmaxUploadSizesClass = Class of TAboutmaxUploadSizes;
  
  { --------------------------------------------------------------------
    TAboutquotaBytesByService
    --------------------------------------------------------------------}
  
  TAboutquotaBytesByService = Class(TGoogleBaseObject)
  Private
    FbytesUsed : string;
    FserviceName : string;
  Protected
    //Property setters
    Procedure SetbytesUsed(AIndex : Integer; AValue : string); virtual;
    Procedure SetserviceName(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property bytesUsed : string Index 0 Read FbytesUsed Write SetbytesUsed;
    Property serviceName : string Index 8 Read FserviceName Write SetserviceName;
  end;
  TAboutquotaBytesByServiceClass = Class of TAboutquotaBytesByService;
  
  { --------------------------------------------------------------------
    TApp
    --------------------------------------------------------------------}
  
  TApp = Class(TGoogleBaseObject)
  Private
    Fauthorized : boolean;
    FcreateInFolderTemplate : string;
    FcreateUrl : string;
    FhasDriveWideScope : boolean;
    Ficons : TAppicons;
    Fid : string;
    Finstalled : boolean;
    Fkind : string;
    FlongDescription : string;
    Fname : string;
    FobjectType : string;
    FopenUrlTemplate : string;
    FprimaryFileExtensions : TAppprimaryFileExtensions;
    FprimaryMimeTypes : TAppprimaryMimeTypes;
    FproductId : string;
    FproductUrl : string;
    FsecondaryFileExtensions : TAppsecondaryFileExtensions;
    FsecondaryMimeTypes : TAppsecondaryMimeTypes;
    FshortDescription : string;
    FsupportsCreate : boolean;
    FsupportsImport : boolean;
    FsupportsMultiOpen : boolean;
    FsupportsOfflineCreate : boolean;
    FuseByDefault : boolean;
  Protected
    //Property setters
    Procedure Setauthorized(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetcreateInFolderTemplate(AIndex : Integer; AValue : string); virtual;
    Procedure SetcreateUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SethasDriveWideScope(AIndex : Integer; AValue : boolean); virtual;
    Procedure Seticons(AIndex : Integer; AValue : TAppicons); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setinstalled(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetlongDescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetobjectType(AIndex : Integer; AValue : string); virtual;
    Procedure SetopenUrlTemplate(AIndex : Integer; AValue : string); virtual;
    Procedure SetprimaryFileExtensions(AIndex : Integer; AValue : TAppprimaryFileExtensions); virtual;
    Procedure SetprimaryMimeTypes(AIndex : Integer; AValue : TAppprimaryMimeTypes); virtual;
    Procedure SetproductId(AIndex : Integer; AValue : string); virtual;
    Procedure SetproductUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SetsecondaryFileExtensions(AIndex : Integer; AValue : TAppsecondaryFileExtensions); virtual;
    Procedure SetsecondaryMimeTypes(AIndex : Integer; AValue : TAppsecondaryMimeTypes); virtual;
    Procedure SetshortDescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetsupportsCreate(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsupportsImport(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsupportsMultiOpen(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsupportsOfflineCreate(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetuseByDefault(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property authorized : boolean Index 0 Read Fauthorized Write Setauthorized;
    Property createInFolderTemplate : string Index 8 Read FcreateInFolderTemplate Write SetcreateInFolderTemplate;
    Property createUrl : string Index 16 Read FcreateUrl Write SetcreateUrl;
    Property hasDriveWideScope : boolean Index 24 Read FhasDriveWideScope Write SethasDriveWideScope;
    Property icons : TAppicons Index 32 Read Ficons Write Seticons;
    Property id : string Index 40 Read Fid Write Setid;
    Property installed : boolean Index 48 Read Finstalled Write Setinstalled;
    Property kind : string Index 56 Read Fkind Write Setkind;
    Property longDescription : string Index 64 Read FlongDescription Write SetlongDescription;
    Property name : string Index 72 Read Fname Write Setname;
    Property objectType : string Index 80 Read FobjectType Write SetobjectType;
    Property openUrlTemplate : string Index 88 Read FopenUrlTemplate Write SetopenUrlTemplate;
    Property primaryFileExtensions : TAppprimaryFileExtensions Index 96 Read FprimaryFileExtensions Write SetprimaryFileExtensions;
    Property primaryMimeTypes : TAppprimaryMimeTypes Index 104 Read FprimaryMimeTypes Write SetprimaryMimeTypes;
    Property productId : string Index 112 Read FproductId Write SetproductId;
    Property productUrl : string Index 120 Read FproductUrl Write SetproductUrl;
    Property secondaryFileExtensions : TAppsecondaryFileExtensions Index 128 Read FsecondaryFileExtensions Write SetsecondaryFileExtensions;
    Property secondaryMimeTypes : TAppsecondaryMimeTypes Index 136 Read FsecondaryMimeTypes Write SetsecondaryMimeTypes;
    Property shortDescription : string Index 144 Read FshortDescription Write SetshortDescription;
    Property supportsCreate : boolean Index 152 Read FsupportsCreate Write SetsupportsCreate;
    Property supportsImport : boolean Index 160 Read FsupportsImport Write SetsupportsImport;
    Property supportsMultiOpen : boolean Index 168 Read FsupportsMultiOpen Write SetsupportsMultiOpen;
    Property supportsOfflineCreate : boolean Index 176 Read FsupportsOfflineCreate Write SetsupportsOfflineCreate;
    Property useByDefault : boolean Index 184 Read FuseByDefault Write SetuseByDefault;
  end;
  TAppClass = Class of TApp;
  
  { --------------------------------------------------------------------
    TAppicons
    --------------------------------------------------------------------}
  
  TAppicons = Class(TGoogleBaseObject)
  Private
    Fcategory : string;
    FiconUrl : string;
    Fsize : integer;
  Protected
    //Property setters
    Procedure Setcategory(AIndex : Integer; AValue : string); virtual;
    Procedure SeticonUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Setsize(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property category : string Index 0 Read Fcategory Write Setcategory;
    Property iconUrl : string Index 8 Read FiconUrl Write SeticonUrl;
    Property size : integer Index 16 Read Fsize Write Setsize;
  end;
  TAppiconsClass = Class of TAppicons;
  
  { --------------------------------------------------------------------
    TAppprimaryFileExtensions
    --------------------------------------------------------------------}
  
  TAppprimaryFileExtensions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAppprimaryFileExtensionsClass = Class of TAppprimaryFileExtensions;
  
  { --------------------------------------------------------------------
    TAppprimaryMimeTypes
    --------------------------------------------------------------------}
  
  TAppprimaryMimeTypes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAppprimaryMimeTypesClass = Class of TAppprimaryMimeTypes;
  
  { --------------------------------------------------------------------
    TAppsecondaryFileExtensions
    --------------------------------------------------------------------}
  
  TAppsecondaryFileExtensions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAppsecondaryFileExtensionsClass = Class of TAppsecondaryFileExtensions;
  
  { --------------------------------------------------------------------
    TAppsecondaryMimeTypes
    --------------------------------------------------------------------}
  
  TAppsecondaryMimeTypes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAppsecondaryMimeTypesClass = Class of TAppsecondaryMimeTypes;
  
  { --------------------------------------------------------------------
    TAppList
    --------------------------------------------------------------------}
  
  TAppList = Class(TGoogleBaseObject)
  Private
    FdefaultAppIds : TAppListdefaultAppIds;
    Fetag : string;
    Fitems : TAppListitems;
    Fkind : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure SetdefaultAppIds(AIndex : Integer; AValue : TAppListdefaultAppIds); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TAppListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property defaultAppIds : TAppListdefaultAppIds Index 0 Read FdefaultAppIds Write SetdefaultAppIds;
    Property etag : string Index 8 Read Fetag Write Setetag;
    Property items : TAppListitems Index 16 Read Fitems Write Setitems;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TAppListClass = Class of TAppList;
  
  { --------------------------------------------------------------------
    TAppListdefaultAppIds
    --------------------------------------------------------------------}
  
  TAppListdefaultAppIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAppListdefaultAppIdsClass = Class of TAppListdefaultAppIds;
  
  { --------------------------------------------------------------------
    TAppListitems
    --------------------------------------------------------------------}
  
  TAppListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAppListitemsClass = Class of TAppListitems;
  
  { --------------------------------------------------------------------
    TChange
    --------------------------------------------------------------------}
  
  TChange = Class(TGoogleBaseObject)
  Private
    Fdeleted : boolean;
    F_file : TFile;
    FfileId : string;
    Fid : string;
    Fkind : string;
    FmodificationDate : TDatetime;
    FselfLink : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setdeleted(AIndex : Integer; AValue : boolean); virtual;
    Procedure Set_file(AIndex : Integer; AValue : TFile); virtual;
    Procedure SetfileId(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetmodificationDate(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property deleted : boolean Index 0 Read Fdeleted Write Setdeleted;
    Property _file : TFile Index 8 Read F_file Write Set_file;
    Property fileId : string Index 16 Read FfileId Write SetfileId;
    Property id : string Index 24 Read Fid Write Setid;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property modificationDate : TDatetime Index 40 Read FmodificationDate Write SetmodificationDate;
    Property selfLink : string Index 48 Read FselfLink Write SetselfLink;
  end;
  TChangeClass = Class of TChange;
  
  { --------------------------------------------------------------------
    TChangeList
    --------------------------------------------------------------------}
  
  TChangeList = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TChangeListitems;
    Fkind : string;
    FlargestChangeId : string;
    FnextLink : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TChangeListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetlargestChangeId(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TChangeListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property largestChangeId : string Index 24 Read FlargestChangeId Write SetlargestChangeId;
    Property nextLink : string Index 32 Read FnextLink Write SetnextLink;
    Property nextPageToken : string Index 40 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 48 Read FselfLink Write SetselfLink;
  end;
  TChangeListClass = Class of TChangeList;
  
  { --------------------------------------------------------------------
    TChangeListitems
    --------------------------------------------------------------------}
  
  TChangeListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TChangeListitemsClass = Class of TChangeListitems;
  
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
    TChildList
    --------------------------------------------------------------------}
  
  TChildList = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TChildListitems;
    Fkind : string;
    FnextLink : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TChildListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TChildListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextLink : string Index 24 Read FnextLink Write SetnextLink;
    Property nextPageToken : string Index 32 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 40 Read FselfLink Write SetselfLink;
  end;
  TChildListClass = Class of TChildList;
  
  { --------------------------------------------------------------------
    TChildListitems
    --------------------------------------------------------------------}
  
  TChildListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TChildListitemsClass = Class of TChildListitems;
  
  { --------------------------------------------------------------------
    TChildReference
    --------------------------------------------------------------------}
  
  TChildReference = Class(TGoogleBaseObject)
  Private
    FchildLink : string;
    Fid : string;
    Fkind : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure SetchildLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property childLink : string Index 0 Read FchildLink Write SetchildLink;
    Property id : string Index 8 Read Fid Write Setid;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property selfLink : string Index 24 Read FselfLink Write SetselfLink;
  end;
  TChildReferenceClass = Class of TChildReference;
  
  { --------------------------------------------------------------------
    TComment
    --------------------------------------------------------------------}
  
  TComment = Class(TGoogleBaseObject)
  Private
    Fanchor : string;
    Fauthor : TUser;
    FcommentId : string;
    Fcontent : string;
    Fcontext : TCommentcontext;
    FcreatedDate : TDatetime;
    Fdeleted : boolean;
    FfileId : string;
    FfileTitle : string;
    FhtmlContent : string;
    Fkind : string;
    FmodifiedDate : TDatetime;
    Freplies : TCommentreplies;
    FselfLink : string;
    Fstatus : string;
  Protected
    //Property setters
    Procedure Setanchor(AIndex : Integer; AValue : string); virtual;
    Procedure Setauthor(AIndex : Integer; AValue : TUser); virtual;
    Procedure SetcommentId(AIndex : Integer; AValue : string); virtual;
    Procedure Setcontent(AIndex : Integer; AValue : string); virtual;
    Procedure Setcontext(AIndex : Integer; AValue : TCommentcontext); virtual;
    Procedure SetcreatedDate(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setdeleted(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetfileId(AIndex : Integer; AValue : string); virtual;
    Procedure SetfileTitle(AIndex : Integer; AValue : string); virtual;
    Procedure SethtmlContent(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetmodifiedDate(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setreplies(AIndex : Integer; AValue : TCommentreplies); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property anchor : string Index 0 Read Fanchor Write Setanchor;
    Property author : TUser Index 8 Read Fauthor Write Setauthor;
    Property commentId : string Index 16 Read FcommentId Write SetcommentId;
    Property content : string Index 24 Read Fcontent Write Setcontent;
    Property context : TCommentcontext Index 32 Read Fcontext Write Setcontext;
    Property createdDate : TDatetime Index 40 Read FcreatedDate Write SetcreatedDate;
    Property deleted : boolean Index 48 Read Fdeleted Write Setdeleted;
    Property fileId : string Index 56 Read FfileId Write SetfileId;
    Property fileTitle : string Index 64 Read FfileTitle Write SetfileTitle;
    Property htmlContent : string Index 72 Read FhtmlContent Write SethtmlContent;
    Property kind : string Index 80 Read Fkind Write Setkind;
    Property modifiedDate : TDatetime Index 88 Read FmodifiedDate Write SetmodifiedDate;
    Property replies : TCommentreplies Index 96 Read Freplies Write Setreplies;
    Property selfLink : string Index 104 Read FselfLink Write SetselfLink;
    Property status : string Index 112 Read Fstatus Write Setstatus;
  end;
  TCommentClass = Class of TComment;
  
  { --------------------------------------------------------------------
    TCommentcontext
    --------------------------------------------------------------------}
  
  TCommentcontext = Class(TGoogleBaseObject)
  Private
    F_type : string;
    Fvalue : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property _type : string Index 0 Read F_type Write Set_type;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TCommentcontextClass = Class of TCommentcontext;
  
  { --------------------------------------------------------------------
    TCommentreplies
    --------------------------------------------------------------------}
  
  TCommentreplies = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCommentrepliesClass = Class of TCommentreplies;
  
  { --------------------------------------------------------------------
    TCommentList
    --------------------------------------------------------------------}
  
  TCommentList = Class(TGoogleBaseObject)
  Private
    Fitems : TCommentListitems;
    Fkind : string;
    FnextLink : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TCommentListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TCommentListitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextLink : string Index 16 Read FnextLink Write SetnextLink;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TCommentListClass = Class of TCommentList;
  
  { --------------------------------------------------------------------
    TCommentListitems
    --------------------------------------------------------------------}
  
  TCommentListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCommentListitemsClass = Class of TCommentListitems;
  
  { --------------------------------------------------------------------
    TCommentReply
    --------------------------------------------------------------------}
  
  TCommentReply = Class(TGoogleBaseObject)
  Private
    Fauthor : TUser;
    Fcontent : string;
    FcreatedDate : TDatetime;
    Fdeleted : boolean;
    FhtmlContent : string;
    Fkind : string;
    FmodifiedDate : TDatetime;
    FreplyId : string;
    Fverb : string;
  Protected
    //Property setters
    Procedure Setauthor(AIndex : Integer; AValue : TUser); virtual;
    Procedure Setcontent(AIndex : Integer; AValue : string); virtual;
    Procedure SetcreatedDate(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setdeleted(AIndex : Integer; AValue : boolean); virtual;
    Procedure SethtmlContent(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetmodifiedDate(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetreplyId(AIndex : Integer; AValue : string); virtual;
    Procedure Setverb(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property author : TUser Index 0 Read Fauthor Write Setauthor;
    Property content : string Index 8 Read Fcontent Write Setcontent;
    Property createdDate : TDatetime Index 16 Read FcreatedDate Write SetcreatedDate;
    Property deleted : boolean Index 24 Read Fdeleted Write Setdeleted;
    Property htmlContent : string Index 32 Read FhtmlContent Write SethtmlContent;
    Property kind : string Index 40 Read Fkind Write Setkind;
    Property modifiedDate : TDatetime Index 48 Read FmodifiedDate Write SetmodifiedDate;
    Property replyId : string Index 56 Read FreplyId Write SetreplyId;
    Property verb : string Index 64 Read Fverb Write Setverb;
  end;
  TCommentReplyClass = Class of TCommentReply;
  
  { --------------------------------------------------------------------
    TCommentReplyList
    --------------------------------------------------------------------}
  
  TCommentReplyList = Class(TGoogleBaseObject)
  Private
    Fitems : TCommentReplyListitems;
    Fkind : string;
    FnextLink : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TCommentReplyListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TCommentReplyListitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextLink : string Index 16 Read FnextLink Write SetnextLink;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TCommentReplyListClass = Class of TCommentReplyList;
  
  { --------------------------------------------------------------------
    TCommentReplyListitems
    --------------------------------------------------------------------}
  
  TCommentReplyListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCommentReplyListitemsClass = Class of TCommentReplyListitems;
  
  { --------------------------------------------------------------------
    TFile
    --------------------------------------------------------------------}
  
  TFile = Class(TGoogleBaseObject)
  Private
    FalternateLink : string;
    FappDataContents : boolean;
    Fcopyable : boolean;
    FcreatedDate : TDatetime;
    FdefaultOpenWithLink : string;
    Fdescription : string;
    FdownloadUrl : string;
    Feditable : boolean;
    FembedLink : string;
    Fetag : string;
    FexplicitlyTrashed : boolean;
    FexportLinks : TFileexportLinks;
    FfileExtension : string;
    FfileSize : string;
    FfolderColorRgb : string;
    FheadRevisionId : string;
    FiconLink : string;
    Fid : string;
    FimageMediaMetadata : TFileimageMediaMetadata;
    FindexableText : TFileindexableText;
    Fkind : string;
    Flabels : TFilelabels;
    FlastModifyingUser : TUser;
    FlastModifyingUserName : string;
    FlastViewedByMeDate : TDatetime;
    FmarkedViewedByMeDate : TDatetime;
    Fmd5Checksum : string;
    FmimeType : string;
    FmodifiedByMeDate : TDatetime;
    FmodifiedDate : TDatetime;
    FopenWithLinks : TFileopenWithLinks;
    ForiginalFilename : string;
    FownerNames : TFileownerNames;
    Fowners : TFileowners;
    Fparents : TFileparents;
    Fpermissions : TFilepermissions;
    Fproperties : TFileproperties;
    FquotaBytesUsed : string;
    FselfLink : string;
    Fshared : boolean;
    FsharedWithMeDate : TDatetime;
    FsharingUser : TUser;
    Fthumbnail : TFilethumbnail;
    FthumbnailLink : string;
    Ftitle : string;
    FuserPermission : TPermission;
    Fversion : string;
    FvideoMediaMetadata : TFilevideoMediaMetadata;
    FwebContentLink : string;
    FwebViewLink : string;
    FwritersCanShare : boolean;
  Protected
    //Property setters
    Procedure SetalternateLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetappDataContents(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setcopyable(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetcreatedDate(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetdefaultOpenWithLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetdownloadUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Seteditable(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetembedLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure SetexplicitlyTrashed(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetexportLinks(AIndex : Integer; AValue : TFileexportLinks); virtual;
    Procedure SetfileExtension(AIndex : Integer; AValue : string); virtual;
    Procedure SetfileSize(AIndex : Integer; AValue : string); virtual;
    Procedure SetfolderColorRgb(AIndex : Integer; AValue : string); virtual;
    Procedure SetheadRevisionId(AIndex : Integer; AValue : string); virtual;
    Procedure SeticonLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetimageMediaMetadata(AIndex : Integer; AValue : TFileimageMediaMetadata); virtual;
    Procedure SetindexableText(AIndex : Integer; AValue : TFileindexableText); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlabels(AIndex : Integer; AValue : TFilelabels); virtual;
    Procedure SetlastModifyingUser(AIndex : Integer; AValue : TUser); virtual;
    Procedure SetlastModifyingUserName(AIndex : Integer; AValue : string); virtual;
    Procedure SetlastViewedByMeDate(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetmarkedViewedByMeDate(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setmd5Checksum(AIndex : Integer; AValue : string); virtual;
    Procedure SetmimeType(AIndex : Integer; AValue : string); virtual;
    Procedure SetmodifiedByMeDate(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetmodifiedDate(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetopenWithLinks(AIndex : Integer; AValue : TFileopenWithLinks); virtual;
    Procedure SetoriginalFilename(AIndex : Integer; AValue : string); virtual;
    Procedure SetownerNames(AIndex : Integer; AValue : TFileownerNames); virtual;
    Procedure Setowners(AIndex : Integer; AValue : TFileowners); virtual;
    Procedure Setparents(AIndex : Integer; AValue : TFileparents); virtual;
    Procedure Setpermissions(AIndex : Integer; AValue : TFilepermissions); virtual;
    Procedure Setproperties(AIndex : Integer; AValue : TFileproperties); virtual;
    Procedure SetquotaBytesUsed(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setshared(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsharedWithMeDate(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetsharingUser(AIndex : Integer; AValue : TUser); virtual;
    Procedure Setthumbnail(AIndex : Integer; AValue : TFilethumbnail); virtual;
    Procedure SetthumbnailLink(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure SetuserPermission(AIndex : Integer; AValue : TPermission); virtual;
    Procedure Setversion(AIndex : Integer; AValue : string); virtual;
    Procedure SetvideoMediaMetadata(AIndex : Integer; AValue : TFilevideoMediaMetadata); virtual;
    Procedure SetwebContentLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetwebViewLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetwritersCanShare(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property alternateLink : string Index 0 Read FalternateLink Write SetalternateLink;
    Property appDataContents : boolean Index 8 Read FappDataContents Write SetappDataContents;
    Property copyable : boolean Index 16 Read Fcopyable Write Setcopyable;
    Property createdDate : TDatetime Index 24 Read FcreatedDate Write SetcreatedDate;
    Property defaultOpenWithLink : string Index 32 Read FdefaultOpenWithLink Write SetdefaultOpenWithLink;
    Property description : string Index 40 Read Fdescription Write Setdescription;
    Property downloadUrl : string Index 48 Read FdownloadUrl Write SetdownloadUrl;
    Property editable : boolean Index 56 Read Feditable Write Seteditable;
    Property embedLink : string Index 64 Read FembedLink Write SetembedLink;
    Property etag : string Index 72 Read Fetag Write Setetag;
    Property explicitlyTrashed : boolean Index 80 Read FexplicitlyTrashed Write SetexplicitlyTrashed;
    Property exportLinks : TFileexportLinks Index 88 Read FexportLinks Write SetexportLinks;
    Property fileExtension : string Index 96 Read FfileExtension Write SetfileExtension;
    Property fileSize : string Index 104 Read FfileSize Write SetfileSize;
    Property folderColorRgb : string Index 112 Read FfolderColorRgb Write SetfolderColorRgb;
    Property headRevisionId : string Index 120 Read FheadRevisionId Write SetheadRevisionId;
    Property iconLink : string Index 128 Read FiconLink Write SeticonLink;
    Property id : string Index 136 Read Fid Write Setid;
    Property imageMediaMetadata : TFileimageMediaMetadata Index 144 Read FimageMediaMetadata Write SetimageMediaMetadata;
    Property indexableText : TFileindexableText Index 152 Read FindexableText Write SetindexableText;
    Property kind : string Index 160 Read Fkind Write Setkind;
    Property labels : TFilelabels Index 168 Read Flabels Write Setlabels;
    Property lastModifyingUser : TUser Index 176 Read FlastModifyingUser Write SetlastModifyingUser;
    Property lastModifyingUserName : string Index 184 Read FlastModifyingUserName Write SetlastModifyingUserName;
    Property lastViewedByMeDate : TDatetime Index 192 Read FlastViewedByMeDate Write SetlastViewedByMeDate;
    Property markedViewedByMeDate : TDatetime Index 200 Read FmarkedViewedByMeDate Write SetmarkedViewedByMeDate;
    Property md5Checksum : string Index 208 Read Fmd5Checksum Write Setmd5Checksum;
    Property mimeType : string Index 216 Read FmimeType Write SetmimeType;
    Property modifiedByMeDate : TDatetime Index 224 Read FmodifiedByMeDate Write SetmodifiedByMeDate;
    Property modifiedDate : TDatetime Index 232 Read FmodifiedDate Write SetmodifiedDate;
    Property openWithLinks : TFileopenWithLinks Index 240 Read FopenWithLinks Write SetopenWithLinks;
    Property originalFilename : string Index 248 Read ForiginalFilename Write SetoriginalFilename;
    Property ownerNames : TFileownerNames Index 256 Read FownerNames Write SetownerNames;
    Property owners : TFileowners Index 264 Read Fowners Write Setowners;
    Property parents : TFileparents Index 272 Read Fparents Write Setparents;
    Property permissions : TFilepermissions Index 280 Read Fpermissions Write Setpermissions;
    Property properties : TFileproperties Index 288 Read Fproperties Write Setproperties;
    Property quotaBytesUsed : string Index 296 Read FquotaBytesUsed Write SetquotaBytesUsed;
    Property selfLink : string Index 304 Read FselfLink Write SetselfLink;
    Property shared : boolean Index 312 Read Fshared Write Setshared;
    Property sharedWithMeDate : TDatetime Index 320 Read FsharedWithMeDate Write SetsharedWithMeDate;
    Property sharingUser : TUser Index 328 Read FsharingUser Write SetsharingUser;
    Property thumbnail : TFilethumbnail Index 336 Read Fthumbnail Write Setthumbnail;
    Property thumbnailLink : string Index 344 Read FthumbnailLink Write SetthumbnailLink;
    Property title : string Index 352 Read Ftitle Write Settitle;
    Property userPermission : TPermission Index 360 Read FuserPermission Write SetuserPermission;
    Property version : string Index 368 Read Fversion Write Setversion;
    Property videoMediaMetadata : TFilevideoMediaMetadata Index 376 Read FvideoMediaMetadata Write SetvideoMediaMetadata;
    Property webContentLink : string Index 384 Read FwebContentLink Write SetwebContentLink;
    Property webViewLink : string Index 392 Read FwebViewLink Write SetwebViewLink;
    Property writersCanShare : boolean Index 400 Read FwritersCanShare Write SetwritersCanShare;
  end;
  TFileClass = Class of TFile;
  
  { --------------------------------------------------------------------
    TFileexportLinks
    --------------------------------------------------------------------}
  
  TFileexportLinks = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TFileexportLinksClass = Class of TFileexportLinks;
  
  { --------------------------------------------------------------------
    TFileimageMediaMetadata
    --------------------------------------------------------------------}
  
  TFileimageMediaMetadata = Class(TGoogleBaseObject)
  Private
    Faperture : integer;
    FcameraMake : string;
    FcameraModel : string;
    FcolorSpace : string;
    Fdate : string;
    FexposureBias : integer;
    FexposureMode : string;
    FexposureTime : integer;
    FflashUsed : boolean;
    FfocalLength : integer;
    Fheight : integer;
    FisoSpeed : integer;
    Flens : string;
    Flocation : TFileimageMediaMetadatalocation;
    FmaxApertureValue : integer;
    FmeteringMode : string;
    Frotation : integer;
    Fsensor : string;
    FsubjectDistance : integer;
    FwhiteBalance : string;
    Fwidth : integer;
  Protected
    //Property setters
    Procedure Setaperture(AIndex : Integer; AValue : integer); virtual;
    Procedure SetcameraMake(AIndex : Integer; AValue : string); virtual;
    Procedure SetcameraModel(AIndex : Integer; AValue : string); virtual;
    Procedure SetcolorSpace(AIndex : Integer; AValue : string); virtual;
    Procedure Setdate(AIndex : Integer; AValue : string); virtual;
    Procedure SetexposureBias(AIndex : Integer; AValue : integer); virtual;
    Procedure SetexposureMode(AIndex : Integer; AValue : string); virtual;
    Procedure SetexposureTime(AIndex : Integer; AValue : integer); virtual;
    Procedure SetflashUsed(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetfocalLength(AIndex : Integer; AValue : integer); virtual;
    Procedure Setheight(AIndex : Integer; AValue : integer); virtual;
    Procedure SetisoSpeed(AIndex : Integer; AValue : integer); virtual;
    Procedure Setlens(AIndex : Integer; AValue : string); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : TFileimageMediaMetadatalocation); virtual;
    Procedure SetmaxApertureValue(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmeteringMode(AIndex : Integer; AValue : string); virtual;
    Procedure Setrotation(AIndex : Integer; AValue : integer); virtual;
    Procedure Setsensor(AIndex : Integer; AValue : string); virtual;
    Procedure SetsubjectDistance(AIndex : Integer; AValue : integer); virtual;
    Procedure SetwhiteBalance(AIndex : Integer; AValue : string); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property aperture : integer Index 0 Read Faperture Write Setaperture;
    Property cameraMake : string Index 8 Read FcameraMake Write SetcameraMake;
    Property cameraModel : string Index 16 Read FcameraModel Write SetcameraModel;
    Property colorSpace : string Index 24 Read FcolorSpace Write SetcolorSpace;
    Property date : string Index 32 Read Fdate Write Setdate;
    Property exposureBias : integer Index 40 Read FexposureBias Write SetexposureBias;
    Property exposureMode : string Index 48 Read FexposureMode Write SetexposureMode;
    Property exposureTime : integer Index 56 Read FexposureTime Write SetexposureTime;
    Property flashUsed : boolean Index 64 Read FflashUsed Write SetflashUsed;
    Property focalLength : integer Index 72 Read FfocalLength Write SetfocalLength;
    Property height : integer Index 80 Read Fheight Write Setheight;
    Property isoSpeed : integer Index 88 Read FisoSpeed Write SetisoSpeed;
    Property lens : string Index 96 Read Flens Write Setlens;
    Property location : TFileimageMediaMetadatalocation Index 104 Read Flocation Write Setlocation;
    Property maxApertureValue : integer Index 112 Read FmaxApertureValue Write SetmaxApertureValue;
    Property meteringMode : string Index 120 Read FmeteringMode Write SetmeteringMode;
    Property rotation : integer Index 128 Read Frotation Write Setrotation;
    Property sensor : string Index 136 Read Fsensor Write Setsensor;
    Property subjectDistance : integer Index 144 Read FsubjectDistance Write SetsubjectDistance;
    Property whiteBalance : string Index 152 Read FwhiteBalance Write SetwhiteBalance;
    Property width : integer Index 160 Read Fwidth Write Setwidth;
  end;
  TFileimageMediaMetadataClass = Class of TFileimageMediaMetadata;
  
  { --------------------------------------------------------------------
    TFileimageMediaMetadatalocation
    --------------------------------------------------------------------}
  
  TFileimageMediaMetadatalocation = Class(TGoogleBaseObject)
  Private
    Faltitude : double;
    Flatitude : double;
    Flongitude : double;
  Protected
    //Property setters
    Procedure Setaltitude(AIndex : Integer; AValue : double); virtual;
    Procedure Setlatitude(AIndex : Integer; AValue : double); virtual;
    Procedure Setlongitude(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property altitude : double Index 0 Read Faltitude Write Setaltitude;
    Property latitude : double Index 8 Read Flatitude Write Setlatitude;
    Property longitude : double Index 16 Read Flongitude Write Setlongitude;
  end;
  TFileimageMediaMetadatalocationClass = Class of TFileimageMediaMetadatalocation;
  
  { --------------------------------------------------------------------
    TFileindexableText
    --------------------------------------------------------------------}
  
  TFileindexableText = Class(TGoogleBaseObject)
  Private
    Ftext : string;
  Protected
    //Property setters
    Procedure Settext(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property text : string Index 0 Read Ftext Write Settext;
  end;
  TFileindexableTextClass = Class of TFileindexableText;
  
  { --------------------------------------------------------------------
    TFilelabels
    --------------------------------------------------------------------}
  
  TFilelabels = Class(TGoogleBaseObject)
  Private
    Fhidden : boolean;
    Frestricted : boolean;
    Fstarred : boolean;
    Ftrashed : boolean;
    Fviewed : boolean;
  Protected
    //Property setters
    Procedure Sethidden(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setrestricted(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setstarred(AIndex : Integer; AValue : boolean); virtual;
    Procedure Settrashed(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setviewed(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property hidden : boolean Index 0 Read Fhidden Write Sethidden;
    Property restricted : boolean Index 8 Read Frestricted Write Setrestricted;
    Property starred : boolean Index 16 Read Fstarred Write Setstarred;
    Property trashed : boolean Index 24 Read Ftrashed Write Settrashed;
    Property viewed : boolean Index 32 Read Fviewed Write Setviewed;
  end;
  TFilelabelsClass = Class of TFilelabels;
  
  { --------------------------------------------------------------------
    TFileopenWithLinks
    --------------------------------------------------------------------}
  
  TFileopenWithLinks = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TFileopenWithLinksClass = Class of TFileopenWithLinks;
  
  { --------------------------------------------------------------------
    TFileownerNames
    --------------------------------------------------------------------}
  
  TFileownerNames = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFileownerNamesClass = Class of TFileownerNames;
  
  { --------------------------------------------------------------------
    TFileowners
    --------------------------------------------------------------------}
  
  TFileowners = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFileownersClass = Class of TFileowners;
  
  { --------------------------------------------------------------------
    TFileparents
    --------------------------------------------------------------------}
  
  TFileparents = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFileparentsClass = Class of TFileparents;
  
  { --------------------------------------------------------------------
    TFilepermissions
    --------------------------------------------------------------------}
  
  TFilepermissions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFilepermissionsClass = Class of TFilepermissions;
  
  { --------------------------------------------------------------------
    TFileproperties
    --------------------------------------------------------------------}
  
  TFileproperties = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFilepropertiesClass = Class of TFileproperties;
  
  { --------------------------------------------------------------------
    TFilethumbnail
    --------------------------------------------------------------------}
  
  TFilethumbnail = Class(TGoogleBaseObject)
  Private
    Fimage : string;
    FmimeType : string;
  Protected
    //Property setters
    Procedure Setimage(AIndex : Integer; AValue : string); virtual;
    Procedure SetmimeType(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property image : string Index 0 Read Fimage Write Setimage;
    Property mimeType : string Index 8 Read FmimeType Write SetmimeType;
  end;
  TFilethumbnailClass = Class of TFilethumbnail;
  
  { --------------------------------------------------------------------
    TFilevideoMediaMetadata
    --------------------------------------------------------------------}
  
  TFilevideoMediaMetadata = Class(TGoogleBaseObject)
  Private
    FdurationMillis : string;
    Fheight : integer;
    Fwidth : integer;
  Protected
    //Property setters
    Procedure SetdurationMillis(AIndex : Integer; AValue : string); virtual;
    Procedure Setheight(AIndex : Integer; AValue : integer); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property durationMillis : string Index 0 Read FdurationMillis Write SetdurationMillis;
    Property height : integer Index 8 Read Fheight Write Setheight;
    Property width : integer Index 16 Read Fwidth Write Setwidth;
  end;
  TFilevideoMediaMetadataClass = Class of TFilevideoMediaMetadata;
  
  { --------------------------------------------------------------------
    TFileList
    --------------------------------------------------------------------}
  
  TFileList = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TFileListitems;
    Fkind : string;
    FnextLink : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TFileListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TFileListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextLink : string Index 24 Read FnextLink Write SetnextLink;
    Property nextPageToken : string Index 32 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 40 Read FselfLink Write SetselfLink;
  end;
  TFileListClass = Class of TFileList;
  
  { --------------------------------------------------------------------
    TFileListitems
    --------------------------------------------------------------------}
  
  TFileListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFileListitemsClass = Class of TFileListitems;
  
  { --------------------------------------------------------------------
    TParentList
    --------------------------------------------------------------------}
  
  TParentList = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TParentListitems;
    Fkind : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TParentListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TParentListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property selfLink : string Index 24 Read FselfLink Write SetselfLink;
  end;
  TParentListClass = Class of TParentList;
  
  { --------------------------------------------------------------------
    TParentListitems
    --------------------------------------------------------------------}
  
  TParentListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TParentListitemsClass = Class of TParentListitems;
  
  { --------------------------------------------------------------------
    TParentReference
    --------------------------------------------------------------------}
  
  TParentReference = Class(TGoogleBaseObject)
  Private
    Fid : string;
    FisRoot : boolean;
    Fkind : string;
    FparentLink : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetisRoot(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetparentLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property isRoot : boolean Index 8 Read FisRoot Write SetisRoot;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property parentLink : string Index 24 Read FparentLink Write SetparentLink;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TParentReferenceClass = Class of TParentReference;
  
  { --------------------------------------------------------------------
    TPermission
    --------------------------------------------------------------------}
  
  TPermission = Class(TGoogleBaseObject)
  Private
    FadditionalRoles : TPermissionadditionalRoles;
    FauthKey : string;
    Fdomain : string;
    FemailAddress : string;
    Fetag : string;
    Fid : string;
    Fkind : string;
    Fname : string;
    FphotoLink : string;
    Frole : string;
    FselfLink : string;
    F_type : string;
    Fvalue : string;
    FwithLink : boolean;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetadditionalRoles(AIndex : Integer; AValue : TPermissionadditionalRoles); virtual;
    Procedure SetauthKey(AIndex : Integer; AValue : string); virtual;
    Procedure Setdomain(AIndex : Integer; AValue : string); virtual;
    Procedure SetemailAddress(AIndex : Integer; AValue : string); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetphotoLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setrole(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
    Procedure SetwithLink(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property additionalRoles : TPermissionadditionalRoles Index 0 Read FadditionalRoles Write SetadditionalRoles;
    Property authKey : string Index 8 Read FauthKey Write SetauthKey;
    Property domain : string Index 16 Read Fdomain Write Setdomain;
    Property emailAddress : string Index 24 Read FemailAddress Write SetemailAddress;
    Property etag : string Index 32 Read Fetag Write Setetag;
    Property id : string Index 40 Read Fid Write Setid;
    Property kind : string Index 48 Read Fkind Write Setkind;
    Property name : string Index 56 Read Fname Write Setname;
    Property photoLink : string Index 64 Read FphotoLink Write SetphotoLink;
    Property role : string Index 72 Read Frole Write Setrole;
    Property selfLink : string Index 80 Read FselfLink Write SetselfLink;
    Property _type : string Index 88 Read F_type Write Set_type;
    Property value : string Index 96 Read Fvalue Write Setvalue;
    Property withLink : boolean Index 104 Read FwithLink Write SetwithLink;
  end;
  TPermissionClass = Class of TPermission;
  
  { --------------------------------------------------------------------
    TPermissionadditionalRoles
    --------------------------------------------------------------------}
  
  TPermissionadditionalRoles = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPermissionadditionalRolesClass = Class of TPermissionadditionalRoles;
  
  { --------------------------------------------------------------------
    TPermissionId
    --------------------------------------------------------------------}
  
  TPermissionId = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TPermissionIdClass = Class of TPermissionId;
  
  { --------------------------------------------------------------------
    TPermissionList
    --------------------------------------------------------------------}
  
  TPermissionList = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TPermissionListitems;
    Fkind : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TPermissionListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TPermissionListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property selfLink : string Index 24 Read FselfLink Write SetselfLink;
  end;
  TPermissionListClass = Class of TPermissionList;
  
  { --------------------------------------------------------------------
    TPermissionListitems
    --------------------------------------------------------------------}
  
  TPermissionListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPermissionListitemsClass = Class of TPermissionListitems;
  
  { --------------------------------------------------------------------
    TProperty
    --------------------------------------------------------------------}
  
  TProperty = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fkey : string;
    Fkind : string;
    FselfLink : string;
    Fvalue : string;
    Fvisibility : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
    Procedure Setvisibility(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property key : string Index 8 Read Fkey Write Setkey;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property selfLink : string Index 24 Read FselfLink Write SetselfLink;
    Property value : string Index 32 Read Fvalue Write Setvalue;
    Property visibility : string Index 40 Read Fvisibility Write Setvisibility;
  end;
  TPropertyClass = Class of TProperty;
  
  { --------------------------------------------------------------------
    TPropertyList
    --------------------------------------------------------------------}
  
  TPropertyList = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TPropertyListitems;
    Fkind : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TPropertyListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TPropertyListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property selfLink : string Index 24 Read FselfLink Write SetselfLink;
  end;
  TPropertyListClass = Class of TPropertyList;
  
  { --------------------------------------------------------------------
    TPropertyListitems
    --------------------------------------------------------------------}
  
  TPropertyListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPropertyListitemsClass = Class of TPropertyListitems;
  
  { --------------------------------------------------------------------
    TRevision
    --------------------------------------------------------------------}
  
  TRevision = Class(TGoogleBaseObject)
  Private
    FdownloadUrl : string;
    Fetag : string;
    FexportLinks : TRevisionexportLinks;
    FfileSize : string;
    Fid : string;
    Fkind : string;
    FlastModifyingUser : TUser;
    FlastModifyingUserName : string;
    Fmd5Checksum : string;
    FmimeType : string;
    FmodifiedDate : TDatetime;
    ForiginalFilename : string;
    Fpinned : boolean;
    FpublishAuto : boolean;
    F_published : boolean;
    FpublishedLink : string;
    FpublishedOutsideDomain : boolean;
    FselfLink : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdownloadUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure SetexportLinks(AIndex : Integer; AValue : TRevisionexportLinks); virtual;
    Procedure SetfileSize(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetlastModifyingUser(AIndex : Integer; AValue : TUser); virtual;
    Procedure SetlastModifyingUserName(AIndex : Integer; AValue : string); virtual;
    Procedure Setmd5Checksum(AIndex : Integer; AValue : string); virtual;
    Procedure SetmimeType(AIndex : Integer; AValue : string); virtual;
    Procedure SetmodifiedDate(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetoriginalFilename(AIndex : Integer; AValue : string); virtual;
    Procedure Setpinned(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetpublishAuto(AIndex : Integer; AValue : boolean); virtual;
    Procedure Set_published(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetpublishedLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetpublishedOutsideDomain(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property downloadUrl : string Index 0 Read FdownloadUrl Write SetdownloadUrl;
    Property etag : string Index 8 Read Fetag Write Setetag;
    Property exportLinks : TRevisionexportLinks Index 16 Read FexportLinks Write SetexportLinks;
    Property fileSize : string Index 24 Read FfileSize Write SetfileSize;
    Property id : string Index 32 Read Fid Write Setid;
    Property kind : string Index 40 Read Fkind Write Setkind;
    Property lastModifyingUser : TUser Index 48 Read FlastModifyingUser Write SetlastModifyingUser;
    Property lastModifyingUserName : string Index 56 Read FlastModifyingUserName Write SetlastModifyingUserName;
    Property md5Checksum : string Index 64 Read Fmd5Checksum Write Setmd5Checksum;
    Property mimeType : string Index 72 Read FmimeType Write SetmimeType;
    Property modifiedDate : TDatetime Index 80 Read FmodifiedDate Write SetmodifiedDate;
    Property originalFilename : string Index 88 Read ForiginalFilename Write SetoriginalFilename;
    Property pinned : boolean Index 96 Read Fpinned Write Setpinned;
    Property publishAuto : boolean Index 104 Read FpublishAuto Write SetpublishAuto;
    Property _published : boolean Index 112 Read F_published Write Set_published;
    Property publishedLink : string Index 120 Read FpublishedLink Write SetpublishedLink;
    Property publishedOutsideDomain : boolean Index 128 Read FpublishedOutsideDomain Write SetpublishedOutsideDomain;
    Property selfLink : string Index 136 Read FselfLink Write SetselfLink;
  end;
  TRevisionClass = Class of TRevision;
  
  { --------------------------------------------------------------------
    TRevisionexportLinks
    --------------------------------------------------------------------}
  
  TRevisionexportLinks = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TRevisionexportLinksClass = Class of TRevisionexportLinks;
  
  { --------------------------------------------------------------------
    TRevisionList
    --------------------------------------------------------------------}
  
  TRevisionList = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TRevisionListitems;
    Fkind : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TRevisionListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TRevisionListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property selfLink : string Index 24 Read FselfLink Write SetselfLink;
  end;
  TRevisionListClass = Class of TRevisionList;
  
  { --------------------------------------------------------------------
    TRevisionListitems
    --------------------------------------------------------------------}
  
  TRevisionListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRevisionListitemsClass = Class of TRevisionListitems;
  
  { --------------------------------------------------------------------
    TUser
    --------------------------------------------------------------------}
  
  TUser = Class(TGoogleBaseObject)
  Private
    FdisplayName : string;
    FemailAddress : string;
    FisAuthenticatedUser : boolean;
    Fkind : string;
    FpermissionId : string;
    Fpicture : TUserpicture;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure SetemailAddress(AIndex : Integer; AValue : string); virtual;
    Procedure SetisAuthenticatedUser(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetpermissionId(AIndex : Integer; AValue : string); virtual;
    Procedure Setpicture(AIndex : Integer; AValue : TUserpicture); virtual;
  Public
  Published
    Property displayName : string Index 0 Read FdisplayName Write SetdisplayName;
    Property emailAddress : string Index 8 Read FemailAddress Write SetemailAddress;
    Property isAuthenticatedUser : boolean Index 16 Read FisAuthenticatedUser Write SetisAuthenticatedUser;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property permissionId : string Index 32 Read FpermissionId Write SetpermissionId;
    Property picture : TUserpicture Index 40 Read Fpicture Write Setpicture;
  end;
  TUserClass = Class of TUser;
  
  { --------------------------------------------------------------------
    TUserpicture
    --------------------------------------------------------------------}
  
  TUserpicture = Class(TGoogleBaseObject)
  Private
    Furl : string;
  Protected
    //Property setters
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property url : string Index 0 Read Furl Write Seturl;
  end;
  TUserpictureClass = Class of TUserpicture;
  
  { --------------------------------------------------------------------
    TAboutResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAboutResource, method Get
  
  TAboutGetOptions = Record
    includeSubscribed : boolean;
    maxChangeIdCount : int64;
    startChangeId : int64;
  end;
  
  TAboutResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(AQuery : string  = '') : TAbout;
    Function Get(AQuery : TAboutgetOptions) : TAbout;
  end;
  
  
  { --------------------------------------------------------------------
    TAppsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAppsResource, method List
  
  TAppsListOptions = Record
    appFilterExtensions : string;
    appFilterMimeTypes : string;
    languageCode : string;
  end;
  
  TAppsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(appId: string) : TApp;
    Function List(AQuery : string  = '') : TAppList;
    Function List(AQuery : TAppslistOptions) : TAppList;
  end;
  
  
  { --------------------------------------------------------------------
    TChangesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TChangesResource, method List
  
  TChangesListOptions = Record
    includeDeleted : boolean;
    includeSubscribed : boolean;
    maxResults : integer;
    pageToken : string;
    startChangeId : int64;
  end;
  
  
  //Optional query Options for TChangesResource, method Watch
  
  TChangesWatchOptions = Record
    includeDeleted : boolean;
    includeSubscribed : boolean;
    maxResults : integer;
    pageToken : string;
    startChangeId : int64;
  end;
  
  TChangesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(changeId: string) : TChange;
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
    TChildrenResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TChildrenResource, method List
  
  TChildrenListOptions = Record
    maxResults : integer;
    pageToken : string;
    q : string;
  end;
  
  TChildrenResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(childId: string; folderId: string);
    Function Get(childId: string; folderId: string) : TChildReference;
    Function Insert(folderId: string; aChildReference : TChildReference) : TChildReference;
    Function List(folderId: string; AQuery : string  = '') : TChildList;
    Function List(folderId: string; AQuery : TChildrenlistOptions) : TChildList;
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
    maxResults : integer;
    pageToken : string;
    updatedMin : string;
  end;
  
  TCommentsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(commentId: string; fileId: string);
    Function Get(commentId: string; fileId: string; AQuery : string  = '') : TComment;
    Function Get(commentId: string; fileId: string; AQuery : TCommentsgetOptions) : TComment;
    Function Insert(fileId: string; aComment : TComment) : TComment;
    Function List(fileId: string; AQuery : string  = '') : TCommentList;
    Function List(fileId: string; AQuery : TCommentslistOptions) : TCommentList;
    Function Patch(commentId: string; fileId: string; aComment : TComment) : TComment;
    Function Update(commentId: string; fileId: string; aComment : TComment) : TComment;
  end;
  
  
  { --------------------------------------------------------------------
    TFilesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TFilesResource, method Copy
  
  TFilesCopyOptions = Record
    convert : boolean;
    ocr : boolean;
    ocrLanguage : string;
    pinned : boolean;
    timedTextLanguage : string;
    timedTextTrackName : string;
    visibility : string;
  end;
  
  
  //Optional query Options for TFilesResource, method Get
  
  TFilesGetOptions = Record
    acknowledgeAbuse : boolean;
    projection : string;
    revisionId : string;
    updateViewedDate : boolean;
  end;
  
  
  //Optional query Options for TFilesResource, method Insert
  
  TFilesInsertOptions = Record
    convert : boolean;
    ocr : boolean;
    ocrLanguage : string;
    pinned : boolean;
    timedTextLanguage : string;
    timedTextTrackName : string;
    useContentAsIndexableText : boolean;
    visibility : string;
  end;
  
  
  //Optional query Options for TFilesResource, method List
  
  TFilesListOptions = Record
    corpus : string;
    maxResults : integer;
    pageToken : string;
    projection : string;
    q : string;
  end;
  
  
  //Optional query Options for TFilesResource, method Patch
  
  TFilesPatchOptions = Record
    addParents : string;
    convert : boolean;
    newRevision : boolean;
    ocr : boolean;
    ocrLanguage : string;
    pinned : boolean;
    removeParents : string;
    setModifiedDate : boolean;
    timedTextLanguage : string;
    timedTextTrackName : string;
    updateViewedDate : boolean;
    useContentAsIndexableText : boolean;
  end;
  
  
  //Optional query Options for TFilesResource, method Update
  
  TFilesUpdateOptions = Record
    addParents : string;
    convert : boolean;
    newRevision : boolean;
    ocr : boolean;
    ocrLanguage : string;
    pinned : boolean;
    removeParents : string;
    setModifiedDate : boolean;
    timedTextLanguage : string;
    timedTextTrackName : string;
    updateViewedDate : boolean;
    useContentAsIndexableText : boolean;
  end;
  
  
  //Optional query Options for TFilesResource, method Watch
  
  TFilesWatchOptions = Record
    acknowledgeAbuse : boolean;
    projection : string;
    revisionId : string;
    updateViewedDate : boolean;
  end;
  
  TFilesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Copy(fileId: string; aFile : TFile; AQuery : string  = '') : TFile;
    Function Copy(fileId: string; aFile : TFile; AQuery : TFilescopyOptions) : TFile;
    Procedure Delete(fileId: string);
    Procedure EmptyTrash;
    Function Get(fileId: string; AQuery : string  = '') : TFile;
    Function Get(fileId: string; AQuery : TFilesgetOptions) : TFile;
    Function Insert(aFile : TFile; AQuery : string  = '') : TFile;
    Function Insert(aFile : TFile; AQuery : TFilesinsertOptions) : TFile;
    Function List(AQuery : string  = '') : TFileList;
    Function List(AQuery : TFileslistOptions) : TFileList;
    Function Patch(fileId: string; aFile : TFile; AQuery : string  = '') : TFile;
    Function Patch(fileId: string; aFile : TFile; AQuery : TFilespatchOptions) : TFile;
    Function Touch(fileId: string) : TFile;
    Function Trash(fileId: string) : TFile;
    Function Untrash(fileId: string) : TFile;
    Function Update(fileId: string; aFile : TFile; AQuery : string  = '') : TFile;
    Function Update(fileId: string; aFile : TFile; AQuery : TFilesupdateOptions) : TFile;
    Function Watch(fileId: string; aChannel : TChannel; AQuery : string  = '') : TChannel;
    Function Watch(fileId: string; aChannel : TChannel; AQuery : TFileswatchOptions) : TChannel;
  end;
  
  
  { --------------------------------------------------------------------
    TParentsResource
    --------------------------------------------------------------------}
  
  TParentsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(fileId: string; parentId: string);
    Function Get(fileId: string; parentId: string) : TParentReference;
    Function Insert(fileId: string; aParentReference : TParentReference) : TParentReference;
    Function List(fileId: string) : TParentList;
  end;
  
  
  { --------------------------------------------------------------------
    TPermissionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TPermissionsResource, method Insert
  
  TPermissionsInsertOptions = Record
    emailMessage : string;
    sendNotificationEmails : boolean;
  end;
  
  
  //Optional query Options for TPermissionsResource, method Patch
  
  TPermissionsPatchOptions = Record
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
    Procedure Delete(fileId: string; permissionId: string);
    Function Get(fileId: string; permissionId: string) : TPermission;
    Function GetIdForEmail(email: string) : TPermissionId;
    Function Insert(fileId: string; aPermission : TPermission; AQuery : string  = '') : TPermission;
    Function Insert(fileId: string; aPermission : TPermission; AQuery : TPermissionsinsertOptions) : TPermission;
    Function List(fileId: string) : TPermissionList;
    Function Patch(fileId: string; permissionId: string; aPermission : TPermission; AQuery : string  = '') : TPermission;
    Function Patch(fileId: string; permissionId: string; aPermission : TPermission; AQuery : TPermissionspatchOptions) : TPermission;
    Function Update(fileId: string; permissionId: string; aPermission : TPermission; AQuery : string  = '') : TPermission;
    Function Update(fileId: string; permissionId: string; aPermission : TPermission; AQuery : TPermissionsupdateOptions) : TPermission;
  end;
  
  
  { --------------------------------------------------------------------
    TPropertiesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TPropertiesResource, method Delete
  
  TPropertiesDeleteOptions = Record
    visibility : string;
  end;
  
  
  //Optional query Options for TPropertiesResource, method Get
  
  TPropertiesGetOptions = Record
    visibility : string;
  end;
  
  
  //Optional query Options for TPropertiesResource, method Patch
  
  TPropertiesPatchOptions = Record
    visibility : string;
  end;
  
  
  //Optional query Options for TPropertiesResource, method Update
  
  TPropertiesUpdateOptions = Record
    visibility : string;
  end;
  
  TPropertiesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(fileId: string; propertyKey: string; AQuery : string  = '');
    Procedure Delete(fileId: string; propertyKey: string; AQuery : TPropertiesdeleteOptions);
    Function Get(fileId: string; propertyKey: string; AQuery : string  = '') : TProperty;
    Function Get(fileId: string; propertyKey: string; AQuery : TPropertiesgetOptions) : TProperty;
    Function Insert(fileId: string; aProperty : TProperty) : TProperty;
    Function List(fileId: string) : TPropertyList;
    Function Patch(fileId: string; propertyKey: string; aProperty : TProperty; AQuery : string  = '') : TProperty;
    Function Patch(fileId: string; propertyKey: string; aProperty : TProperty; AQuery : TPropertiespatchOptions) : TProperty;
    Function Update(fileId: string; propertyKey: string; aProperty : TProperty; AQuery : string  = '') : TProperty;
    Function Update(fileId: string; propertyKey: string; aProperty : TProperty; AQuery : TPropertiesupdateOptions) : TProperty;
  end;
  
  
  { --------------------------------------------------------------------
    TRealtimeResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TRealtimeResource, method Get
  
  TRealtimeGetOptions = Record
    revision : integer;
  end;
  
  
  //Optional query Options for TRealtimeResource, method Update
  
  TRealtimeUpdateOptions = Record
    baseRevision : string;
  end;
  
  TRealtimeResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Get(fileId: string; AQuery : string  = '');
    Procedure Get(fileId: string; AQuery : TRealtimegetOptions);
    Procedure Update(fileId: string; AQuery : string  = '');
    Procedure Update(fileId: string; AQuery : TRealtimeupdateOptions);
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
    maxResults : integer;
    pageToken : string;
  end;
  
  TRepliesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(commentId: string; fileId: string; replyId: string);
    Function Get(commentId: string; fileId: string; replyId: string; AQuery : string  = '') : TCommentReply;
    Function Get(commentId: string; fileId: string; replyId: string; AQuery : TRepliesgetOptions) : TCommentReply;
    Function Insert(commentId: string; fileId: string; aCommentReply : TCommentReply) : TCommentReply;
    Function List(commentId: string; fileId: string; AQuery : string  = '') : TCommentReplyList;
    Function List(commentId: string; fileId: string; AQuery : TReplieslistOptions) : TCommentReplyList;
    Function Patch(commentId: string; fileId: string; replyId: string; aCommentReply : TCommentReply) : TCommentReply;
    Function Update(commentId: string; fileId: string; replyId: string; aCommentReply : TCommentReply) : TCommentReply;
  end;
  
  
  { --------------------------------------------------------------------
    TRevisionsResource
    --------------------------------------------------------------------}
  
  TRevisionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(fileId: string; revisionId: string);
    Function Get(fileId: string; revisionId: string) : TRevision;
    Function List(fileId: string) : TRevisionList;
    Function Patch(fileId: string; revisionId: string; aRevision : TRevision) : TRevision;
    Function Update(fileId: string; revisionId: string; aRevision : TRevision) : TRevision;
  end;
  
  
  { --------------------------------------------------------------------
    TDriveAPI
    --------------------------------------------------------------------}
  
  TDriveAPI = Class(TGoogleAPI)
  Private
    FAboutInstance : TAboutResource;
    FAppsInstance : TAppsResource;
    FChangesInstance : TChangesResource;
    FChannelsInstance : TChannelsResource;
    FChildrenInstance : TChildrenResource;
    FCommentsInstance : TCommentsResource;
    FFilesInstance : TFilesResource;
    FParentsInstance : TParentsResource;
    FPermissionsInstance : TPermissionsResource;
    FPropertiesInstance : TPropertiesResource;
    FRealtimeInstance : TRealtimeResource;
    FRepliesInstance : TRepliesResource;
    FRevisionsInstance : TRevisionsResource;
    Function GetAboutInstance : TAboutResource;virtual;
    Function GetAppsInstance : TAppsResource;virtual;
    Function GetChangesInstance : TChangesResource;virtual;
    Function GetChannelsInstance : TChannelsResource;virtual;
    Function GetChildrenInstance : TChildrenResource;virtual;
    Function GetCommentsInstance : TCommentsResource;virtual;
    Function GetFilesInstance : TFilesResource;virtual;
    Function GetParentsInstance : TParentsResource;virtual;
    Function GetPermissionsInstance : TPermissionsResource;virtual;
    Function GetPropertiesInstance : TPropertiesResource;virtual;
    Function GetRealtimeInstance : TRealtimeResource;virtual;
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
    Function CreateAppsResource(AOwner : TComponent) : TAppsResource;virtual;overload;
    Function CreateAppsResource : TAppsResource;virtual;overload;
    Function CreateChangesResource(AOwner : TComponent) : TChangesResource;virtual;overload;
    Function CreateChangesResource : TChangesResource;virtual;overload;
    Function CreateChannelsResource(AOwner : TComponent) : TChannelsResource;virtual;overload;
    Function CreateChannelsResource : TChannelsResource;virtual;overload;
    Function CreateChildrenResource(AOwner : TComponent) : TChildrenResource;virtual;overload;
    Function CreateChildrenResource : TChildrenResource;virtual;overload;
    Function CreateCommentsResource(AOwner : TComponent) : TCommentsResource;virtual;overload;
    Function CreateCommentsResource : TCommentsResource;virtual;overload;
    Function CreateFilesResource(AOwner : TComponent) : TFilesResource;virtual;overload;
    Function CreateFilesResource : TFilesResource;virtual;overload;
    Function CreateParentsResource(AOwner : TComponent) : TParentsResource;virtual;overload;
    Function CreateParentsResource : TParentsResource;virtual;overload;
    Function CreatePermissionsResource(AOwner : TComponent) : TPermissionsResource;virtual;overload;
    Function CreatePermissionsResource : TPermissionsResource;virtual;overload;
    Function CreatePropertiesResource(AOwner : TComponent) : TPropertiesResource;virtual;overload;
    Function CreatePropertiesResource : TPropertiesResource;virtual;overload;
    Function CreateRealtimeResource(AOwner : TComponent) : TRealtimeResource;virtual;overload;
    Function CreateRealtimeResource : TRealtimeResource;virtual;overload;
    Function CreateRepliesResource(AOwner : TComponent) : TRepliesResource;virtual;overload;
    Function CreateRepliesResource : TRepliesResource;virtual;overload;
    Function CreateRevisionsResource(AOwner : TComponent) : TRevisionsResource;virtual;overload;
    Function CreateRevisionsResource : TRevisionsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AboutResource : TAboutResource Read GetAboutInstance;
    Property AppsResource : TAppsResource Read GetAppsInstance;
    Property ChangesResource : TChangesResource Read GetChangesInstance;
    Property ChannelsResource : TChannelsResource Read GetChannelsInstance;
    Property ChildrenResource : TChildrenResource Read GetChildrenInstance;
    Property CommentsResource : TCommentsResource Read GetCommentsInstance;
    Property FilesResource : TFilesResource Read GetFilesInstance;
    Property ParentsResource : TParentsResource Read GetParentsInstance;
    Property PermissionsResource : TPermissionsResource Read GetPermissionsInstance;
    Property PropertiesResource : TPropertiesResource Read GetPropertiesInstance;
    Property RealtimeResource : TRealtimeResource Read GetRealtimeInstance;
    Property RepliesResource : TRepliesResource Read GetRepliesInstance;
    Property RevisionsResource : TRevisionsResource Read GetRevisionsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAbout
  --------------------------------------------------------------------}


Procedure TAbout.SetadditionalRoleInfo(AIndex : Integer; AValue : TAboutadditionalRoleInfo); 

begin
  If (FadditionalRoleInfo=AValue) then exit;
  FadditionalRoleInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.SetdomainSharingPolicy(AIndex : Integer; AValue : string); 

begin
  If (FdomainSharingPolicy=AValue) then exit;
  FdomainSharingPolicy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.SetexportFormats(AIndex : Integer; AValue : TAboutexportFormats); 

begin
  If (FexportFormats=AValue) then exit;
  FexportFormats:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.Setfeatures(AIndex : Integer; AValue : TAboutfeatures); 

begin
  If (Ffeatures=AValue) then exit;
  Ffeatures:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.SetfolderColorPalette(AIndex : Integer; AValue : TAboutfolderColorPalette); 

begin
  If (FfolderColorPalette=AValue) then exit;
  FfolderColorPalette:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.SetimportFormats(AIndex : Integer; AValue : TAboutimportFormats); 

begin
  If (FimportFormats=AValue) then exit;
  FimportFormats:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.SetisCurrentAppInstalled(AIndex : Integer; AValue : boolean); 

begin
  If (FisCurrentAppInstalled=AValue) then exit;
  FisCurrentAppInstalled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.SetlanguageCode(AIndex : Integer; AValue : string); 

begin
  If (FlanguageCode=AValue) then exit;
  FlanguageCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.SetlargestChangeId(AIndex : Integer; AValue : string); 

begin
  If (FlargestChangeId=AValue) then exit;
  FlargestChangeId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.SetmaxUploadSizes(AIndex : Integer; AValue : TAboutmaxUploadSizes); 

begin
  If (FmaxUploadSizes=AValue) then exit;
  FmaxUploadSizes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.SetpermissionId(AIndex : Integer; AValue : string); 

begin
  If (FpermissionId=AValue) then exit;
  FpermissionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.SetquotaBytesByService(AIndex : Integer; AValue : TAboutquotaBytesByService); 

begin
  If (FquotaBytesByService=AValue) then exit;
  FquotaBytesByService:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.SetquotaBytesTotal(AIndex : Integer; AValue : string); 

begin
  If (FquotaBytesTotal=AValue) then exit;
  FquotaBytesTotal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.SetquotaBytesUsed(AIndex : Integer; AValue : string); 

begin
  If (FquotaBytesUsed=AValue) then exit;
  FquotaBytesUsed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.SetquotaBytesUsedAggregate(AIndex : Integer; AValue : string); 

begin
  If (FquotaBytesUsedAggregate=AValue) then exit;
  FquotaBytesUsedAggregate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.SetquotaBytesUsedInTrash(AIndex : Integer; AValue : string); 

begin
  If (FquotaBytesUsedInTrash=AValue) then exit;
  FquotaBytesUsedInTrash:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.SetquotaType(AIndex : Integer; AValue : string); 

begin
  If (FquotaType=AValue) then exit;
  FquotaType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.SetremainingChangeIds(AIndex : Integer; AValue : string); 

begin
  If (FremainingChangeIds=AValue) then exit;
  FremainingChangeIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.SetrootFolderId(AIndex : Integer; AValue : string); 

begin
  If (FrootFolderId=AValue) then exit;
  FrootFolderId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAbout.Setuser(AIndex : Integer; AValue : TUser); 

begin
  If (Fuser=AValue) then exit;
  Fuser:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAboutadditionalRoleInfo
  --------------------------------------------------------------------}


Procedure TAboutadditionalRoleInfo.SetroleSets(AIndex : Integer; AValue : TAboutadditionalRoleInforoleSets); 

begin
  If (FroleSets=AValue) then exit;
  FroleSets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAboutadditionalRoleInfo.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAboutadditionalRoleInfo.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TAboutadditionalRoleInforoleSets
  --------------------------------------------------------------------}


Procedure TAboutadditionalRoleInforoleSets.SetadditionalRoles(AIndex : Integer; AValue : TAboutadditionalRoleInforoleSetsadditionalRoles); 

begin
  If (FadditionalRoles=AValue) then exit;
  FadditionalRoles:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAboutadditionalRoleInforoleSets.SetprimaryRole(AIndex : Integer; AValue : string); 

begin
  If (FprimaryRole=AValue) then exit;
  FprimaryRole:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAboutadditionalRoleInforoleSetsadditionalRoles
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAboutexportFormats
  --------------------------------------------------------------------}


Procedure TAboutexportFormats.Setsource(AIndex : Integer; AValue : string); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAboutexportFormats.Settargets(AIndex : Integer; AValue : TAboutexportFormatstargets); 

begin
  If (Ftargets=AValue) then exit;
  Ftargets:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAboutexportFormatstargets
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAboutfeatures
  --------------------------------------------------------------------}


Procedure TAboutfeatures.SetfeatureName(AIndex : Integer; AValue : string); 

begin
  If (FfeatureName=AValue) then exit;
  FfeatureName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAboutfeatures.SetfeatureRate(AIndex : Integer; AValue : double); 

begin
  If (FfeatureRate=AValue) then exit;
  FfeatureRate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAboutfolderColorPalette
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAboutimportFormats
  --------------------------------------------------------------------}


Procedure TAboutimportFormats.Setsource(AIndex : Integer; AValue : string); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAboutimportFormats.Settargets(AIndex : Integer; AValue : TAboutimportFormatstargets); 

begin
  If (Ftargets=AValue) then exit;
  Ftargets:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAboutimportFormatstargets
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAboutmaxUploadSizes
  --------------------------------------------------------------------}


Procedure TAboutmaxUploadSizes.Setsize(AIndex : Integer; AValue : string); 

begin
  If (Fsize=AValue) then exit;
  Fsize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAboutmaxUploadSizes.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAboutmaxUploadSizes.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TAboutquotaBytesByService
  --------------------------------------------------------------------}


Procedure TAboutquotaBytesByService.SetbytesUsed(AIndex : Integer; AValue : string); 

begin
  If (FbytesUsed=AValue) then exit;
  FbytesUsed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAboutquotaBytesByService.SetserviceName(AIndex : Integer; AValue : string); 

begin
  If (FserviceName=AValue) then exit;
  FserviceName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TApp
  --------------------------------------------------------------------}


Procedure TApp.Setauthorized(AIndex : Integer; AValue : boolean); 

begin
  If (Fauthorized=AValue) then exit;
  Fauthorized:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApp.SetcreateInFolderTemplate(AIndex : Integer; AValue : string); 

begin
  If (FcreateInFolderTemplate=AValue) then exit;
  FcreateInFolderTemplate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApp.SetcreateUrl(AIndex : Integer; AValue : string); 

begin
  If (FcreateUrl=AValue) then exit;
  FcreateUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApp.SethasDriveWideScope(AIndex : Integer; AValue : boolean); 

begin
  If (FhasDriveWideScope=AValue) then exit;
  FhasDriveWideScope:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApp.Seticons(AIndex : Integer; AValue : TAppicons); 

begin
  If (Ficons=AValue) then exit;
  Ficons:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApp.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApp.Setinstalled(AIndex : Integer; AValue : boolean); 

begin
  If (Finstalled=AValue) then exit;
  Finstalled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApp.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApp.SetlongDescription(AIndex : Integer; AValue : string); 

begin
  If (FlongDescription=AValue) then exit;
  FlongDescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApp.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApp.SetobjectType(AIndex : Integer; AValue : string); 

begin
  If (FobjectType=AValue) then exit;
  FobjectType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApp.SetopenUrlTemplate(AIndex : Integer; AValue : string); 

begin
  If (FopenUrlTemplate=AValue) then exit;
  FopenUrlTemplate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApp.SetprimaryFileExtensions(AIndex : Integer; AValue : TAppprimaryFileExtensions); 

begin
  If (FprimaryFileExtensions=AValue) then exit;
  FprimaryFileExtensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApp.SetprimaryMimeTypes(AIndex : Integer; AValue : TAppprimaryMimeTypes); 

begin
  If (FprimaryMimeTypes=AValue) then exit;
  FprimaryMimeTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApp.SetproductId(AIndex : Integer; AValue : string); 

begin
  If (FproductId=AValue) then exit;
  FproductId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApp.SetproductUrl(AIndex : Integer; AValue : string); 

begin
  If (FproductUrl=AValue) then exit;
  FproductUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApp.SetsecondaryFileExtensions(AIndex : Integer; AValue : TAppsecondaryFileExtensions); 

begin
  If (FsecondaryFileExtensions=AValue) then exit;
  FsecondaryFileExtensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApp.SetsecondaryMimeTypes(AIndex : Integer; AValue : TAppsecondaryMimeTypes); 

begin
  If (FsecondaryMimeTypes=AValue) then exit;
  FsecondaryMimeTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApp.SetshortDescription(AIndex : Integer; AValue : string); 

begin
  If (FshortDescription=AValue) then exit;
  FshortDescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApp.SetsupportsCreate(AIndex : Integer; AValue : boolean); 

begin
  If (FsupportsCreate=AValue) then exit;
  FsupportsCreate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApp.SetsupportsImport(AIndex : Integer; AValue : boolean); 

begin
  If (FsupportsImport=AValue) then exit;
  FsupportsImport:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApp.SetsupportsMultiOpen(AIndex : Integer; AValue : boolean); 

begin
  If (FsupportsMultiOpen=AValue) then exit;
  FsupportsMultiOpen:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApp.SetsupportsOfflineCreate(AIndex : Integer; AValue : boolean); 

begin
  If (FsupportsOfflineCreate=AValue) then exit;
  FsupportsOfflineCreate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApp.SetuseByDefault(AIndex : Integer; AValue : boolean); 

begin
  If (FuseByDefault=AValue) then exit;
  FuseByDefault:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAppicons
  --------------------------------------------------------------------}


Procedure TAppicons.Setcategory(AIndex : Integer; AValue : string); 

begin
  If (Fcategory=AValue) then exit;
  Fcategory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAppicons.SeticonUrl(AIndex : Integer; AValue : string); 

begin
  If (FiconUrl=AValue) then exit;
  FiconUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAppicons.Setsize(AIndex : Integer; AValue : integer); 

begin
  If (Fsize=AValue) then exit;
  Fsize:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAppprimaryFileExtensions
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAppprimaryMimeTypes
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAppsecondaryFileExtensions
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAppsecondaryMimeTypes
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAppList
  --------------------------------------------------------------------}


Procedure TAppList.SetdefaultAppIds(AIndex : Integer; AValue : TAppListdefaultAppIds); 

begin
  If (FdefaultAppIds=AValue) then exit;
  FdefaultAppIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAppList.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAppList.Setitems(AIndex : Integer; AValue : TAppListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAppList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAppList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAppListdefaultAppIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAppListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TChange
  --------------------------------------------------------------------}


Procedure TChange.Setdeleted(AIndex : Integer; AValue : boolean); 

begin
  If (Fdeleted=AValue) then exit;
  Fdeleted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChange.Set_file(AIndex : Integer; AValue : TFile); 

begin
  If (F_file=AValue) then exit;
  F_file:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChange.SetfileId(AIndex : Integer; AValue : string); 

begin
  If (FfileId=AValue) then exit;
  FfileId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChange.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChange.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChange.SetmodificationDate(AIndex : Integer; AValue : TDatetime); 

begin
  If (FmodificationDate=AValue) then exit;
  FmodificationDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChange.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
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


Procedure TChangeList.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangeList.Setitems(AIndex : Integer; AValue : TChangeListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangeList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangeList.SetlargestChangeId(AIndex : Integer; AValue : string); 

begin
  If (FlargestChangeId=AValue) then exit;
  FlargestChangeId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangeList.SetnextLink(AIndex : Integer; AValue : string); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangeList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangeList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChangeListitems
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
  TChildList
  --------------------------------------------------------------------}


Procedure TChildList.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChildList.Setitems(AIndex : Integer; AValue : TChildListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChildList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChildList.SetnextLink(AIndex : Integer; AValue : string); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChildList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChildList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChildListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TChildReference
  --------------------------------------------------------------------}


Procedure TChildReference.SetchildLink(AIndex : Integer; AValue : string); 

begin
  If (FchildLink=AValue) then exit;
  FchildLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChildReference.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChildReference.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChildReference.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TComment
  --------------------------------------------------------------------}


Procedure TComment.Setanchor(AIndex : Integer; AValue : string); 

begin
  If (Fanchor=AValue) then exit;
  Fanchor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setauthor(AIndex : Integer; AValue : TUser); 

begin
  If (Fauthor=AValue) then exit;
  Fauthor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.SetcommentId(AIndex : Integer; AValue : string); 

begin
  If (FcommentId=AValue) then exit;
  FcommentId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setcontent(AIndex : Integer; AValue : string); 

begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setcontext(AIndex : Integer; AValue : TCommentcontext); 

begin
  If (Fcontext=AValue) then exit;
  Fcontext:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.SetcreatedDate(AIndex : Integer; AValue : TDatetime); 

begin
  If (FcreatedDate=AValue) then exit;
  FcreatedDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setdeleted(AIndex : Integer; AValue : boolean); 

begin
  If (Fdeleted=AValue) then exit;
  Fdeleted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.SetfileId(AIndex : Integer; AValue : string); 

begin
  If (FfileId=AValue) then exit;
  FfileId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.SetfileTitle(AIndex : Integer; AValue : string); 

begin
  If (FfileTitle=AValue) then exit;
  FfileTitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.SethtmlContent(AIndex : Integer; AValue : string); 

begin
  If (FhtmlContent=AValue) then exit;
  FhtmlContent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.SetmodifiedDate(AIndex : Integer; AValue : TDatetime); 

begin
  If (FmodifiedDate=AValue) then exit;
  FmodifiedDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setreplies(AIndex : Integer; AValue : TCommentreplies); 

begin
  If (Freplies=AValue) then exit;
  Freplies:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentcontext
  --------------------------------------------------------------------}


Procedure TCommentcontext.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentcontext.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TCommentcontext.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TCommentreplies
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCommentList
  --------------------------------------------------------------------}


Procedure TCommentList.Setitems(AIndex : Integer; AValue : TCommentListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentList.SetnextLink(AIndex : Integer; AValue : string); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCommentReply
  --------------------------------------------------------------------}


Procedure TCommentReply.Setauthor(AIndex : Integer; AValue : TUser); 

begin
  If (Fauthor=AValue) then exit;
  Fauthor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentReply.Setcontent(AIndex : Integer; AValue : string); 

begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentReply.SetcreatedDate(AIndex : Integer; AValue : TDatetime); 

begin
  If (FcreatedDate=AValue) then exit;
  FcreatedDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentReply.Setdeleted(AIndex : Integer; AValue : boolean); 

begin
  If (Fdeleted=AValue) then exit;
  Fdeleted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentReply.SethtmlContent(AIndex : Integer; AValue : string); 

begin
  If (FhtmlContent=AValue) then exit;
  FhtmlContent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentReply.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentReply.SetmodifiedDate(AIndex : Integer; AValue : TDatetime); 

begin
  If (FmodifiedDate=AValue) then exit;
  FmodifiedDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentReply.SetreplyId(AIndex : Integer; AValue : string); 

begin
  If (FreplyId=AValue) then exit;
  FreplyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentReply.Setverb(AIndex : Integer; AValue : string); 

begin
  If (Fverb=AValue) then exit;
  Fverb:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentReplyList
  --------------------------------------------------------------------}


Procedure TCommentReplyList.Setitems(AIndex : Integer; AValue : TCommentReplyListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentReplyList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentReplyList.SetnextLink(AIndex : Integer; AValue : string); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentReplyList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentReplyList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentReplyListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFile
  --------------------------------------------------------------------}


Procedure TFile.SetalternateLink(AIndex : Integer; AValue : string); 

begin
  If (FalternateLink=AValue) then exit;
  FalternateLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetappDataContents(AIndex : Integer; AValue : boolean); 

begin
  If (FappDataContents=AValue) then exit;
  FappDataContents:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setcopyable(AIndex : Integer; AValue : boolean); 

begin
  If (Fcopyable=AValue) then exit;
  Fcopyable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetcreatedDate(AIndex : Integer; AValue : TDatetime); 

begin
  If (FcreatedDate=AValue) then exit;
  FcreatedDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetdefaultOpenWithLink(AIndex : Integer; AValue : string); 

begin
  If (FdefaultOpenWithLink=AValue) then exit;
  FdefaultOpenWithLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetdownloadUrl(AIndex : Integer; AValue : string); 

begin
  If (FdownloadUrl=AValue) then exit;
  FdownloadUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Seteditable(AIndex : Integer; AValue : boolean); 

begin
  If (Feditable=AValue) then exit;
  Feditable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetembedLink(AIndex : Integer; AValue : string); 

begin
  If (FembedLink=AValue) then exit;
  FembedLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetexplicitlyTrashed(AIndex : Integer; AValue : boolean); 

begin
  If (FexplicitlyTrashed=AValue) then exit;
  FexplicitlyTrashed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetexportLinks(AIndex : Integer; AValue : TFileexportLinks); 

begin
  If (FexportLinks=AValue) then exit;
  FexportLinks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetfileExtension(AIndex : Integer; AValue : string); 

begin
  If (FfileExtension=AValue) then exit;
  FfileExtension:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetfileSize(AIndex : Integer; AValue : string); 

begin
  If (FfileSize=AValue) then exit;
  FfileSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetfolderColorRgb(AIndex : Integer; AValue : string); 

begin
  If (FfolderColorRgb=AValue) then exit;
  FfolderColorRgb:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetheadRevisionId(AIndex : Integer; AValue : string); 

begin
  If (FheadRevisionId=AValue) then exit;
  FheadRevisionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SeticonLink(AIndex : Integer; AValue : string); 

begin
  If (FiconLink=AValue) then exit;
  FiconLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetimageMediaMetadata(AIndex : Integer; AValue : TFileimageMediaMetadata); 

begin
  If (FimageMediaMetadata=AValue) then exit;
  FimageMediaMetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetindexableText(AIndex : Integer; AValue : TFileindexableText); 

begin
  If (FindexableText=AValue) then exit;
  FindexableText:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setlabels(AIndex : Integer; AValue : TFilelabels); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetlastModifyingUser(AIndex : Integer; AValue : TUser); 

begin
  If (FlastModifyingUser=AValue) then exit;
  FlastModifyingUser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetlastModifyingUserName(AIndex : Integer; AValue : string); 

begin
  If (FlastModifyingUserName=AValue) then exit;
  FlastModifyingUserName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetlastViewedByMeDate(AIndex : Integer; AValue : TDatetime); 

begin
  If (FlastViewedByMeDate=AValue) then exit;
  FlastViewedByMeDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetmarkedViewedByMeDate(AIndex : Integer; AValue : TDatetime); 

begin
  If (FmarkedViewedByMeDate=AValue) then exit;
  FmarkedViewedByMeDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setmd5Checksum(AIndex : Integer; AValue : string); 

begin
  If (Fmd5Checksum=AValue) then exit;
  Fmd5Checksum:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetmimeType(AIndex : Integer; AValue : string); 

begin
  If (FmimeType=AValue) then exit;
  FmimeType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetmodifiedByMeDate(AIndex : Integer; AValue : TDatetime); 

begin
  If (FmodifiedByMeDate=AValue) then exit;
  FmodifiedByMeDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetmodifiedDate(AIndex : Integer; AValue : TDatetime); 

begin
  If (FmodifiedDate=AValue) then exit;
  FmodifiedDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetopenWithLinks(AIndex : Integer; AValue : TFileopenWithLinks); 

begin
  If (FopenWithLinks=AValue) then exit;
  FopenWithLinks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetoriginalFilename(AIndex : Integer; AValue : string); 

begin
  If (ForiginalFilename=AValue) then exit;
  ForiginalFilename:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetownerNames(AIndex : Integer; AValue : TFileownerNames); 

begin
  If (FownerNames=AValue) then exit;
  FownerNames:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setowners(AIndex : Integer; AValue : TFileowners); 

begin
  If (Fowners=AValue) then exit;
  Fowners:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setparents(AIndex : Integer; AValue : TFileparents); 

begin
  If (Fparents=AValue) then exit;
  Fparents:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setpermissions(AIndex : Integer; AValue : TFilepermissions); 

begin
  If (Fpermissions=AValue) then exit;
  Fpermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setproperties(AIndex : Integer; AValue : TFileproperties); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetquotaBytesUsed(AIndex : Integer; AValue : string); 

begin
  If (FquotaBytesUsed=AValue) then exit;
  FquotaBytesUsed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setshared(AIndex : Integer; AValue : boolean); 

begin
  If (Fshared=AValue) then exit;
  Fshared:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetsharedWithMeDate(AIndex : Integer; AValue : TDatetime); 

begin
  If (FsharedWithMeDate=AValue) then exit;
  FsharedWithMeDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetsharingUser(AIndex : Integer; AValue : TUser); 

begin
  If (FsharingUser=AValue) then exit;
  FsharingUser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setthumbnail(AIndex : Integer; AValue : TFilethumbnail); 

begin
  If (Fthumbnail=AValue) then exit;
  Fthumbnail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetthumbnailLink(AIndex : Integer; AValue : string); 

begin
  If (FthumbnailLink=AValue) then exit;
  FthumbnailLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetuserPermission(AIndex : Integer; AValue : TPermission); 

begin
  If (FuserPermission=AValue) then exit;
  FuserPermission:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setversion(AIndex : Integer; AValue : string); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetvideoMediaMetadata(AIndex : Integer; AValue : TFilevideoMediaMetadata); 

begin
  If (FvideoMediaMetadata=AValue) then exit;
  FvideoMediaMetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetwebContentLink(AIndex : Integer; AValue : string); 

begin
  If (FwebContentLink=AValue) then exit;
  FwebContentLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetwebViewLink(AIndex : Integer; AValue : string); 

begin
  If (FwebViewLink=AValue) then exit;
  FwebViewLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.SetwritersCanShare(AIndex : Integer; AValue : boolean); 

begin
  If (FwritersCanShare=AValue) then exit;
  FwritersCanShare:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFileexportLinks
  --------------------------------------------------------------------}


Class Function TFileexportLinks.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TFileimageMediaMetadata
  --------------------------------------------------------------------}


Procedure TFileimageMediaMetadata.Setaperture(AIndex : Integer; AValue : integer); 

begin
  If (Faperture=AValue) then exit;
  Faperture:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileimageMediaMetadata.SetcameraMake(AIndex : Integer; AValue : string); 

begin
  If (FcameraMake=AValue) then exit;
  FcameraMake:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileimageMediaMetadata.SetcameraModel(AIndex : Integer; AValue : string); 

begin
  If (FcameraModel=AValue) then exit;
  FcameraModel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileimageMediaMetadata.SetcolorSpace(AIndex : Integer; AValue : string); 

begin
  If (FcolorSpace=AValue) then exit;
  FcolorSpace:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileimageMediaMetadata.Setdate(AIndex : Integer; AValue : string); 

begin
  If (Fdate=AValue) then exit;
  Fdate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileimageMediaMetadata.SetexposureBias(AIndex : Integer; AValue : integer); 

begin
  If (FexposureBias=AValue) then exit;
  FexposureBias:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileimageMediaMetadata.SetexposureMode(AIndex : Integer; AValue : string); 

begin
  If (FexposureMode=AValue) then exit;
  FexposureMode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileimageMediaMetadata.SetexposureTime(AIndex : Integer; AValue : integer); 

begin
  If (FexposureTime=AValue) then exit;
  FexposureTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileimageMediaMetadata.SetflashUsed(AIndex : Integer; AValue : boolean); 

begin
  If (FflashUsed=AValue) then exit;
  FflashUsed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileimageMediaMetadata.SetfocalLength(AIndex : Integer; AValue : integer); 

begin
  If (FfocalLength=AValue) then exit;
  FfocalLength:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileimageMediaMetadata.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileimageMediaMetadata.SetisoSpeed(AIndex : Integer; AValue : integer); 

begin
  If (FisoSpeed=AValue) then exit;
  FisoSpeed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileimageMediaMetadata.Setlens(AIndex : Integer; AValue : string); 

begin
  If (Flens=AValue) then exit;
  Flens:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileimageMediaMetadata.Setlocation(AIndex : Integer; AValue : TFileimageMediaMetadatalocation); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileimageMediaMetadata.SetmaxApertureValue(AIndex : Integer; AValue : integer); 

begin
  If (FmaxApertureValue=AValue) then exit;
  FmaxApertureValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileimageMediaMetadata.SetmeteringMode(AIndex : Integer; AValue : string); 

begin
  If (FmeteringMode=AValue) then exit;
  FmeteringMode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileimageMediaMetadata.Setrotation(AIndex : Integer; AValue : integer); 

begin
  If (Frotation=AValue) then exit;
  Frotation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileimageMediaMetadata.Setsensor(AIndex : Integer; AValue : string); 

begin
  If (Fsensor=AValue) then exit;
  Fsensor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileimageMediaMetadata.SetsubjectDistance(AIndex : Integer; AValue : integer); 

begin
  If (FsubjectDistance=AValue) then exit;
  FsubjectDistance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileimageMediaMetadata.SetwhiteBalance(AIndex : Integer; AValue : string); 

begin
  If (FwhiteBalance=AValue) then exit;
  FwhiteBalance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileimageMediaMetadata.Setwidth(AIndex : Integer; AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFileimageMediaMetadatalocation
  --------------------------------------------------------------------}


Procedure TFileimageMediaMetadatalocation.Setaltitude(AIndex : Integer; AValue : double); 

begin
  If (Faltitude=AValue) then exit;
  Faltitude:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileimageMediaMetadatalocation.Setlatitude(AIndex : Integer; AValue : double); 

begin
  If (Flatitude=AValue) then exit;
  Flatitude:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileimageMediaMetadatalocation.Setlongitude(AIndex : Integer; AValue : double); 

begin
  If (Flongitude=AValue) then exit;
  Flongitude:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFileindexableText
  --------------------------------------------------------------------}


Procedure TFileindexableText.Settext(AIndex : Integer; AValue : string); 

begin
  If (Ftext=AValue) then exit;
  Ftext:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFilelabels
  --------------------------------------------------------------------}


Procedure TFilelabels.Sethidden(AIndex : Integer; AValue : boolean); 

begin
  If (Fhidden=AValue) then exit;
  Fhidden:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilelabels.Setrestricted(AIndex : Integer; AValue : boolean); 

begin
  If (Frestricted=AValue) then exit;
  Frestricted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilelabels.Setstarred(AIndex : Integer; AValue : boolean); 

begin
  If (Fstarred=AValue) then exit;
  Fstarred:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilelabels.Settrashed(AIndex : Integer; AValue : boolean); 

begin
  If (Ftrashed=AValue) then exit;
  Ftrashed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilelabels.Setviewed(AIndex : Integer; AValue : boolean); 

begin
  If (Fviewed=AValue) then exit;
  Fviewed:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFileopenWithLinks
  --------------------------------------------------------------------}


Class Function TFileopenWithLinks.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TFileownerNames
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFileowners
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFileparents
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFilepermissions
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFileproperties
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFilethumbnail
  --------------------------------------------------------------------}


Procedure TFilethumbnail.Setimage(AIndex : Integer; AValue : string); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilethumbnail.SetmimeType(AIndex : Integer; AValue : string); 

begin
  If (FmimeType=AValue) then exit;
  FmimeType:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFilevideoMediaMetadata
  --------------------------------------------------------------------}


Procedure TFilevideoMediaMetadata.SetdurationMillis(AIndex : Integer; AValue : string); 

begin
  If (FdurationMillis=AValue) then exit;
  FdurationMillis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilevideoMediaMetadata.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilevideoMediaMetadata.Setwidth(AIndex : Integer; AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFileList
  --------------------------------------------------------------------}


Procedure TFileList.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileList.Setitems(AIndex : Integer; AValue : TFileListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileList.SetnextLink(AIndex : Integer; AValue : string); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFileListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TParentList
  --------------------------------------------------------------------}


Procedure TParentList.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParentList.Setitems(AIndex : Integer; AValue : TParentListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParentList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParentList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TParentListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TParentReference
  --------------------------------------------------------------------}


Procedure TParentReference.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParentReference.SetisRoot(AIndex : Integer; AValue : boolean); 

begin
  If (FisRoot=AValue) then exit;
  FisRoot:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParentReference.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParentReference.SetparentLink(AIndex : Integer; AValue : string); 

begin
  If (FparentLink=AValue) then exit;
  FparentLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParentReference.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPermission
  --------------------------------------------------------------------}


Procedure TPermission.SetadditionalRoles(AIndex : Integer; AValue : TPermissionadditionalRoles); 

begin
  If (FadditionalRoles=AValue) then exit;
  FadditionalRoles:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.SetauthKey(AIndex : Integer; AValue : string); 

begin
  If (FauthKey=AValue) then exit;
  FauthKey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.Setdomain(AIndex : Integer; AValue : string); 

begin
  If (Fdomain=AValue) then exit;
  Fdomain:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.SetemailAddress(AIndex : Integer; AValue : string); 

begin
  If (FemailAddress=AValue) then exit;
  FemailAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.SetphotoLink(AIndex : Integer; AValue : string); 

begin
  If (FphotoLink=AValue) then exit;
  FphotoLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.Setrole(AIndex : Integer; AValue : string); 

begin
  If (Frole=AValue) then exit;
  Frole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.SetwithLink(AIndex : Integer; AValue : boolean); 

begin
  If (FwithLink=AValue) then exit;
  FwithLink:=AValue;
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
  TPermissionadditionalRoles
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPermissionId
  --------------------------------------------------------------------}


Procedure TPermissionId.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermissionId.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPermissionList
  --------------------------------------------------------------------}


Procedure TPermissionList.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermissionList.Setitems(AIndex : Integer; AValue : TPermissionListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermissionList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermissionList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPermissionListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TProperty
  --------------------------------------------------------------------}


Procedure TProperty.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProperty.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProperty.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProperty.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProperty.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProperty.Setvisibility(AIndex : Integer; AValue : string); 

begin
  If (Fvisibility=AValue) then exit;
  Fvisibility:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPropertyList
  --------------------------------------------------------------------}


Procedure TPropertyList.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPropertyList.Setitems(AIndex : Integer; AValue : TPropertyListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPropertyList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPropertyList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPropertyListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TRevision
  --------------------------------------------------------------------}


Procedure TRevision.SetdownloadUrl(AIndex : Integer; AValue : string); 

begin
  If (FdownloadUrl=AValue) then exit;
  FdownloadUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevision.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevision.SetexportLinks(AIndex : Integer; AValue : TRevisionexportLinks); 

begin
  If (FexportLinks=AValue) then exit;
  FexportLinks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevision.SetfileSize(AIndex : Integer; AValue : string); 

begin
  If (FfileSize=AValue) then exit;
  FfileSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevision.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevision.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevision.SetlastModifyingUser(AIndex : Integer; AValue : TUser); 

begin
  If (FlastModifyingUser=AValue) then exit;
  FlastModifyingUser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevision.SetlastModifyingUserName(AIndex : Integer; AValue : string); 

begin
  If (FlastModifyingUserName=AValue) then exit;
  FlastModifyingUserName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevision.Setmd5Checksum(AIndex : Integer; AValue : string); 

begin
  If (Fmd5Checksum=AValue) then exit;
  Fmd5Checksum:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevision.SetmimeType(AIndex : Integer; AValue : string); 

begin
  If (FmimeType=AValue) then exit;
  FmimeType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevision.SetmodifiedDate(AIndex : Integer; AValue : TDatetime); 

begin
  If (FmodifiedDate=AValue) then exit;
  FmodifiedDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevision.SetoriginalFilename(AIndex : Integer; AValue : string); 

begin
  If (ForiginalFilename=AValue) then exit;
  ForiginalFilename:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevision.Setpinned(AIndex : Integer; AValue : boolean); 

begin
  If (Fpinned=AValue) then exit;
  Fpinned:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevision.SetpublishAuto(AIndex : Integer; AValue : boolean); 

begin
  If (FpublishAuto=AValue) then exit;
  FpublishAuto:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevision.Set_published(AIndex : Integer; AValue : boolean); 

begin
  If (F_published=AValue) then exit;
  F_published:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevision.SetpublishedLink(AIndex : Integer; AValue : string); 

begin
  If (FpublishedLink=AValue) then exit;
  FpublishedLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevision.SetpublishedOutsideDomain(AIndex : Integer; AValue : boolean); 

begin
  If (FpublishedOutsideDomain=AValue) then exit;
  FpublishedOutsideDomain:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevision.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
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
  TRevisionexportLinks
  --------------------------------------------------------------------}


Class Function TRevisionexportLinks.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TRevisionList
  --------------------------------------------------------------------}


Procedure TRevisionList.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevisionList.Setitems(AIndex : Integer; AValue : TRevisionListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevisionList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRevisionList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRevisionListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TUser
  --------------------------------------------------------------------}


Procedure TUser.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.SetemailAddress(AIndex : Integer; AValue : string); 

begin
  If (FemailAddress=AValue) then exit;
  FemailAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.SetisAuthenticatedUser(AIndex : Integer; AValue : boolean); 

begin
  If (FisAuthenticatedUser=AValue) then exit;
  FisAuthenticatedUser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.SetpermissionId(AIndex : Integer; AValue : string); 

begin
  If (FpermissionId=AValue) then exit;
  FpermissionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setpicture(AIndex : Integer; AValue : TUserpicture); 

begin
  If (Fpicture=AValue) then exit;
  Fpicture:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUserpicture
  --------------------------------------------------------------------}


Procedure TUserpicture.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
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

Function TAboutResource.Get(AQuery : string = '') : TAbout;

Const
  _HTTPMethod = 'GET';
  _Path       = 'about';
  _Methodid   = 'drive.about.get';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TAbout) as TAbout;
end;


Function TAboutResource.Get(AQuery : TAboutgetOptions) : TAbout;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'includeSubscribed',AQuery.includeSubscribed);
  AddToQuery(_Q,'maxChangeIdCount',AQuery.maxChangeIdCount);
  AddToQuery(_Q,'startChangeId',AQuery.startChangeId);
  Result:=Get(_Q);
end;



{ --------------------------------------------------------------------
  TAppsResource
  --------------------------------------------------------------------}


Class Function TAppsResource.ResourceName : String;

begin
  Result:='apps';
end;

Class Function TAppsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdriveAPI;
end;

Function TAppsResource.Get(appId: string) : TApp;

Const
  _HTTPMethod = 'GET';
  _Path       = 'apps/{appId}';
  _Methodid   = 'drive.apps.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['appId',appId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TApp) as TApp;
end;

Function TAppsResource.List(AQuery : string = '') : TAppList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'apps';
  _Methodid   = 'drive.apps.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TAppList) as TAppList;
end;


Function TAppsResource.List(AQuery : TAppslistOptions) : TAppList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'appFilterExtensions',AQuery.appFilterExtensions);
  AddToQuery(_Q,'appFilterMimeTypes',AQuery.appFilterMimeTypes);
  AddToQuery(_Q,'languageCode',AQuery.languageCode);
  Result:=List(_Q);
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

Function TChangesResource.Get(changeId: string) : TChange;

Const
  _HTTPMethod = 'GET';
  _Path       = 'changes/{changeId}';
  _Methodid   = 'drive.changes.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['changeId',changeId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TChange) as TChange;
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
  AddToQuery(_Q,'includeDeleted',AQuery.includeDeleted);
  AddToQuery(_Q,'includeSubscribed',AQuery.includeSubscribed);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'startChangeId',AQuery.startChangeId);
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
  AddToQuery(_Q,'includeDeleted',AQuery.includeDeleted);
  AddToQuery(_Q,'includeSubscribed',AQuery.includeSubscribed);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'startChangeId',AQuery.startChangeId);
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
  TChildrenResource
  --------------------------------------------------------------------}


Class Function TChildrenResource.ResourceName : String;

begin
  Result:='children';
end;

Class Function TChildrenResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdriveAPI;
end;

Procedure TChildrenResource.Delete(childId: string; folderId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'files/{folderId}/children/{childId}';
  _Methodid   = 'drive.children.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['childId',childId,'folderId',folderId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TChildrenResource.Get(childId: string; folderId: string) : TChildReference;

Const
  _HTTPMethod = 'GET';
  _Path       = 'files/{folderId}/children/{childId}';
  _Methodid   = 'drive.children.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['childId',childId,'folderId',folderId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TChildReference) as TChildReference;
end;

Function TChildrenResource.Insert(folderId: string; aChildReference : TChildReference) : TChildReference;

Const
  _HTTPMethod = 'POST';
  _Path       = 'files/{folderId}/children';
  _Methodid   = 'drive.children.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['folderId',folderId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aChildReference,TChildReference) as TChildReference;
end;

Function TChildrenResource.List(folderId: string; AQuery : string = '') : TChildList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'files/{folderId}/children';
  _Methodid   = 'drive.children.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['folderId',folderId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TChildList) as TChildList;
end;


Function TChildrenResource.List(folderId: string; AQuery : TChildrenlistOptions) : TChildList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'q',AQuery.q);
  Result:=List(folderId,_Q);
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

Function TCommentsResource.Insert(fileId: string; aComment : TComment) : TComment;

Const
  _HTTPMethod = 'POST';
  _Path       = 'files/{fileId}/comments';
  _Methodid   = 'drive.comments.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aComment,TComment) as TComment;
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
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'updatedMin',AQuery.updatedMin);
  Result:=List(fileId,_Q);
end;

Function TCommentsResource.Patch(commentId: string; fileId: string; aComment : TComment) : TComment;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'files/{fileId}/comments/{commentId}';
  _Methodid   = 'drive.comments.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['commentId',commentId,'fileId',fileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aComment,TComment) as TComment;
end;

Function TCommentsResource.Update(commentId: string; fileId: string; aComment : TComment) : TComment;

Const
  _HTTPMethod = 'PUT';
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
  AddToQuery(_Q,'convert',AQuery.convert);
  AddToQuery(_Q,'ocr',AQuery.ocr);
  AddToQuery(_Q,'ocrLanguage',AQuery.ocrLanguage);
  AddToQuery(_Q,'pinned',AQuery.pinned);
  AddToQuery(_Q,'timedTextLanguage',AQuery.timedTextLanguage);
  AddToQuery(_Q,'timedTextTrackName',AQuery.timedTextTrackName);
  AddToQuery(_Q,'visibility',AQuery.visibility);
  Result:=Copy(fileId,aFile,_Q);
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
  AddToQuery(_Q,'projection',AQuery.projection);
  AddToQuery(_Q,'revisionId',AQuery.revisionId);
  AddToQuery(_Q,'updateViewedDate',AQuery.updateViewedDate);
  Result:=Get(fileId,_Q);
end;

Function TFilesResource.Insert(aFile : TFile; AQuery : string = '') : TFile;

Const
  _HTTPMethod = 'POST';
  _Path       = 'files';
  _Methodid   = 'drive.files.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aFile,TFile) as TFile;
end;


Function TFilesResource.Insert(aFile : TFile; AQuery : TFilesinsertOptions) : TFile;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'convert',AQuery.convert);
  AddToQuery(_Q,'ocr',AQuery.ocr);
  AddToQuery(_Q,'ocrLanguage',AQuery.ocrLanguage);
  AddToQuery(_Q,'pinned',AQuery.pinned);
  AddToQuery(_Q,'timedTextLanguage',AQuery.timedTextLanguage);
  AddToQuery(_Q,'timedTextTrackName',AQuery.timedTextTrackName);
  AddToQuery(_Q,'useContentAsIndexableText',AQuery.useContentAsIndexableText);
  AddToQuery(_Q,'visibility',AQuery.visibility);
  Result:=Insert(aFile,_Q);
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
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'projection',AQuery.projection);
  AddToQuery(_Q,'q',AQuery.q);
  Result:=List(_Q);
end;

Function TFilesResource.Patch(fileId: string; aFile : TFile; AQuery : string = '') : TFile;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'files/{fileId}';
  _Methodid   = 'drive.files.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aFile,TFile) as TFile;
end;


Function TFilesResource.Patch(fileId: string; aFile : TFile; AQuery : TFilespatchOptions) : TFile;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'addParents',AQuery.addParents);
  AddToQuery(_Q,'convert',AQuery.convert);
  AddToQuery(_Q,'newRevision',AQuery.newRevision);
  AddToQuery(_Q,'ocr',AQuery.ocr);
  AddToQuery(_Q,'ocrLanguage',AQuery.ocrLanguage);
  AddToQuery(_Q,'pinned',AQuery.pinned);
  AddToQuery(_Q,'removeParents',AQuery.removeParents);
  AddToQuery(_Q,'setModifiedDate',AQuery.setModifiedDate);
  AddToQuery(_Q,'timedTextLanguage',AQuery.timedTextLanguage);
  AddToQuery(_Q,'timedTextTrackName',AQuery.timedTextTrackName);
  AddToQuery(_Q,'updateViewedDate',AQuery.updateViewedDate);
  AddToQuery(_Q,'useContentAsIndexableText',AQuery.useContentAsIndexableText);
  Result:=Patch(fileId,aFile,_Q);
end;

Function TFilesResource.Touch(fileId: string) : TFile;

Const
  _HTTPMethod = 'POST';
  _Path       = 'files/{fileId}/touch';
  _Methodid   = 'drive.files.touch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TFile) as TFile;
end;

Function TFilesResource.Trash(fileId: string) : TFile;

Const
  _HTTPMethod = 'POST';
  _Path       = 'files/{fileId}/trash';
  _Methodid   = 'drive.files.trash';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TFile) as TFile;
end;

Function TFilesResource.Untrash(fileId: string) : TFile;

Const
  _HTTPMethod = 'POST';
  _Path       = 'files/{fileId}/untrash';
  _Methodid   = 'drive.files.untrash';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TFile) as TFile;
end;

Function TFilesResource.Update(fileId: string; aFile : TFile; AQuery : string = '') : TFile;

Const
  _HTTPMethod = 'PUT';
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
  AddToQuery(_Q,'convert',AQuery.convert);
  AddToQuery(_Q,'newRevision',AQuery.newRevision);
  AddToQuery(_Q,'ocr',AQuery.ocr);
  AddToQuery(_Q,'ocrLanguage',AQuery.ocrLanguage);
  AddToQuery(_Q,'pinned',AQuery.pinned);
  AddToQuery(_Q,'removeParents',AQuery.removeParents);
  AddToQuery(_Q,'setModifiedDate',AQuery.setModifiedDate);
  AddToQuery(_Q,'timedTextLanguage',AQuery.timedTextLanguage);
  AddToQuery(_Q,'timedTextTrackName',AQuery.timedTextTrackName);
  AddToQuery(_Q,'updateViewedDate',AQuery.updateViewedDate);
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
  AddToQuery(_Q,'projection',AQuery.projection);
  AddToQuery(_Q,'revisionId',AQuery.revisionId);
  AddToQuery(_Q,'updateViewedDate',AQuery.updateViewedDate);
  Result:=Watch(fileId,aChannel,_Q);
end;



{ --------------------------------------------------------------------
  TParentsResource
  --------------------------------------------------------------------}


Class Function TParentsResource.ResourceName : String;

begin
  Result:='parents';
end;

Class Function TParentsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdriveAPI;
end;

Procedure TParentsResource.Delete(fileId: string; parentId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'files/{fileId}/parents/{parentId}';
  _Methodid   = 'drive.parents.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId,'parentId',parentId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TParentsResource.Get(fileId: string; parentId: string) : TParentReference;

Const
  _HTTPMethod = 'GET';
  _Path       = 'files/{fileId}/parents/{parentId}';
  _Methodid   = 'drive.parents.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId,'parentId',parentId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TParentReference) as TParentReference;
end;

Function TParentsResource.Insert(fileId: string; aParentReference : TParentReference) : TParentReference;

Const
  _HTTPMethod = 'POST';
  _Path       = 'files/{fileId}/parents';
  _Methodid   = 'drive.parents.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aParentReference,TParentReference) as TParentReference;
end;

Function TParentsResource.List(fileId: string) : TParentList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'files/{fileId}/parents';
  _Methodid   = 'drive.parents.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TParentList) as TParentList;
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

Function TPermissionsResource.GetIdForEmail(email: string) : TPermissionId;

Const
  _HTTPMethod = 'GET';
  _Path       = 'permissionIds/{email}';
  _Methodid   = 'drive.permissions.getIdForEmail';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['email',email]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPermissionId) as TPermissionId;
end;

Function TPermissionsResource.Insert(fileId: string; aPermission : TPermission; AQuery : string = '') : TPermission;

Const
  _HTTPMethod = 'POST';
  _Path       = 'files/{fileId}/permissions';
  _Methodid   = 'drive.permissions.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aPermission,TPermission) as TPermission;
end;


Function TPermissionsResource.Insert(fileId: string; aPermission : TPermission; AQuery : TPermissionsinsertOptions) : TPermission;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'emailMessage',AQuery.emailMessage);
  AddToQuery(_Q,'sendNotificationEmails',AQuery.sendNotificationEmails);
  Result:=Insert(fileId,aPermission,_Q);
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

Function TPermissionsResource.Patch(fileId: string; permissionId: string; aPermission : TPermission; AQuery : string = '') : TPermission;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'files/{fileId}/permissions/{permissionId}';
  _Methodid   = 'drive.permissions.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId,'permissionId',permissionId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aPermission,TPermission) as TPermission;
end;


Function TPermissionsResource.Patch(fileId: string; permissionId: string; aPermission : TPermission; AQuery : TPermissionspatchOptions) : TPermission;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'transferOwnership',AQuery.transferOwnership);
  Result:=Patch(fileId,permissionId,aPermission,_Q);
end;

Function TPermissionsResource.Update(fileId: string; permissionId: string; aPermission : TPermission; AQuery : string = '') : TPermission;

Const
  _HTTPMethod = 'PUT';
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
  TPropertiesResource
  --------------------------------------------------------------------}


Class Function TPropertiesResource.ResourceName : String;

begin
  Result:='properties';
end;

Class Function TPropertiesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdriveAPI;
end;

Procedure TPropertiesResource.Delete(fileId: string; propertyKey: string; AQuery : string = '');

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'files/{fileId}/properties/{propertyKey}';
  _Methodid   = 'drive.properties.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId,'propertyKey',propertyKey]);
  ServiceCall(_HTTPMethod,_P,AQuery,Nil,Nil);
end;


Procedure TPropertiesResource.Delete(fileId: string; propertyKey: string; AQuery : TPropertiesdeleteOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'visibility',AQuery.visibility);
  Delete(fileId,propertyKey,_Q);
end;

Function TPropertiesResource.Get(fileId: string; propertyKey: string; AQuery : string = '') : TProperty;

Const
  _HTTPMethod = 'GET';
  _Path       = 'files/{fileId}/properties/{propertyKey}';
  _Methodid   = 'drive.properties.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId,'propertyKey',propertyKey]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TProperty) as TProperty;
end;


Function TPropertiesResource.Get(fileId: string; propertyKey: string; AQuery : TPropertiesgetOptions) : TProperty;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'visibility',AQuery.visibility);
  Result:=Get(fileId,propertyKey,_Q);
end;

Function TPropertiesResource.Insert(fileId: string; aProperty : TProperty) : TProperty;

Const
  _HTTPMethod = 'POST';
  _Path       = 'files/{fileId}/properties';
  _Methodid   = 'drive.properties.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aProperty,TProperty) as TProperty;
end;

Function TPropertiesResource.List(fileId: string) : TPropertyList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'files/{fileId}/properties';
  _Methodid   = 'drive.properties.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPropertyList) as TPropertyList;
end;

Function TPropertiesResource.Patch(fileId: string; propertyKey: string; aProperty : TProperty; AQuery : string = '') : TProperty;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'files/{fileId}/properties/{propertyKey}';
  _Methodid   = 'drive.properties.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId,'propertyKey',propertyKey]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aProperty,TProperty) as TProperty;
end;


Function TPropertiesResource.Patch(fileId: string; propertyKey: string; aProperty : TProperty; AQuery : TPropertiespatchOptions) : TProperty;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'visibility',AQuery.visibility);
  Result:=Patch(fileId,propertyKey,aProperty,_Q);
end;

Function TPropertiesResource.Update(fileId: string; propertyKey: string; aProperty : TProperty; AQuery : string = '') : TProperty;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'files/{fileId}/properties/{propertyKey}';
  _Methodid   = 'drive.properties.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId,'propertyKey',propertyKey]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aProperty,TProperty) as TProperty;
end;


Function TPropertiesResource.Update(fileId: string; propertyKey: string; aProperty : TProperty; AQuery : TPropertiesupdateOptions) : TProperty;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'visibility',AQuery.visibility);
  Result:=Update(fileId,propertyKey,aProperty,_Q);
end;



{ --------------------------------------------------------------------
  TRealtimeResource
  --------------------------------------------------------------------}


Class Function TRealtimeResource.ResourceName : String;

begin
  Result:='realtime';
end;

Class Function TRealtimeResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdriveAPI;
end;

Procedure TRealtimeResource.Get(fileId: string; AQuery : string = '');

Const
  _HTTPMethod = 'GET';
  _Path       = 'files/{fileId}/realtime';
  _Methodid   = 'drive.realtime.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId]);
  ServiceCall(_HTTPMethod,_P,AQuery,Nil,Nil);
end;


Procedure TRealtimeResource.Get(fileId: string; AQuery : TRealtimegetOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'revision',AQuery.revision);
  Get(fileId,_Q);
end;

Procedure TRealtimeResource.Update(fileId: string; AQuery : string = '');

Const
  _HTTPMethod = 'PUT';
  _Path       = 'files/{fileId}/realtime';
  _Methodid   = 'drive.realtime.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId]);
  ServiceCall(_HTTPMethod,_P,AQuery,Nil,Nil);
end;


Procedure TRealtimeResource.Update(fileId: string; AQuery : TRealtimeupdateOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'baseRevision',AQuery.baseRevision);
  Update(fileId,_Q);
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

Function TRepliesResource.Get(commentId: string; fileId: string; replyId: string; AQuery : string = '') : TCommentReply;

Const
  _HTTPMethod = 'GET';
  _Path       = 'files/{fileId}/comments/{commentId}/replies/{replyId}';
  _Methodid   = 'drive.replies.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['commentId',commentId,'fileId',fileId,'replyId',replyId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TCommentReply) as TCommentReply;
end;


Function TRepliesResource.Get(commentId: string; fileId: string; replyId: string; AQuery : TRepliesgetOptions) : TCommentReply;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'includeDeleted',AQuery.includeDeleted);
  Result:=Get(commentId,fileId,replyId,_Q);
end;

Function TRepliesResource.Insert(commentId: string; fileId: string; aCommentReply : TCommentReply) : TCommentReply;

Const
  _HTTPMethod = 'POST';
  _Path       = 'files/{fileId}/comments/{commentId}/replies';
  _Methodid   = 'drive.replies.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['commentId',commentId,'fileId',fileId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCommentReply,TCommentReply) as TCommentReply;
end;

Function TRepliesResource.List(commentId: string; fileId: string; AQuery : string = '') : TCommentReplyList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'files/{fileId}/comments/{commentId}/replies';
  _Methodid   = 'drive.replies.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['commentId',commentId,'fileId',fileId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TCommentReplyList) as TCommentReplyList;
end;


Function TRepliesResource.List(commentId: string; fileId: string; AQuery : TReplieslistOptions) : TCommentReplyList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'includeDeleted',AQuery.includeDeleted);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(commentId,fileId,_Q);
end;

Function TRepliesResource.Patch(commentId: string; fileId: string; replyId: string; aCommentReply : TCommentReply) : TCommentReply;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'files/{fileId}/comments/{commentId}/replies/{replyId}';
  _Methodid   = 'drive.replies.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['commentId',commentId,'fileId',fileId,'replyId',replyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCommentReply,TCommentReply) as TCommentReply;
end;

Function TRepliesResource.Update(commentId: string; fileId: string; replyId: string; aCommentReply : TCommentReply) : TCommentReply;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'files/{fileId}/comments/{commentId}/replies/{replyId}';
  _Methodid   = 'drive.replies.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['commentId',commentId,'fileId',fileId,'replyId',replyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCommentReply,TCommentReply) as TCommentReply;
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

Function TRevisionsResource.Get(fileId: string; revisionId: string) : TRevision;

Const
  _HTTPMethod = 'GET';
  _Path       = 'files/{fileId}/revisions/{revisionId}';
  _Methodid   = 'drive.revisions.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId,'revisionId',revisionId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TRevision) as TRevision;
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

Function TRevisionsResource.Patch(fileId: string; revisionId: string; aRevision : TRevision) : TRevision;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'files/{fileId}/revisions/{revisionId}';
  _Methodid   = 'drive.revisions.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['fileId',fileId,'revisionId',revisionId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aRevision,TRevision) as TRevision;
end;

Function TRevisionsResource.Update(fileId: string; revisionId: string; aRevision : TRevision) : TRevision;

Const
  _HTTPMethod = 'PUT';
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
  Result:='v2';
end;

Class Function TDriveAPI.APIRevision : String;

begin
  Result:='20150326';
end;

Class Function TDriveAPI.APIID : String;

begin
  Result:='drive:v2';
end;

Class Function TDriveAPI.APITitle : String;

begin
  Result:='Drive API';
end;

Class Function TDriveAPI.APIDescription : String;

begin
  Result:='The API to interact with Drive.';
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
  Result:='/drive/v2/';
end;

Class Function TDriveAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/drive/v2/';
end;

Class Function TDriveAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TDriveAPI.APIservicePath : string;

begin
  Result:='drive/v2/';
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
  Result[2].Name:='https://www.googleapis.com/auth/drive.apps.readonly';
  Result[2].Description:='View your Google Drive apps';
  Result[3].Name:='https://www.googleapis.com/auth/drive.file';
  Result[3].Description:='View and manage Google Drive files that you have opened or created with this app';
  Result[4].Name:='https://www.googleapis.com/auth/drive.metadata';
  Result[4].Description:='View and manage metadata of files in your Google Drive';
  Result[5].Name:='https://www.googleapis.com/auth/drive.metadata.readonly';
  Result[5].Description:='View metadata for files in your Google Drive';
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
  TAbout.RegisterObject;
  TAboutadditionalRoleInfo.RegisterObject;
  TAboutadditionalRoleInforoleSets.RegisterObject;
  TAboutadditionalRoleInforoleSetsadditionalRoles.RegisterObject;
  TAboutexportFormats.RegisterObject;
  TAboutexportFormatstargets.RegisterObject;
  TAboutfeatures.RegisterObject;
  TAboutfolderColorPalette.RegisterObject;
  TAboutimportFormats.RegisterObject;
  TAboutimportFormatstargets.RegisterObject;
  TAboutmaxUploadSizes.RegisterObject;
  TAboutquotaBytesByService.RegisterObject;
  TApp.RegisterObject;
  TAppicons.RegisterObject;
  TAppprimaryFileExtensions.RegisterObject;
  TAppprimaryMimeTypes.RegisterObject;
  TAppsecondaryFileExtensions.RegisterObject;
  TAppsecondaryMimeTypes.RegisterObject;
  TAppList.RegisterObject;
  TAppListdefaultAppIds.RegisterObject;
  TAppListitems.RegisterObject;
  TChange.RegisterObject;
  TChangeList.RegisterObject;
  TChangeListitems.RegisterObject;
  TChannel.RegisterObject;
  TChannelparams.RegisterObject;
  TChildList.RegisterObject;
  TChildListitems.RegisterObject;
  TChildReference.RegisterObject;
  TComment.RegisterObject;
  TCommentcontext.RegisterObject;
  TCommentreplies.RegisterObject;
  TCommentList.RegisterObject;
  TCommentListitems.RegisterObject;
  TCommentReply.RegisterObject;
  TCommentReplyList.RegisterObject;
  TCommentReplyListitems.RegisterObject;
  TFile.RegisterObject;
  TFileexportLinks.RegisterObject;
  TFileimageMediaMetadata.RegisterObject;
  TFileimageMediaMetadatalocation.RegisterObject;
  TFileindexableText.RegisterObject;
  TFilelabels.RegisterObject;
  TFileopenWithLinks.RegisterObject;
  TFileownerNames.RegisterObject;
  TFileowners.RegisterObject;
  TFileparents.RegisterObject;
  TFilepermissions.RegisterObject;
  TFileproperties.RegisterObject;
  TFilethumbnail.RegisterObject;
  TFilevideoMediaMetadata.RegisterObject;
  TFileList.RegisterObject;
  TFileListitems.RegisterObject;
  TParentList.RegisterObject;
  TParentListitems.RegisterObject;
  TParentReference.RegisterObject;
  TPermission.RegisterObject;
  TPermissionadditionalRoles.RegisterObject;
  TPermissionId.RegisterObject;
  TPermissionList.RegisterObject;
  TPermissionListitems.RegisterObject;
  TProperty.RegisterObject;
  TPropertyList.RegisterObject;
  TPropertyListitems.RegisterObject;
  TRevision.RegisterObject;
  TRevisionexportLinks.RegisterObject;
  TRevisionList.RegisterObject;
  TRevisionListitems.RegisterObject;
  TUser.RegisterObject;
  TUserpicture.RegisterObject;
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
  Result.API:=Self;
end;



Function TDriveAPI.GetAppsInstance : TAppsResource;

begin
  if (FAppsInstance=Nil) then
    FAppsInstance:=CreateAppsResource;
  Result:=FAppsInstance;
end;

Function TDriveAPI.CreateAppsResource : TAppsResource;

begin
  Result:=CreateAppsResource(Self);
end;


Function TDriveAPI.CreateAppsResource(AOwner : TComponent) : TAppsResource;

begin
  Result:=TAppsResource.Create(AOwner);
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
end;



Function TDriveAPI.GetChildrenInstance : TChildrenResource;

begin
  if (FChildrenInstance=Nil) then
    FChildrenInstance:=CreateChildrenResource;
  Result:=FChildrenInstance;
end;

Function TDriveAPI.CreateChildrenResource : TChildrenResource;

begin
  Result:=CreateChildrenResource(Self);
end;


Function TDriveAPI.CreateChildrenResource(AOwner : TComponent) : TChildrenResource;

begin
  Result:=TChildrenResource.Create(AOwner);
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
end;



Function TDriveAPI.GetParentsInstance : TParentsResource;

begin
  if (FParentsInstance=Nil) then
    FParentsInstance:=CreateParentsResource;
  Result:=FParentsInstance;
end;

Function TDriveAPI.CreateParentsResource : TParentsResource;

begin
  Result:=CreateParentsResource(Self);
end;


Function TDriveAPI.CreateParentsResource(AOwner : TComponent) : TParentsResource;

begin
  Result:=TParentsResource.Create(AOwner);
  Result.API:=Self;
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
  Result.API:=Self;
end;



Function TDriveAPI.GetPropertiesInstance : TPropertiesResource;

begin
  if (FPropertiesInstance=Nil) then
    FPropertiesInstance:=CreatePropertiesResource;
  Result:=FPropertiesInstance;
end;

Function TDriveAPI.CreatePropertiesResource : TPropertiesResource;

begin
  Result:=CreatePropertiesResource(Self);
end;


Function TDriveAPI.CreatePropertiesResource(AOwner : TComponent) : TPropertiesResource;

begin
  Result:=TPropertiesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDriveAPI.GetRealtimeInstance : TRealtimeResource;

begin
  if (FRealtimeInstance=Nil) then
    FRealtimeInstance:=CreateRealtimeResource;
  Result:=FRealtimeInstance;
end;

Function TDriveAPI.CreateRealtimeResource : TRealtimeResource;

begin
  Result:=CreateRealtimeResource(Self);
end;


Function TDriveAPI.CreateRealtimeResource(AOwner : TComponent) : TRealtimeResource;

begin
  Result:=TRealtimeResource.Create(AOwner);
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
end;



initialization
  TDriveAPI.RegisterAPI;
end.
