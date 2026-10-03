{ -----------------------------------------------------------------------
  Do not edit !
  
  This file was automatically generated on 2026-10-03 10:20.
  Used command-line parameters:
     -s drive -o drive -q
  Source OpenAPI document data:
    Title: Google Drive API
    Version: v3
  -----------------------------------------------------------------------}
unit drive.Dto;

{$mode objfpc}
{$h+}


interface

uses types;

Type

  TUser = class;
  TAbout = class;
  TAccessProposalRoleAndView = class;
  TAccessProposal = class;
  TAddReviewer = class;
  TAppIcons = class;
  TApp = class;
  TAppList = class;
  TReviewerResponse = class;
  TApproval = class;
  TApprovalList = class;
  TApproveApprovalRequest = class;
  TCancelApprovalRequest = class;
  TDrive = class;
  TDecryptionMetadata = class;
  TClientEncryptionDetails = class;
  TContentRestriction = class;
  TDownloadRestriction = class;
  TDownloadRestrictionsMetadata = class;
  TPermission = class;
  TFile = class;
  TTeamDrive = class;
  TChange = class;
  TChangeList = class;
  TChannel = class;
  TReply = class;
  TComment = class;
  TCommentApprovalRequest = class;
  TCommentList = class;
  TDeclineApprovalRequest = class;
  TDriveList = class;
  TFileList = class;
  TGenerateCseTokenResponse = class;
  TGeneratedIds = class;
  TLabel = class;
  TLabelField = class;
  TLabelFieldModification = class;
  TLabelList = class;
  TLabelModification = class;
  TListAccessProposalsResponse = class;
  TModifyLabelsRequest = class;
  TModifyLabelsResponse = class;
  TStatus = class;
  TOperation = class;
  TPermissionList = class;
  TReplaceReviewer = class;
  TReassignApprovalRequest = class;
  TReplyList = class;
  TResolveAccessProposalRequest = class;
  TRevision = class;
  TRevisionList = class;
  TStartApprovalRequest = class;
  TStartPageToken = class;
  TTeamDriveList = class;
  TAccessProposalRoleAndViewArray = Array of TAccessProposalRoleAndView;
  TAccessProposalArray = Array of TAccessProposal;
  TAddReviewerArray = Array of TAddReviewer;
  TAppIconsArray = Array of TAppIcons;
  TApprovalArray = Array of TApproval;
  TAppArray = Array of TApp;
  TChangeArray = Array of TChange;
  TCommentArray = Array of TComment;
  TContentRestrictionArray = Array of TContentRestriction;
  TDriveArray = Array of TDrive;
  TFileArray = Array of TFile;
  TLabelFieldModificationArray = Array of TLabelFieldModification;
  TLabelModificationArray = Array of TLabelModification;
  TLabelArray = Array of TLabel;
  stringArray = Array of string;
  TPermissionArray = Array of TPermission;
  TReplaceReviewerArray = Array of TReplaceReviewer;
  TReplyArray = Array of TReply;
  TReviewerResponseArray = Array of TReviewerResponse;
  TRevisionArray = Array of TRevision;
  TTeamDriveArray = Array of TTeamDrive;
  TUserArray = Array of TUser;
  
  TUser = Class(TObject)
    displayName : string;
    emailAddress : string;
    kind : string;
    me : boolean;
    permissionId : string;
    photoLink : string;
  end;
  
  TAbout = Class(TObject)
    appInstalled : boolean;
    canCreateDrives : boolean;
    canCreateTeamDrives : boolean;
    driveThemes : stringArray;
    exportFormats : string;
    folderColorPalette : TStringDynArray;
    importFormats : string;
    kind : string;
    maxImportSizes : string;
    maxUploadSize : string;
    storageQuota : string;
    teamDriveThemes : stringArray;
    user : TUser;
    constructor CreateWithMembers;
  end;
  
  TAccessProposalRoleAndView = Class(TObject)
    role : string;
    view : string;
  end;
  
  TAccessProposal = Class(TObject)
    createTime : string;
    fileId : string;
    proposalId : string;
    recipientEmailAddress : string;
    requesterEmailAddress : string;
    requestMessage : string;
    rolesAndViews : TAccessProposalRoleAndViewArray;
  end;
  
  TAddReviewer = Class(TObject)
    addedReviewerEmail : string;
  end;
  
  TAppIcons = Class(TObject)
    category : string;
    iconUrl : string;
    size : integer;
  end;
  
  TApp = Class(TObject)
    authorized : boolean;
    createInFolderTemplate : string;
    createUrl : string;
    hasDriveWideScope : boolean;
    icons : TAppIconsArray;
    id : string;
    installed : boolean;
    kind : string;
    longDescription : string;
    name : string;
    objectType : string;
    openUrlTemplate : string;
    primaryFileExtensions : TStringDynArray;
    primaryMimeTypes : TStringDynArray;
    productId : string;
    productUrl : string;
    secondaryFileExtensions : TStringDynArray;
    secondaryMimeTypes : TStringDynArray;
    shortDescription : string;
    supportsCreate : boolean;
    supportsImport : boolean;
    supportsMultiOpen : boolean;
    supportsOfflineCreate : boolean;
    useByDefault : boolean;
  end;
  
  TAppList = Class(TObject)
    defaultAppIds : TStringDynArray;
    items : TAppArray;
    kind : string;
    selfLink : string;
  end;
  
  TReviewerResponse = Class(TObject)
    kind : string;
    response : string;
    reviewer : TUser;
    constructor CreateWithMembers;
  end;
  
  TApproval = Class(TObject)
    approvalId : string;
    completeTime : string;
    createTime : string;
    dueTime : string;
    fileContentChangeBehavior : string;
    initiator : TUser;
    kind : string;
    modifyTime : string;
    reviewerResponses : TReviewerResponseArray;
    status : string;
    targetFileId : string;
    constructor CreateWithMembers;
  end;
  
  TApprovalList = Class(TObject)
    items : TApprovalArray;
    kind : string;
    nextPageToken : string;
  end;
  
  TApproveApprovalRequest = Class(TObject)
    message : string;
  end;
  
  TCancelApprovalRequest = Class(TObject)
    message : string;
  end;
  
  TDrive = Class(TObject)
    backgroundImageFile : string;
    backgroundImageLink : string;
    capabilities : string;
    colorRgb : string;
    createdTime : TDateTime;
    hidden : boolean;
    id : string;
    kind : string;
    name : string;
    orgUnitId : string;
    restrictions : string;
    themeId : string;
  end;
  
  TDecryptionMetadata = Class(TObject)
    aes256GcmChunkSize : string;
    encryptionResourceKeyHash : string;
    jwt : string;
    kaclsId : string;
    kaclsName : string;
    keyFormat : string;
    wrappedKey : string;
  end;
  
  TClientEncryptionDetails = Class(TObject)
    decryptionMetadata : TDecryptionMetadata;
    encryptionState : string;
    constructor CreateWithMembers;
  end;
  
  TContentRestriction = Class(TObject)
    ownerRestricted : boolean;
    readOnly : boolean;
    reason : string;
    restrictingUser : TUser;
    restrictionTime : TDateTime;
    systemRestricted : boolean;
    type_ : string;
    constructor CreateWithMembers;
  end;
  
  TDownloadRestriction = Class(TObject)
    restrictedForReaders : boolean;
    restrictedForWriters : boolean;
  end;
  
  TDownloadRestrictionsMetadata = Class(TObject)
    effectiveDownloadRestrictionWithContext : TDownloadRestriction;
    itemDownloadRestriction : TDownloadRestriction;
    constructor CreateWithMembers;
  end;
  
  TPermission = Class(TObject)
    allowFileDiscovery : boolean;
    deleted : boolean;
    displayName : string;
    domain : string;
    emailAddress : string;
    expirationTime : TDateTime;
    id : string;
    inheritedPermissionsDisabled : boolean;
    kind : string;
    pendingOwner : boolean;
    permissionDetails : stringArray;
    photoLink : string;
    role : string;
    teamDrivePermissionDetails : stringArray;
    type_ : string;
    view : string;
  end;
  
  TFile = Class(TObject)
    appProperties : string;
    capabilities : string;
    clientEncryptionDetails : TClientEncryptionDetails;
    contentHints : string;
    contentRestrictions : TContentRestrictionArray;
    copyRequiresWriterPermission : boolean;
    createdTime : TDateTime;
    description : string;
    downloadRestrictions : TDownloadRestrictionsMetadata;
    driveId : string;
    explicitlyTrashed : boolean;
    exportLinks : string;
    fileExtension : string;
    folderColorRgb : string;
    fullFileExtension : string;
    hasAugmentedPermissions : boolean;
    hasThumbnail : boolean;
    headRevisionId : string;
    iconLink : string;
    id : string;
    imageMediaMetadata : string;
    inheritedPermissionsDisabled : boolean;
    isAppAuthorized : boolean;
    kind : string;
    labelInfo : string;
    lastModifyingUser : TUser;
    linkShareMetadata : string;
    md5Checksum : string;
    mimeType : string;
    modifiedByMe : boolean;
    modifiedByMeTime : TDateTime;
    modifiedTime : TDateTime;
    name : string;
    originalFilename : string;
    ownedByMe : boolean;
    owners : TUserArray;
    parents : TStringDynArray;
    permissionIds : TStringDynArray;
    permissions : TPermissionArray;
    properties : string;
    quotaBytesUsed : string;
    resourceKey : string;
    sha1Checksum : string;
    sha256Checksum : string;
    shared : boolean;
    sharedWithMeTime : TDateTime;
    sharingUser : TUser;
    shortcutDetails : string;
    size : string;
    spaces : TStringDynArray;
    starred : boolean;
    teamDriveId : string;
    thumbnailLink : string;
    thumbnailVersion : string;
    trashed : boolean;
    trashedTime : TDateTime;
    trashingUser : TUser;
    version : string;
    videoMediaMetadata : string;
    viewedByMe : boolean;
    viewedByMeTime : TDateTime;
    viewersCanCopyContent : boolean;
    webContentLink : string;
    webViewLink : string;
    writersCanShare : boolean;
    constructor CreateWithMembers;
  end;
  
  TTeamDrive = Class(TObject)
    backgroundImageFile : string;
    backgroundImageLink : string;
    capabilities : string;
    colorRgb : string;
    createdTime : TDateTime;
    id : string;
    kind : string;
    name : string;
    orgUnitId : string;
    restrictions : string;
    themeId : string;
  end;
  
  TChange = Class(TObject)
    changeType : string;
    drive : TDrive;
    driveId : string;
    fileId : string;
    file_ : TFile;
    kind : string;
    removed : boolean;
    teamDrive : TTeamDrive;
    teamDriveId : string;
    time : TDateTime;
    type_ : string;
    constructor CreateWithMembers;
  end;
  
  TChangeList = Class(TObject)
    changes : TChangeArray;
    kind : string;
    newStartPageToken : string;
    nextPageToken : string;
  end;
  
  TChannel = Class(TObject)
    address : string;
    expiration : string;
    id : string;
    kind : string;
    params : string;
    payload : boolean;
    resourceId : string;
    resourceUri : string;
    token : string;
    type_ : string;
  end;
  
  TReply = Class(TObject)
    action : string;
    assigneeEmailAddress : string;
    author : TUser;
    content : string;
    createdTime : TDateTime;
    deleted : boolean;
    htmlContent : string;
    id : string;
    kind : string;
    mentionedEmailAddresses : TStringDynArray;
    modifiedTime : TDateTime;
    constructor CreateWithMembers;
  end;
  
  TComment = Class(TObject)
    anchor : string;
    assigneeEmailAddress : string;
    author : TUser;
    content : string;
    createdTime : TDateTime;
    deleted : boolean;
    htmlContent : string;
    id : string;
    kind : string;
    mentionedEmailAddresses : TStringDynArray;
    modifiedTime : TDateTime;
    quotedFileContent : string;
    replies : TReplyArray;
    resolved : boolean;
    constructor CreateWithMembers;
  end;
  
  TCommentApprovalRequest = Class(TObject)
    message : string;
  end;
  
  TCommentList = Class(TObject)
    comments : TCommentArray;
    kind : string;
    nextPageToken : string;
  end;
  
  TDeclineApprovalRequest = Class(TObject)
    message : string;
  end;
  
  TDriveList = Class(TObject)
    drives : TDriveArray;
    kind : string;
    nextPageToken : string;
  end;
  
  TFileList = Class(TObject)
    files : TFileArray;
    incompleteSearch : boolean;
    kind : string;
    nextPageToken : string;
  end;
  
  TGenerateCseTokenResponse = Class(TObject)
    currentKaclsId : string;
    currentKaclsName : string;
    fileId : string;
    jwt : string;
    kind : string;
  end;
  
  TGeneratedIds = Class(TObject)
    ids : TStringDynArray;
    kind : string;
    space : string;
  end;
  
  TLabel = Class(TObject)
    fields : string;
    id : string;
    kind : string;
    revisionId : string;
  end;
  
  TLabelField = Class(TObject)
    dateString : TStringDynArray;
    id : string;
    integer : TStringDynArray;
    kind : string;
    selection : TStringDynArray;
    text : TStringDynArray;
    user : TUserArray;
    valueType : string;
  end;
  
  TLabelFieldModification = Class(TObject)
    fieldId : string;
    kind : string;
    setDateValues : TStringDynArray;
    setIntegerValues : TStringDynArray;
    setSelectionValues : TStringDynArray;
    setTextValues : TStringDynArray;
    setUserValues : TStringDynArray;
    unsetValues : boolean;
  end;
  
  TLabelList = Class(TObject)
    kind : string;
    labels : TLabelArray;
    nextPageToken : string;
  end;
  
  TLabelModification = Class(TObject)
    fieldModifications : TLabelFieldModificationArray;
    kind : string;
    labelId : string;
    removeLabel : boolean;
  end;
  
  TListAccessProposalsResponse = Class(TObject)
    accessProposals : TAccessProposalArray;
    nextPageToken : string;
  end;
  
  TModifyLabelsRequest = Class(TObject)
    kind : string;
    labelModifications : TLabelModificationArray;
  end;
  
  TModifyLabelsResponse = Class(TObject)
    kind : string;
    modifiedLabels : TLabelArray;
  end;
  
  TStatus = Class(TObject)
    code : integer;
    details : stringArray;
    message : string;
  end;
  
  TOperation = Class(TObject)
    done : boolean;
    error : TStatus;
    metadata : string;
    name : string;
    response : string;
    constructor CreateWithMembers;
  end;
  
  TPermissionList = Class(TObject)
    kind : string;
    nextPageToken : string;
    permissions : TPermissionArray;
  end;
  
  TReplaceReviewer = Class(TObject)
    addedReviewerEmail : string;
    removedReviewerEmail : string;
  end;
  
  TReassignApprovalRequest = Class(TObject)
    addReviewers : TAddReviewerArray;
    message : string;
    replaceReviewers : TReplaceReviewerArray;
  end;
  
  TReplyList = Class(TObject)
    kind : string;
    nextPageToken : string;
    replies : TReplyArray;
  end;
  
  TResolveAccessProposalRequest = Class(TObject)
    action : string;
    role : TStringDynArray;
    sendNotification : boolean;
    view : string;
  end;
  
  TRevision = Class(TObject)
    exportLinks : string;
    id : string;
    keepForever : boolean;
    kind : string;
    lastModifyingUser : TUser;
    md5Checksum : string;
    mimeType : string;
    modifiedTime : TDateTime;
    originalFilename : string;
    publishAuto : boolean;
    publishedLink : string;
    publishedOutsideDomain : boolean;
    published_ : boolean;
    size : string;
    constructor CreateWithMembers;
  end;
  
  TRevisionList = Class(TObject)
    kind : string;
    nextPageToken : string;
    revisions : TRevisionArray;
  end;
  
  TStartApprovalRequest = Class(TObject)
    dueTime : string;
    fileContentChangeBehavior : string;
    lockFile : boolean;
    message : string;
    reviewerEmails : TStringDynArray;
  end;
  
  TStartPageToken = Class(TObject)
    kind : string;
    startPageToken : string;
  end;
  
  TTeamDriveList = Class(TObject)
    kind : string;
    nextPageToken : string;
    teamDrives : TTeamDriveArray;
  end;
  
implementation

constructor TAbout.CreateWithMembers;

begin
  user := TUser.Create;
end;

constructor TReviewerResponse.CreateWithMembers;

begin
  reviewer := TUser.Create;
end;

constructor TApproval.CreateWithMembers;

begin
  initiator := TUser.Create;
end;

constructor TClientEncryptionDetails.CreateWithMembers;

begin
  decryptionMetadata := TDecryptionMetadata.Create;
end;

constructor TContentRestriction.CreateWithMembers;

begin
  restrictingUser := TUser.Create;
end;

constructor TDownloadRestrictionsMetadata.CreateWithMembers;

begin
  effectiveDownloadRestrictionWithContext := TDownloadRestriction.Create;
  itemDownloadRestriction := TDownloadRestriction.Create;
end;

constructor TFile.CreateWithMembers;

begin
  clientEncryptionDetails := TClientEncryptionDetails.CreateWithMembers;
  downloadRestrictions := TDownloadRestrictionsMetadata.CreateWithMembers;
  lastModifyingUser := TUser.Create;
  sharingUser := TUser.Create;
  trashingUser := TUser.Create;
end;

constructor TChange.CreateWithMembers;

begin
  drive := TDrive.Create;
  file_ := TFile.CreateWithMembers;
  teamDrive := TTeamDrive.Create;
end;

constructor TReply.CreateWithMembers;

begin
  author := TUser.Create;
end;

constructor TComment.CreateWithMembers;

begin
  author := TUser.Create;
end;

constructor TOperation.CreateWithMembers;

begin
  error := TStatus.Create;
end;

constructor TRevision.CreateWithMembers;

begin
  lastModifyingUser := TUser.Create;
end;

end.
