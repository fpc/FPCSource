{ -----------------------------------------------------------------------
  Do not edit !
  
  This file was automatically generated on 2026-10-03 10:20.
  Used command-line parameters:
     -s drive -o drive -q
  Source OpenAPI document data:
    Title: Google Drive API
    Version: v3
  -----------------------------------------------------------------------}
unit drive.Service.Intf;

{$mode objfpc}
{$h+}
{$modeswitch advancedrecords}

interface

uses
   SysUtils, classes, fpopenapiclient, drive.Dto;

Type
  // Complex response result types
  
  TCommentsDeleteResponseKind = (
    CommentsDeleterkSuccess,
    CommentsDeleterkUnexpected
  );
  
  TCommentsDeleteResult = record
    public
    FStatusCode: Integer;
    FStatusText: String;
    FContentType: String;
    FResponseKind: TCommentsDeleteResponseKind;
    FRawContent: String;
    FRawStream: TStream;
    property StatusCode: Integer read FStatusCode;
    property StatusText: String read FStatusText;
    property ContentType: String read FContentType;
    property ResponseKind: TCommentsDeleteResponseKind read FResponseKind;
    property RawContent: String read FRawContent;
    property RawStream: TStream read FRawStream;
    function Success: Boolean; inline;
    function IsClientError: Boolean; inline;
    function IsServerError: Boolean; inline;
    procedure Clear;
    function ExtractRawStream: TStream;
  end;
  
  TDrivesDeleteResponseKind = (
    DrivesDeleterkSuccess,
    DrivesDeleterkUnexpected
  );
  
  TDrivesDeleteResult = record
    public
    FStatusCode: Integer;
    FStatusText: String;
    FContentType: String;
    FResponseKind: TDrivesDeleteResponseKind;
    FRawContent: String;
    FRawStream: TStream;
    property StatusCode: Integer read FStatusCode;
    property StatusText: String read FStatusText;
    property ContentType: String read FContentType;
    property ResponseKind: TDrivesDeleteResponseKind read FResponseKind;
    property RawContent: String read FRawContent;
    property RawStream: TStream read FRawStream;
    function Success: Boolean; inline;
    function IsClientError: Boolean; inline;
    function IsServerError: Boolean; inline;
    procedure Clear;
    function ExtractRawStream: TStream;
  end;
  
  TFilesDeleteResponseKind = (
    FilesDeleterkSuccess,
    FilesDeleterkUnexpected
  );
  
  TFilesDeleteResult = record
    public
    FStatusCode: Integer;
    FStatusText: String;
    FContentType: String;
    FResponseKind: TFilesDeleteResponseKind;
    FRawContent: String;
    FRawStream: TStream;
    property StatusCode: Integer read FStatusCode;
    property StatusText: String read FStatusText;
    property ContentType: String read FContentType;
    property ResponseKind: TFilesDeleteResponseKind read FResponseKind;
    property RawContent: String read FRawContent;
    property RawStream: TStream read FRawStream;
    function Success: Boolean; inline;
    function IsClientError: Boolean; inline;
    function IsServerError: Boolean; inline;
    procedure Clear;
    function ExtractRawStream: TStream;
  end;
  
  TFilesEmptyTrashResponseKind = (
    FilesEmptyTrashrkSuccess,
    FilesEmptyTrashrkUnexpected
  );
  
  TFilesEmptyTrashResult = record
    public
    FStatusCode: Integer;
    FStatusText: String;
    FContentType: String;
    FResponseKind: TFilesEmptyTrashResponseKind;
    FRawContent: String;
    FRawStream: TStream;
    property StatusCode: Integer read FStatusCode;
    property StatusText: String read FStatusText;
    property ContentType: String read FContentType;
    property ResponseKind: TFilesEmptyTrashResponseKind read FResponseKind;
    property RawContent: String read FRawContent;
    property RawStream: TStream read FRawStream;
    function Success: Boolean; inline;
    function IsClientError: Boolean; inline;
    function IsServerError: Boolean; inline;
    procedure Clear;
    function ExtractRawStream: TStream;
  end;
  
  TPermissionsDeleteResponseKind = (
    PermissionsDeleterkSuccess,
    PermissionsDeleterkUnexpected
  );
  
  TPermissionsDeleteResult = record
    public
    FStatusCode: Integer;
    FStatusText: String;
    FContentType: String;
    FResponseKind: TPermissionsDeleteResponseKind;
    FRawContent: String;
    FRawStream: TStream;
    property StatusCode: Integer read FStatusCode;
    property StatusText: String read FStatusText;
    property ContentType: String read FContentType;
    property ResponseKind: TPermissionsDeleteResponseKind read FResponseKind;
    property RawContent: String read FRawContent;
    property RawStream: TStream read FRawStream;
    function Success: Boolean; inline;
    function IsClientError: Boolean; inline;
    function IsServerError: Boolean; inline;
    procedure Clear;
    function ExtractRawStream: TStream;
  end;
  
  TRepliesDeleteResponseKind = (
    RepliesDeleterkSuccess,
    RepliesDeleterkUnexpected
  );
  
  TRepliesDeleteResult = record
    public
    FStatusCode: Integer;
    FStatusText: String;
    FContentType: String;
    FResponseKind: TRepliesDeleteResponseKind;
    FRawContent: String;
    FRawStream: TStream;
    property StatusCode: Integer read FStatusCode;
    property StatusText: String read FStatusText;
    property ContentType: String read FContentType;
    property ResponseKind: TRepliesDeleteResponseKind read FResponseKind;
    property RawContent: String read FRawContent;
    property RawStream: TStream read FRawStream;
    function Success: Boolean; inline;
    function IsClientError: Boolean; inline;
    function IsServerError: Boolean; inline;
    procedure Clear;
    function ExtractRawStream: TStream;
  end;
  
  TRevisionsDeleteResponseKind = (
    RevisionsDeleterkSuccess,
    RevisionsDeleterkUnexpected
  );
  
  TRevisionsDeleteResult = record
    public
    FStatusCode: Integer;
    FStatusText: String;
    FContentType: String;
    FResponseKind: TRevisionsDeleteResponseKind;
    FRawContent: String;
    FRawStream: TStream;
    property StatusCode: Integer read FStatusCode;
    property StatusText: String read FStatusText;
    property ContentType: String read FContentType;
    property ResponseKind: TRevisionsDeleteResponseKind read FResponseKind;
    property RawContent: String read FRawContent;
    property RawStream: TStream read FRawStream;
    function Success: Boolean; inline;
    function IsClientError: Boolean; inline;
    function IsServerError: Boolean; inline;
    procedure Clear;
    function ExtractRawStream: TStream;
  end;
  
  TTeamdrivesDeleteResponseKind = (
    TeamdrivesDeleterkSuccess,
    TeamdrivesDeleterkUnexpected
  );
  
  TTeamdrivesDeleteResult = record
    public
    FStatusCode: Integer;
    FStatusText: String;
    FContentType: String;
    FResponseKind: TTeamdrivesDeleteResponseKind;
    FRawContent: String;
    FRawStream: TStream;
    property StatusCode: Integer read FStatusCode;
    property StatusText: String read FStatusText;
    property ContentType: String read FContentType;
    property ResponseKind: TTeamdrivesDeleteResponseKind read FResponseKind;
    property RawContent: String read FRawContent;
    property RawStream: TStream read FRawStream;
    function Success: Boolean; inline;
    function IsClientError: Boolean; inline;
    function IsServerError: Boolean; inline;
    procedure Clear;
    function ExtractRawStream: TStream;
  end;
  
  // Service result types
  TAboutServiceResult = specialize TServiceResult<TAbout>;
  TAccessProposalServiceResult = specialize TServiceResult<TAccessProposal>;
  TAppListServiceResult = specialize TServiceResult<TAppList>;
  TApprovalListServiceResult = specialize TServiceResult<TApprovalList>;
  TApprovalServiceResult = specialize TServiceResult<TApproval>;
  TAppServiceResult = specialize TServiceResult<TApp>;
  TChangeListServiceResult = specialize TServiceResult<TChangeList>;
  TChannelServiceResult = specialize TServiceResult<TChannel>;
  TCommentListServiceResult = specialize TServiceResult<TCommentList>;
  TCommentServiceResult = specialize TServiceResult<TComment>;
  TDriveListServiceResult = specialize TServiceResult<TDriveList>;
  TDriveServiceResult = specialize TServiceResult<TDrive>;
  TFileListServiceResult = specialize TServiceResult<TFileList>;
  TFileServiceResult = specialize TServiceResult<TFile>;
  TGenerateCseTokenResponseServiceResult = specialize TServiceResult<TGenerateCseTokenResponse>;
  TGeneratedIdsServiceResult = specialize TServiceResult<TGeneratedIds>;
  TLabelListServiceResult = specialize TServiceResult<TLabelList>;
  TListAccessProposalsResponseServiceResult = specialize TServiceResult<TListAccessProposalsResponse>;
  TModifyLabelsResponseServiceResult = specialize TServiceResult<TModifyLabelsResponse>;
  TOperationServiceResult = specialize TServiceResult<TOperation>;
  TPermissionListServiceResult = specialize TServiceResult<TPermissionList>;
  TPermissionServiceResult = specialize TServiceResult<TPermission>;
  TReplyListServiceResult = specialize TServiceResult<TReplyList>;
  TReplyServiceResult = specialize TServiceResult<TReply>;
  TRevisionListServiceResult = specialize TServiceResult<TRevisionList>;
  TRevisionServiceResult = specialize TServiceResult<TRevision>;
  TStartPageTokenServiceResult = specialize TServiceResult<TStartPageToken>;
  TTeamDriveListServiceResult = specialize TServiceResult<TTeamDriveList>;
  TTeamDriveServiceResult = specialize TServiceResult<TTeamDrive>;
  
  // Service IAbout
  
  IAbout = interface  ['{4D2B0A05-51A8-4E7C-A651-3D0DB67EF942}']
    Function Get() : TAboutServiceResult;
  end;
  
  // Service IAccessproposals
  
  IAccessproposals = interface  ['{ADAA010B-83A7-4909-997E-E148D98428E6}']
    Function Get(aFileId : string; aProposalId : string) : TAccessProposalServiceResult;
    Function List(aFileId : string; aPageSize : integer; aPageToken : string) : TListAccessProposalsResponseServiceResult;
  end;
  
  // Service IApprovals
  
  IApprovals = interface  ['{DC5A2F3C-749A-451B-93EF-15BB10A2CFBD}']
    Function Approve(aApprovalId : string; aFileId : string; aRequest : TApproveApprovalRequest) : TApprovalServiceResult;
    Function Cancel(aApprovalId : string; aFileId : string; aRequest : TCancelApprovalRequest) : TApprovalServiceResult;
    Function Comment(aApprovalId : string; aFileId : string; aRequest : TCommentApprovalRequest) : TApprovalServiceResult;
    Function Decline(aApprovalId : string; aFileId : string; aRequest : TDeclineApprovalRequest) : TApprovalServiceResult;
    Function Get(aApprovalId : string; aFileId : string) : TApprovalServiceResult;
    Function List(aFileId : string; aPageSize : integer; aPageToken : string) : TApprovalListServiceResult;
    Function Reassign(aApprovalId : string; aFileId : string; aRequest : TReassignApprovalRequest) : TApprovalServiceResult;
    Function Start(aFileId : string; aRequest : TStartApprovalRequest) : TApprovalServiceResult;
  end;
  
  // Service IApps
  
  IApps = interface  ['{50EF29CF-EB5A-4828-ABFF-26059EEB532A}']
    Function Get(aAppId : string) : TAppServiceResult;
    Function List(aAppFilterExtensions : string; aAppFilterMimeTypes : string; aLanguageCode : string) : TAppListServiceResult;
  end;
  
  // Service IChanges
  
  IChanges = interface  ['{153BEF19-93BF-4609-986B-18BB4E8D8AC2}']
    Function GetStartPageToken(aDriveId : string; aTeamDriveId : string; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false) : TStartPageTokenServiceResult;
    Function List(aDriveId : string; aIncludeLabels : string; aIncludePermissionsForView : string; aPageToken : string; aTeamDriveId : string; aIncludeCorpusRemovals : boolean = false; aIncludeItemsFromAllDrives : boolean = false; aIncludeRemoved : boolean = true; aIncludeTeamDriveItems : boolean = false; aPageSize : integer = 100; aRestrictToMyDrive : boolean = false; aSpaces : string = 'drive'; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false) : TChangeListServiceResult;
    Function Watch(aDriveId : string; aIncludeLabels : string; aIncludePermissionsForView : string; aPageToken : string; aTeamDriveId : string; aRequest : TChannel; aIncludeCorpusRemovals : boolean = false; aIncludeItemsFromAllDrives : boolean = false; aIncludeRemoved : boolean = true; aIncludeTeamDriveItems : boolean = false; aPageSize : integer = 100; aRestrictToMyDrive : boolean = false; aSpaces : string = 'drive'; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false) : TChannelServiceResult;
  end;
  
  // Service IComments
  
  IComments = interface  ['{B52AA2B7-AC03-4DB4-BA25-EBFDFA0E7B1A}']
    Function Create_(aFileId : string; aRequest : TComment) : TCommentServiceResult;
    Function Delete(aCommentId : string; aFileId : string) : TCommentsDeleteResult;
    Function Get(aCommentId : string; aFileId : string; aIncludeDeleted : boolean = false) : TCommentServiceResult;
    Function List(aFileId : string; aPageToken : string; aStartModifiedTime : string; aIncludeDeleted : boolean = false; aPageSize : integer = 20) : TCommentListServiceResult;
    Function Update(aCommentId : string; aFileId : string; aRequest : TComment) : TCommentServiceResult;
  end;
  
  // Service IDrives
  
  IDrives = interface  ['{52236C02-2614-48C3-B43A-F759B3BEB515}']
    Function Create_(aRequestId : string; aRequest : TDrive) : TDriveServiceResult;
    Function Delete(aDriveId : string; aAllowItemDeletion : boolean = false; aUseDomainAdminAccess : boolean = false) : TDrivesDeleteResult;
    Function Get(aDriveId : string; aUseDomainAdminAccess : boolean = false) : TDriveServiceResult;
    Function Hide(aDriveId : string) : TDriveServiceResult;
    Function List(aPageToken : string; aQ : string; aPageSize : integer = 10; aUseDomainAdminAccess : boolean = false) : TDriveListServiceResult;
    Function Unhide(aDriveId : string) : TDriveServiceResult;
    Function Update(aDriveId : string; aRequest : TDrive; aUseDomainAdminAccess : boolean = false) : TDriveServiceResult;
  end;
  
  // Service IFiles
  
  IFiles = interface  ['{8BD6C5AA-7F5B-420B-9EEE-227CF431DCDD}']
    Function Copy(aFileId : string; aIncludeLabels : string; aIncludePermissionsForView : string; aOcrLanguage : string; aRequest : TFile; aCopyComments : boolean = false; aEnforceSingleParent : boolean = false; aIgnoreDefaultVisibility : boolean = false; aKeepRevisionForever : boolean = false; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false) : TFileServiceResult;
    Function Create_(aIncludeLabels : string; aIncludePermissionsForView : string; aOcrLanguage : string; aRequest : TFile; aEnforceSingleParent : boolean = false; aIgnoreDefaultVisibility : boolean = false; aKeepRevisionForever : boolean = false; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false; aUseContentAsIndexableText : boolean = false) : TFileServiceResult;
    Function Delete(aFileId : string; aEnforceSingleParent : boolean = false; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false) : TFilesDeleteResult;
    Function Download(aFileId : string; aMimeType : string; aRevisionId : string) : TOperationServiceResult;
    Function EmptyTrash(aDriveId : string; aEnforceSingleParent : boolean = false) : TFilesEmptyTrashResult;
    Function GenerateCseToken(aFileId : string; aParent : string) : TGenerateCseTokenResponseServiceResult;
    Function GenerateIds(aCount : integer = 10; aSpace : string = 'drive'; aType : string = 'files') : TGeneratedIdsServiceResult;
    Function Get(aFileId : string; aIncludeLabels : string; aIncludePermissionsForView : string; aAcknowledgeAbuse : boolean = false; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false) : TFileServiceResult;
    Function List(aCorpora : string; aCorpus : string; aDriveId : string; aIncludeLabels : string; aIncludePermissionsForView : string; aOrderBy : string; aPageToken : string; aQ : string; aTeamDriveId : string; aIncludeItemsFromAllDrives : boolean = false; aIncludeTeamDriveItems : boolean = false; aPageSize : integer = 100; aSpaces : string = 'drive'; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false) : TFileListServiceResult;
    Function ListLabels(aFileId : string; aPageToken : string; aMaxResults : integer = 100) : TLabelListServiceResult;
    Function ModifyLabels(aFileId : string; aRequest : TModifyLabelsRequest) : TModifyLabelsResponseServiceResult;
    Function Update(aAddParents : string; aFileId : string; aIncludeLabels : string; aIncludePermissionsForView : string; aOcrLanguage : string; aRemoveParents : string; aRequest : TFile; aEnforceSingleParent : boolean = false; aKeepRevisionForever : boolean = false; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false; aUseContentAsIndexableText : boolean = false) : TFileServiceResult;
    Function Watch(aFileId : string; aIncludeLabels : string; aIncludePermissionsForView : string; aRequest : TChannel; aAcknowledgeAbuse : boolean = false; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false) : TChannelServiceResult;
  end;
  
  // Service IOperations
  
  IOperations = interface  ['{72851704-FC93-4742-8F9A-4CA0AA86E5DD}']
    Function Get(aName : string) : TOperationServiceResult;
  end;
  
  // Service IPermissions
  
  IPermissions = interface  ['{642C70E2-6050-4611-8452-E54A421F4100}']
    Function Create_(aEmailMessage : string; aFileId : string; aSendNotificationEmail : boolean; aRequest : TPermission; aEnforceExpansiveAccess : boolean = false; aEnforceSingleParent : boolean = false; aMoveToNewOwnersRoot : boolean = false; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false; aTransferOwnership : boolean = false; aUseDomainAdminAccess : boolean = false) : TPermissionServiceResult;
    Function Delete(aFileId : string; aPermissionId : string; aEnforceExpansiveAccess : boolean = false; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false; aUseDomainAdminAccess : boolean = false) : TPermissionsDeleteResult;
    Function Get(aFileId : string; aPermissionId : string; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false; aUseDomainAdminAccess : boolean = false) : TPermissionServiceResult;
    Function List(aFileId : string; aIncludePermissionsForView : string; aPageSize : integer; aPageToken : string; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false; aUseDomainAdminAccess : boolean = false) : TPermissionListServiceResult;
    Function Update(aFileId : string; aPermissionId : string; aRequest : TPermission; aEnforceExpansiveAccess : boolean = false; aRemoveExpiration : boolean = false; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false; aTransferOwnership : boolean = false; aUseDomainAdminAccess : boolean = false) : TPermissionServiceResult;
  end;
  
  // Service IReplies
  
  IReplies = interface  ['{3D202DF5-397E-4EB7-A78B-8247E4B786EE}']
    Function Create_(aCommentId : string; aFileId : string; aRequest : TReply) : TReplyServiceResult;
    Function Delete(aCommentId : string; aFileId : string; aReplyId : string) : TRepliesDeleteResult;
    Function Get(aCommentId : string; aFileId : string; aReplyId : string; aIncludeDeleted : boolean = false) : TReplyServiceResult;
    Function List(aCommentId : string; aFileId : string; aPageToken : string; aIncludeDeleted : boolean = false; aPageSize : integer = 20) : TReplyListServiceResult;
    Function Update(aCommentId : string; aFileId : string; aReplyId : string; aRequest : TReply) : TReplyServiceResult;
  end;
  
  // Service IRevisions
  
  IRevisions = interface  ['{9324DBD3-586F-4551-8579-F8B6D16DCF20}']
    Function Delete(aFileId : string; aRevisionId : string) : TRevisionsDeleteResult;
    Function Get(aFileId : string; aRevisionId : string; aAcknowledgeAbuse : boolean = false) : TRevisionServiceResult;
    Function List(aFileId : string; aPageToken : string; aPageSize : integer = 200) : TRevisionListServiceResult;
    Function Update(aFileId : string; aRevisionId : string; aRequest : TRevision) : TRevisionServiceResult;
  end;
  
  // Service ITeamdrives
  
  ITeamdrives = interface  ['{1A0BF13B-D3ED-44BD-B3F8-35ECFA9F4FA2}']
    Function Create_(aRequestId : string; aRequest : TTeamDrive) : TTeamDriveServiceResult;
    Function Delete(aTeamDriveId : string) : TTeamdrivesDeleteResult;
    Function Get(aTeamDriveId : string; aUseDomainAdminAccess : boolean = false) : TTeamDriveServiceResult;
    Function List(aPageToken : string; aQ : string; aPageSize : integer = 10; aUseDomainAdminAccess : boolean = false) : TTeamDriveListServiceResult;
    Function Update(aTeamDriveId : string; aRequest : TTeamDrive; aUseDomainAdminAccess : boolean = false) : TTeamDriveServiceResult;
  end;
  

implementation

function TCommentsDeleteResult.Success: Boolean;
begin
  Result:=(FStatusCode >= 200) and (FStatusCode < 300);
end;

function TCommentsDeleteResult.IsClientError: Boolean;
begin
  Result:=(FStatusCode >= 400) and (FStatusCode < 500);
end;

function TCommentsDeleteResult.IsServerError: Boolean;
begin
  Result:=(FStatusCode >= 500) and (FStatusCode < 600);
end;

procedure TCommentsDeleteResult.Clear;
begin
  FStatusCode:=0;
  FStatusText:='';
  FContentType:='';
  FResponseKind:=Default(TCommentsDeleteResponseKind);
  FRawContent:='';
  FreeAndNil(FRawStream);
end;

function TCommentsDeleteResult.ExtractRawStream: TStream;
begin
  Result:=FRawStream;
  FRawStream:=nil;
end;

function TDrivesDeleteResult.Success: Boolean;
begin
  Result:=(FStatusCode >= 200) and (FStatusCode < 300);
end;

function TDrivesDeleteResult.IsClientError: Boolean;
begin
  Result:=(FStatusCode >= 400) and (FStatusCode < 500);
end;

function TDrivesDeleteResult.IsServerError: Boolean;
begin
  Result:=(FStatusCode >= 500) and (FStatusCode < 600);
end;

procedure TDrivesDeleteResult.Clear;
begin
  FStatusCode:=0;
  FStatusText:='';
  FContentType:='';
  FResponseKind:=Default(TDrivesDeleteResponseKind);
  FRawContent:='';
  FreeAndNil(FRawStream);
end;

function TDrivesDeleteResult.ExtractRawStream: TStream;
begin
  Result:=FRawStream;
  FRawStream:=nil;
end;

function TFilesDeleteResult.Success: Boolean;
begin
  Result:=(FStatusCode >= 200) and (FStatusCode < 300);
end;

function TFilesDeleteResult.IsClientError: Boolean;
begin
  Result:=(FStatusCode >= 400) and (FStatusCode < 500);
end;

function TFilesDeleteResult.IsServerError: Boolean;
begin
  Result:=(FStatusCode >= 500) and (FStatusCode < 600);
end;

procedure TFilesDeleteResult.Clear;
begin
  FStatusCode:=0;
  FStatusText:='';
  FContentType:='';
  FResponseKind:=Default(TFilesDeleteResponseKind);
  FRawContent:='';
  FreeAndNil(FRawStream);
end;

function TFilesDeleteResult.ExtractRawStream: TStream;
begin
  Result:=FRawStream;
  FRawStream:=nil;
end;

function TFilesEmptyTrashResult.Success: Boolean;
begin
  Result:=(FStatusCode >= 200) and (FStatusCode < 300);
end;

function TFilesEmptyTrashResult.IsClientError: Boolean;
begin
  Result:=(FStatusCode >= 400) and (FStatusCode < 500);
end;

function TFilesEmptyTrashResult.IsServerError: Boolean;
begin
  Result:=(FStatusCode >= 500) and (FStatusCode < 600);
end;

procedure TFilesEmptyTrashResult.Clear;
begin
  FStatusCode:=0;
  FStatusText:='';
  FContentType:='';
  FResponseKind:=Default(TFilesEmptyTrashResponseKind);
  FRawContent:='';
  FreeAndNil(FRawStream);
end;

function TFilesEmptyTrashResult.ExtractRawStream: TStream;
begin
  Result:=FRawStream;
  FRawStream:=nil;
end;

function TPermissionsDeleteResult.Success: Boolean;
begin
  Result:=(FStatusCode >= 200) and (FStatusCode < 300);
end;

function TPermissionsDeleteResult.IsClientError: Boolean;
begin
  Result:=(FStatusCode >= 400) and (FStatusCode < 500);
end;

function TPermissionsDeleteResult.IsServerError: Boolean;
begin
  Result:=(FStatusCode >= 500) and (FStatusCode < 600);
end;

procedure TPermissionsDeleteResult.Clear;
begin
  FStatusCode:=0;
  FStatusText:='';
  FContentType:='';
  FResponseKind:=Default(TPermissionsDeleteResponseKind);
  FRawContent:='';
  FreeAndNil(FRawStream);
end;

function TPermissionsDeleteResult.ExtractRawStream: TStream;
begin
  Result:=FRawStream;
  FRawStream:=nil;
end;

function TRepliesDeleteResult.Success: Boolean;
begin
  Result:=(FStatusCode >= 200) and (FStatusCode < 300);
end;

function TRepliesDeleteResult.IsClientError: Boolean;
begin
  Result:=(FStatusCode >= 400) and (FStatusCode < 500);
end;

function TRepliesDeleteResult.IsServerError: Boolean;
begin
  Result:=(FStatusCode >= 500) and (FStatusCode < 600);
end;

procedure TRepliesDeleteResult.Clear;
begin
  FStatusCode:=0;
  FStatusText:='';
  FContentType:='';
  FResponseKind:=Default(TRepliesDeleteResponseKind);
  FRawContent:='';
  FreeAndNil(FRawStream);
end;

function TRepliesDeleteResult.ExtractRawStream: TStream;
begin
  Result:=FRawStream;
  FRawStream:=nil;
end;

function TRevisionsDeleteResult.Success: Boolean;
begin
  Result:=(FStatusCode >= 200) and (FStatusCode < 300);
end;

function TRevisionsDeleteResult.IsClientError: Boolean;
begin
  Result:=(FStatusCode >= 400) and (FStatusCode < 500);
end;

function TRevisionsDeleteResult.IsServerError: Boolean;
begin
  Result:=(FStatusCode >= 500) and (FStatusCode < 600);
end;

procedure TRevisionsDeleteResult.Clear;
begin
  FStatusCode:=0;
  FStatusText:='';
  FContentType:='';
  FResponseKind:=Default(TRevisionsDeleteResponseKind);
  FRawContent:='';
  FreeAndNil(FRawStream);
end;

function TRevisionsDeleteResult.ExtractRawStream: TStream;
begin
  Result:=FRawStream;
  FRawStream:=nil;
end;

function TTeamdrivesDeleteResult.Success: Boolean;
begin
  Result:=(FStatusCode >= 200) and (FStatusCode < 300);
end;

function TTeamdrivesDeleteResult.IsClientError: Boolean;
begin
  Result:=(FStatusCode >= 400) and (FStatusCode < 500);
end;

function TTeamdrivesDeleteResult.IsServerError: Boolean;
begin
  Result:=(FStatusCode >= 500) and (FStatusCode < 600);
end;

procedure TTeamdrivesDeleteResult.Clear;
begin
  FStatusCode:=0;
  FStatusText:='';
  FContentType:='';
  FResponseKind:=Default(TTeamdrivesDeleteResponseKind);
  FRawContent:='';
  FreeAndNil(FRawStream);
end;

function TTeamdrivesDeleteResult.ExtractRawStream: TStream;
begin
  Result:=FRawStream;
  FRawStream:=nil;
end;

end.
