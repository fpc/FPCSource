{ -----------------------------------------------------------------------
  Do not edit !
  
  This file was automatically generated on 2026-10-03 10:20.
  Used command-line parameters:
     -s drive -o drive -q
  Source OpenAPI document data:
    Title: Google Drive API
    Version: v3
  -----------------------------------------------------------------------}
unit drive.Service.Impl;

{$mode objfpc}
{$h+}

interface

uses
  classes, fpopenapiclient
  , drive.Service.Intf                     // Service definition 
  , drive.Dto;

Type
  // Service IAbout
  
  TAboutProxy = Class (TFPOpenAPIServiceClient,IAbout)
    Function Get() : TAboutServiceResult;
  end;
  
  // Service IAccessproposals
  
  TAccessproposalsProxy = Class (TFPOpenAPIServiceClient,IAccessproposals)
    Function Get(aFileId : string; aProposalId : string) : TAccessProposalServiceResult;
    Function List(aFileId : string; aPageSize : integer; aPageToken : string) : TListAccessProposalsResponseServiceResult;
  end;
  
  // Service IApprovals
  
  TApprovalsProxy = Class (TFPOpenAPIServiceClient,IApprovals)
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
  
  TAppsProxy = Class (TFPOpenAPIServiceClient,IApps)
    Function Get(aAppId : string) : TAppServiceResult;
    Function List(aAppFilterExtensions : string; aAppFilterMimeTypes : string; aLanguageCode : string) : TAppListServiceResult;
  end;
  
  // Service IChanges
  
  TChangesProxy = Class (TFPOpenAPIServiceClient,IChanges)
    Function GetStartPageToken(aDriveId : string; aTeamDriveId : string; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false) : TStartPageTokenServiceResult;
    Function List(aDriveId : string; aIncludeLabels : string; aIncludePermissionsForView : string; aPageToken : string; aTeamDriveId : string; aIncludeCorpusRemovals : boolean = false; aIncludeItemsFromAllDrives : boolean = false; aIncludeRemoved : boolean = true; aIncludeTeamDriveItems : boolean = false; aPageSize : integer = 100; aRestrictToMyDrive : boolean = false; aSpaces : string = 'drive'; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false) : TChangeListServiceResult;
    Function Watch(aDriveId : string; aIncludeLabels : string; aIncludePermissionsForView : string; aPageToken : string; aTeamDriveId : string; aRequest : TChannel; aIncludeCorpusRemovals : boolean = false; aIncludeItemsFromAllDrives : boolean = false; aIncludeRemoved : boolean = true; aIncludeTeamDriveItems : boolean = false; aPageSize : integer = 100; aRestrictToMyDrive : boolean = false; aSpaces : string = 'drive'; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false) : TChannelServiceResult;
  end;
  
  // Service IComments
  
  TCommentsProxy = Class (TFPOpenAPIServiceClient,IComments)
    Function Create_(aFileId : string; aRequest : TComment) : TCommentServiceResult;
    Function Delete(aCommentId : string; aFileId : string) : TCommentsDeleteResult;
    Function Get(aCommentId : string; aFileId : string; aIncludeDeleted : boolean = false) : TCommentServiceResult;
    Function List(aFileId : string; aPageToken : string; aStartModifiedTime : string; aIncludeDeleted : boolean = false; aPageSize : integer = 20) : TCommentListServiceResult;
    Function Update(aCommentId : string; aFileId : string; aRequest : TComment) : TCommentServiceResult;
  end;
  
  // Service IDrives
  
  TDrivesProxy = Class (TFPOpenAPIServiceClient,IDrives)
    Function Create_(aRequestId : string; aRequest : TDrive) : TDriveServiceResult;
    Function Delete(aDriveId : string; aAllowItemDeletion : boolean = false; aUseDomainAdminAccess : boolean = false) : TDrivesDeleteResult;
    Function Get(aDriveId : string; aUseDomainAdminAccess : boolean = false) : TDriveServiceResult;
    Function Hide(aDriveId : string) : TDriveServiceResult;
    Function List(aPageToken : string; aQ : string; aPageSize : integer = 10; aUseDomainAdminAccess : boolean = false) : TDriveListServiceResult;
    Function Unhide(aDriveId : string) : TDriveServiceResult;
    Function Update(aDriveId : string; aRequest : TDrive; aUseDomainAdminAccess : boolean = false) : TDriveServiceResult;
  end;
  
  // Service IFiles
  
  TFilesProxy = Class (TFPOpenAPIServiceClient,IFiles)
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
  
  TOperationsProxy = Class (TFPOpenAPIServiceClient,IOperations)
    Function Get(aName : string) : TOperationServiceResult;
  end;
  
  // Service IPermissions
  
  TPermissionsProxy = Class (TFPOpenAPIServiceClient,IPermissions)
    Function Create_(aEmailMessage : string; aFileId : string; aSendNotificationEmail : boolean; aRequest : TPermission; aEnforceExpansiveAccess : boolean = false; aEnforceSingleParent : boolean = false; aMoveToNewOwnersRoot : boolean = false; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false; aTransferOwnership : boolean = false; aUseDomainAdminAccess : boolean = false) : TPermissionServiceResult;
    Function Delete(aFileId : string; aPermissionId : string; aEnforceExpansiveAccess : boolean = false; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false; aUseDomainAdminAccess : boolean = false) : TPermissionsDeleteResult;
    Function Get(aFileId : string; aPermissionId : string; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false; aUseDomainAdminAccess : boolean = false) : TPermissionServiceResult;
    Function List(aFileId : string; aIncludePermissionsForView : string; aPageSize : integer; aPageToken : string; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false; aUseDomainAdminAccess : boolean = false) : TPermissionListServiceResult;
    Function Update(aFileId : string; aPermissionId : string; aRequest : TPermission; aEnforceExpansiveAccess : boolean = false; aRemoveExpiration : boolean = false; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false; aTransferOwnership : boolean = false; aUseDomainAdminAccess : boolean = false) : TPermissionServiceResult;
  end;
  
  // Service IReplies
  
  TRepliesProxy = Class (TFPOpenAPIServiceClient,IReplies)
    Function Create_(aCommentId : string; aFileId : string; aRequest : TReply) : TReplyServiceResult;
    Function Delete(aCommentId : string; aFileId : string; aReplyId : string) : TRepliesDeleteResult;
    Function Get(aCommentId : string; aFileId : string; aReplyId : string; aIncludeDeleted : boolean = false) : TReplyServiceResult;
    Function List(aCommentId : string; aFileId : string; aPageToken : string; aIncludeDeleted : boolean = false; aPageSize : integer = 20) : TReplyListServiceResult;
    Function Update(aCommentId : string; aFileId : string; aReplyId : string; aRequest : TReply) : TReplyServiceResult;
  end;
  
  // Service IRevisions
  
  TRevisionsProxy = Class (TFPOpenAPIServiceClient,IRevisions)
    Function Delete(aFileId : string; aRevisionId : string) : TRevisionsDeleteResult;
    Function Get(aFileId : string; aRevisionId : string; aAcknowledgeAbuse : boolean = false) : TRevisionServiceResult;
    Function List(aFileId : string; aPageToken : string; aPageSize : integer = 200) : TRevisionListServiceResult;
    Function Update(aFileId : string; aRevisionId : string; aRequest : TRevision) : TRevisionServiceResult;
  end;
  
  // Service ITeamdrives
  
  TTeamdrivesProxy = Class (TFPOpenAPIServiceClient,ITeamdrives)
    Function Create_(aRequestId : string; aRequest : TTeamDrive) : TTeamDriveServiceResult;
    Function Delete(aTeamDriveId : string) : TTeamdrivesDeleteResult;
    Function Get(aTeamDriveId : string; aUseDomainAdminAccess : boolean = false) : TTeamDriveServiceResult;
    Function List(aPageToken : string; aQ : string; aPageSize : integer = 10; aUseDomainAdminAccess : boolean = false) : TTeamDriveListServiceResult;
    Function Update(aTeamDriveId : string; aRequest : TTeamDrive; aUseDomainAdminAccess : boolean = false) : TTeamDriveServiceResult;
  end;
  

implementation

uses
  SysUtils, DateUtils
  , drive.Serializer;

Function TAboutProxy.Get() : TAboutServiceResult;

const
  lMethodURL = '/about';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TAboutServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TAboutServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TAbout.Deserialize(lResponse.Content);
end;

Function TAccessproposalsProxy.Get(aFileId : string; aProposalId : string) : TAccessProposalServiceResult;

const
  lMethodURL = '/files/{fileId}/accessproposals/{proposalId}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TAccessProposalServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lUrl:=ReplacePathParam(lURL,'proposalId',aProposalId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TAccessProposalServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TAccessProposal.Deserialize(lResponse.Content);
end;

Function TAccessproposalsProxy.List(aFileId : string; aPageSize : integer; aPageToken : string) : TListAccessProposalsResponseServiceResult;

const
  lMethodURL = '/files/{fileId}/accessproposals';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TListAccessProposalsResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lQuery:=ConcatRestParam(lQuery,'pageSize',IntToStr(aPageSize));
  lQuery:=ConcatRestParam(lQuery,'pageToken',aPageToken);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TListAccessProposalsResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TListAccessProposalsResponse.Deserialize(lResponse.Content);
end;

Function TApprovalsProxy.Approve(aApprovalId : string; aFileId : string; aRequest : TApproveApprovalRequest) : TApprovalServiceResult;

const
  lMethodURL = '/files/{fileId}/approvals/{approvalId}:approve';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TApprovalServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'approvalId',aApprovalId);
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TApprovalServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TApproval.Deserialize(lResponse.Content);
end;

Function TApprovalsProxy.Cancel(aApprovalId : string; aFileId : string; aRequest : TCancelApprovalRequest) : TApprovalServiceResult;

const
  lMethodURL = '/files/{fileId}/approvals/{approvalId}:cancel';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TApprovalServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'approvalId',aApprovalId);
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TApprovalServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TApproval.Deserialize(lResponse.Content);
end;

Function TApprovalsProxy.Comment(aApprovalId : string; aFileId : string; aRequest : TCommentApprovalRequest) : TApprovalServiceResult;

const
  lMethodURL = '/files/{fileId}/approvals/{approvalId}:comment';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TApprovalServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'approvalId',aApprovalId);
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TApprovalServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TApproval.Deserialize(lResponse.Content);
end;

Function TApprovalsProxy.Decline(aApprovalId : string; aFileId : string; aRequest : TDeclineApprovalRequest) : TApprovalServiceResult;

const
  lMethodURL = '/files/{fileId}/approvals/{approvalId}:decline';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TApprovalServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'approvalId',aApprovalId);
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TApprovalServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TApproval.Deserialize(lResponse.Content);
end;

Function TApprovalsProxy.Get(aApprovalId : string; aFileId : string) : TApprovalServiceResult;

const
  lMethodURL = '/files/{fileId}/approvals/{approvalId}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TApprovalServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'approvalId',aApprovalId);
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TApprovalServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TApproval.Deserialize(lResponse.Content);
end;

Function TApprovalsProxy.List(aFileId : string; aPageSize : integer; aPageToken : string) : TApprovalListServiceResult;

const
  lMethodURL = '/files/{fileId}/approvals';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TApprovalListServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lQuery:=ConcatRestParam(lQuery,'pageSize',IntToStr(aPageSize));
  lQuery:=ConcatRestParam(lQuery,'pageToken',aPageToken);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TApprovalListServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TApprovalList.Deserialize(lResponse.Content);
end;

Function TApprovalsProxy.Reassign(aApprovalId : string; aFileId : string; aRequest : TReassignApprovalRequest) : TApprovalServiceResult;

const
  lMethodURL = '/files/{fileId}/approvals/{approvalId}:reassign';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TApprovalServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'approvalId',aApprovalId);
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TApprovalServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TApproval.Deserialize(lResponse.Content);
end;

Function TApprovalsProxy.Start(aFileId : string; aRequest : TStartApprovalRequest) : TApprovalServiceResult;

const
  lMethodURL = '/files/{fileId}/approvals:start';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TApprovalServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TApprovalServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TApproval.Deserialize(lResponse.Content);
end;

Function TAppsProxy.Get(aAppId : string) : TAppServiceResult;

const
  lMethodURL = '/apps/{appId}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TAppServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'appId',aAppId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TAppServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TApp.Deserialize(lResponse.Content);
end;

Function TAppsProxy.List(aAppFilterExtensions : string; aAppFilterMimeTypes : string; aLanguageCode : string) : TAppListServiceResult;

const
  lMethodURL = '/apps';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TAppListServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lQuery:=ConcatRestParam(lQuery,'appFilterExtensions',aAppFilterExtensions);
  lQuery:=ConcatRestParam(lQuery,'appFilterMimeTypes',aAppFilterMimeTypes);
  lQuery:=ConcatRestParam(lQuery,'languageCode',aLanguageCode);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TAppListServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TAppList.Deserialize(lResponse.Content);
end;

Function TChangesProxy.GetStartPageToken(aDriveId : string; aTeamDriveId : string; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false) : TStartPageTokenServiceResult;

const
  lMethodURL = '/changes/startPageToken';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TStartPageTokenServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lQuery:=ConcatRestParam(lQuery,'driveId',aDriveId);
  lQuery:=ConcatRestParam(lQuery,'supportsAllDrives',cRESTBooleans[aSupportsAllDrives]);
  lQuery:=ConcatRestParam(lQuery,'supportsTeamDrives',cRESTBooleans[aSupportsTeamDrives]);
  lQuery:=ConcatRestParam(lQuery,'teamDriveId',aTeamDriveId);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TStartPageTokenServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TStartPageToken.Deserialize(lResponse.Content);
end;

Function TChangesProxy.List(aDriveId : string; aIncludeLabels : string; aIncludePermissionsForView : string; aPageToken : string; aTeamDriveId : string; aIncludeCorpusRemovals : boolean = false; aIncludeItemsFromAllDrives : boolean = false; aIncludeRemoved : boolean = true; aIncludeTeamDriveItems : boolean = false; aPageSize : integer = 100; aRestrictToMyDrive : boolean = false; aSpaces : string = 'drive'; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false) : TChangeListServiceResult;

const
  lMethodURL = '/changes';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TChangeListServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lQuery:=ConcatRestParam(lQuery,'driveId',aDriveId);
  lQuery:=ConcatRestParam(lQuery,'includeCorpusRemovals',cRESTBooleans[aIncludeCorpusRemovals]);
  lQuery:=ConcatRestParam(lQuery,'includeItemsFromAllDrives',cRESTBooleans[aIncludeItemsFromAllDrives]);
  lQuery:=ConcatRestParam(lQuery,'includeLabels',aIncludeLabels);
  lQuery:=ConcatRestParam(lQuery,'includePermissionsForView',aIncludePermissionsForView);
  lQuery:=ConcatRestParam(lQuery,'includeRemoved',cRESTBooleans[aIncludeRemoved]);
  lQuery:=ConcatRestParam(lQuery,'includeTeamDriveItems',cRESTBooleans[aIncludeTeamDriveItems]);
  lQuery:=ConcatRestParam(lQuery,'pageSize',IntToStr(aPageSize));
  lQuery:=ConcatRestParam(lQuery,'pageToken',aPageToken);
  lQuery:=ConcatRestParam(lQuery,'restrictToMyDrive',cRESTBooleans[aRestrictToMyDrive]);
  lQuery:=ConcatRestParam(lQuery,'spaces',aSpaces);
  lQuery:=ConcatRestParam(lQuery,'supportsAllDrives',cRESTBooleans[aSupportsAllDrives]);
  lQuery:=ConcatRestParam(lQuery,'supportsTeamDrives',cRESTBooleans[aSupportsTeamDrives]);
  lQuery:=ConcatRestParam(lQuery,'teamDriveId',aTeamDriveId);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TChangeListServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TChangeList.Deserialize(lResponse.Content);
end;

Function TChangesProxy.Watch(aDriveId : string; aIncludeLabels : string; aIncludePermissionsForView : string; aPageToken : string; aTeamDriveId : string; aRequest : TChannel; aIncludeCorpusRemovals : boolean = false; aIncludeItemsFromAllDrives : boolean = false; aIncludeRemoved : boolean = true; aIncludeTeamDriveItems : boolean = false; aPageSize : integer = 100; aRestrictToMyDrive : boolean = false; aSpaces : string = 'drive'; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false) : TChannelServiceResult;

const
  lMethodURL = '/changes/watch';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TChannelServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lQuery:=ConcatRestParam(lQuery,'driveId',aDriveId);
  lQuery:=ConcatRestParam(lQuery,'includeCorpusRemovals',cRESTBooleans[aIncludeCorpusRemovals]);
  lQuery:=ConcatRestParam(lQuery,'includeItemsFromAllDrives',cRESTBooleans[aIncludeItemsFromAllDrives]);
  lQuery:=ConcatRestParam(lQuery,'includeLabels',aIncludeLabels);
  lQuery:=ConcatRestParam(lQuery,'includePermissionsForView',aIncludePermissionsForView);
  lQuery:=ConcatRestParam(lQuery,'includeRemoved',cRESTBooleans[aIncludeRemoved]);
  lQuery:=ConcatRestParam(lQuery,'includeTeamDriveItems',cRESTBooleans[aIncludeTeamDriveItems]);
  lQuery:=ConcatRestParam(lQuery,'pageSize',IntToStr(aPageSize));
  lQuery:=ConcatRestParam(lQuery,'pageToken',aPageToken);
  lQuery:=ConcatRestParam(lQuery,'restrictToMyDrive',cRESTBooleans[aRestrictToMyDrive]);
  lQuery:=ConcatRestParam(lQuery,'spaces',aSpaces);
  lQuery:=ConcatRestParam(lQuery,'supportsAllDrives',cRESTBooleans[aSupportsAllDrives]);
  lQuery:=ConcatRestParam(lQuery,'supportsTeamDrives',cRESTBooleans[aSupportsTeamDrives]);
  lQuery:=ConcatRestParam(lQuery,'teamDriveId',aTeamDriveId);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TChannelServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TChannel.Deserialize(lResponse.Content);
end;

Function TCommentsProxy.Create_(aFileId : string; aRequest : TComment) : TCommentServiceResult;

const
  lMethodURL = '/files/{fileId}/comments';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TCommentServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TCommentServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TComment.Deserialize(lResponse.Content);
end;

Function TCommentsProxy.Delete(aCommentId : string; aFileId : string) : TCommentsDeleteResult;

const
  lMethodURL = '/files/{fileId}/comments/{commentId}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TCommentsDeleteResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'commentId',aCommentId);
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result.FStatusCode:=lResponse.StatusCode;
  Result.FStatusText:=lResponse.StatusText;
  Result.FContentType:=lResponse.ContentType;
  Result.FRawContent:=lResponse.Content;
  Result.FRawStream:=lResponse.ContentStream;
  
  case lResponse.StatusCode of
    200: begin
      Result.FResponseKind:=TCommentsDeleteResponseKind.CommentsDeleterkSuccess;
    end;
    else begin
      Result.FResponseKind:=TCommentsDeleteResponseKind.CommentsDeleterkUnexpected;
    end;
  end;
end;

Function TCommentsProxy.Get(aCommentId : string; aFileId : string; aIncludeDeleted : boolean = false) : TCommentServiceResult;

const
  lMethodURL = '/files/{fileId}/comments/{commentId}';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TCommentServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'commentId',aCommentId);
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lQuery:=ConcatRestParam(lQuery,'includeDeleted',cRESTBooleans[aIncludeDeleted]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TCommentServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TComment.Deserialize(lResponse.Content);
end;

Function TCommentsProxy.List(aFileId : string; aPageToken : string; aStartModifiedTime : string; aIncludeDeleted : boolean = false; aPageSize : integer = 20) : TCommentListServiceResult;

const
  lMethodURL = '/files/{fileId}/comments';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TCommentListServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lQuery:=ConcatRestParam(lQuery,'includeDeleted',cRESTBooleans[aIncludeDeleted]);
  lQuery:=ConcatRestParam(lQuery,'pageSize',IntToStr(aPageSize));
  lQuery:=ConcatRestParam(lQuery,'pageToken',aPageToken);
  lQuery:=ConcatRestParam(lQuery,'startModifiedTime',aStartModifiedTime);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TCommentListServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TCommentList.Deserialize(lResponse.Content);
end;

Function TCommentsProxy.Update(aCommentId : string; aFileId : string; aRequest : TComment) : TCommentServiceResult;

const
  lMethodURL = '/files/{fileId}/comments/{commentId}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TCommentServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'commentId',aCommentId);
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lResponse:=ExecuteRequest('patch',lURL,aRequest.Serialize);
  Result:=TCommentServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TComment.Deserialize(lResponse.Content);
end;

Function TDrivesProxy.Create_(aRequestId : string; aRequest : TDrive) : TDriveServiceResult;

const
  lMethodURL = '/drives';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TDriveServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lQuery:=ConcatRestParam(lQuery,'requestId',aRequestId);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TDriveServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TDrive.Deserialize(lResponse.Content);
end;

Function TDrivesProxy.Delete(aDriveId : string; aAllowItemDeletion : boolean = false; aUseDomainAdminAccess : boolean = false) : TDrivesDeleteResult;

const
  lMethodURL = '/drives/{driveId}';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TDrivesDeleteResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'driveId',aDriveId);
  lQuery:=ConcatRestParam(lQuery,'allowItemDeletion',cRESTBooleans[aAllowItemDeletion]);
  lQuery:=ConcatRestParam(lQuery,'useDomainAdminAccess',cRESTBooleans[aUseDomainAdminAccess]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result.FStatusCode:=lResponse.StatusCode;
  Result.FStatusText:=lResponse.StatusText;
  Result.FContentType:=lResponse.ContentType;
  Result.FRawContent:=lResponse.Content;
  Result.FRawStream:=lResponse.ContentStream;
  
  case lResponse.StatusCode of
    200: begin
      Result.FResponseKind:=TDrivesDeleteResponseKind.DrivesDeleterkSuccess;
    end;
    else begin
      Result.FResponseKind:=TDrivesDeleteResponseKind.DrivesDeleterkUnexpected;
    end;
  end;
end;

Function TDrivesProxy.Get(aDriveId : string; aUseDomainAdminAccess : boolean = false) : TDriveServiceResult;

const
  lMethodURL = '/drives/{driveId}';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TDriveServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'driveId',aDriveId);
  lQuery:=ConcatRestParam(lQuery,'useDomainAdminAccess',cRESTBooleans[aUseDomainAdminAccess]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TDriveServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TDrive.Deserialize(lResponse.Content);
end;

Function TDrivesProxy.Hide(aDriveId : string) : TDriveServiceResult;

const
  lMethodURL = '/drives/{driveId}/hide';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TDriveServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'driveId',aDriveId);
  lResponse:=ExecuteRequest('post',lURL,'');
  Result:=TDriveServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TDrive.Deserialize(lResponse.Content);
end;

Function TDrivesProxy.List(aPageToken : string; aQ : string; aPageSize : integer = 10; aUseDomainAdminAccess : boolean = false) : TDriveListServiceResult;

const
  lMethodURL = '/drives';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TDriveListServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lQuery:=ConcatRestParam(lQuery,'pageSize',IntToStr(aPageSize));
  lQuery:=ConcatRestParam(lQuery,'pageToken',aPageToken);
  lQuery:=ConcatRestParam(lQuery,'q',aQ);
  lQuery:=ConcatRestParam(lQuery,'useDomainAdminAccess',cRESTBooleans[aUseDomainAdminAccess]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TDriveListServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TDriveList.Deserialize(lResponse.Content);
end;

Function TDrivesProxy.Unhide(aDriveId : string) : TDriveServiceResult;

const
  lMethodURL = '/drives/{driveId}/unhide';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TDriveServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'driveId',aDriveId);
  lResponse:=ExecuteRequest('post',lURL,'');
  Result:=TDriveServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TDrive.Deserialize(lResponse.Content);
end;

Function TDrivesProxy.Update(aDriveId : string; aRequest : TDrive; aUseDomainAdminAccess : boolean = false) : TDriveServiceResult;

const
  lMethodURL = '/drives/{driveId}';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TDriveServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'driveId',aDriveId);
  lQuery:=ConcatRestParam(lQuery,'useDomainAdminAccess',cRESTBooleans[aUseDomainAdminAccess]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('patch',lURL,aRequest.Serialize);
  Result:=TDriveServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TDrive.Deserialize(lResponse.Content);
end;

Function TFilesProxy.Copy(aFileId : string; aIncludeLabels : string; aIncludePermissionsForView : string; aOcrLanguage : string; aRequest : TFile; aCopyComments : boolean = false; aEnforceSingleParent : boolean = false; aIgnoreDefaultVisibility : boolean = false; aKeepRevisionForever : boolean = false; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false) : TFileServiceResult;

const
  lMethodURL = '/files/{fileId}/copy';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TFileServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lQuery:=ConcatRestParam(lQuery,'copyComments',cRESTBooleans[aCopyComments]);
  lQuery:=ConcatRestParam(lQuery,'enforceSingleParent',cRESTBooleans[aEnforceSingleParent]);
  lQuery:=ConcatRestParam(lQuery,'ignoreDefaultVisibility',cRESTBooleans[aIgnoreDefaultVisibility]);
  lQuery:=ConcatRestParam(lQuery,'includeLabels',aIncludeLabels);
  lQuery:=ConcatRestParam(lQuery,'includePermissionsForView',aIncludePermissionsForView);
  lQuery:=ConcatRestParam(lQuery,'keepRevisionForever',cRESTBooleans[aKeepRevisionForever]);
  lQuery:=ConcatRestParam(lQuery,'ocrLanguage',aOcrLanguage);
  lQuery:=ConcatRestParam(lQuery,'supportsAllDrives',cRESTBooleans[aSupportsAllDrives]);
  lQuery:=ConcatRestParam(lQuery,'supportsTeamDrives',cRESTBooleans[aSupportsTeamDrives]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TFileServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TFile.Deserialize(lResponse.Content);
end;

Function TFilesProxy.Create_(aIncludeLabels : string; aIncludePermissionsForView : string; aOcrLanguage : string; aRequest : TFile; aEnforceSingleParent : boolean = false; aIgnoreDefaultVisibility : boolean = false; aKeepRevisionForever : boolean = false; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false; aUseContentAsIndexableText : boolean = false) : TFileServiceResult;

const
  lMethodURL = '/files';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TFileServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lQuery:=ConcatRestParam(lQuery,'enforceSingleParent',cRESTBooleans[aEnforceSingleParent]);
  lQuery:=ConcatRestParam(lQuery,'ignoreDefaultVisibility',cRESTBooleans[aIgnoreDefaultVisibility]);
  lQuery:=ConcatRestParam(lQuery,'includeLabels',aIncludeLabels);
  lQuery:=ConcatRestParam(lQuery,'includePermissionsForView',aIncludePermissionsForView);
  lQuery:=ConcatRestParam(lQuery,'keepRevisionForever',cRESTBooleans[aKeepRevisionForever]);
  lQuery:=ConcatRestParam(lQuery,'ocrLanguage',aOcrLanguage);
  lQuery:=ConcatRestParam(lQuery,'supportsAllDrives',cRESTBooleans[aSupportsAllDrives]);
  lQuery:=ConcatRestParam(lQuery,'supportsTeamDrives',cRESTBooleans[aSupportsTeamDrives]);
  lQuery:=ConcatRestParam(lQuery,'useContentAsIndexableText',cRESTBooleans[aUseContentAsIndexableText]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TFileServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TFile.Deserialize(lResponse.Content);
end;

Function TFilesProxy.Delete(aFileId : string; aEnforceSingleParent : boolean = false; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false) : TFilesDeleteResult;

const
  lMethodURL = '/files/{fileId}';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TFilesDeleteResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lQuery:=ConcatRestParam(lQuery,'enforceSingleParent',cRESTBooleans[aEnforceSingleParent]);
  lQuery:=ConcatRestParam(lQuery,'supportsAllDrives',cRESTBooleans[aSupportsAllDrives]);
  lQuery:=ConcatRestParam(lQuery,'supportsTeamDrives',cRESTBooleans[aSupportsTeamDrives]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result.FStatusCode:=lResponse.StatusCode;
  Result.FStatusText:=lResponse.StatusText;
  Result.FContentType:=lResponse.ContentType;
  Result.FRawContent:=lResponse.Content;
  Result.FRawStream:=lResponse.ContentStream;
  
  case lResponse.StatusCode of
    200: begin
      Result.FResponseKind:=TFilesDeleteResponseKind.FilesDeleterkSuccess;
    end;
    else begin
      Result.FResponseKind:=TFilesDeleteResponseKind.FilesDeleterkUnexpected;
    end;
  end;
end;

Function TFilesProxy.Download(aFileId : string; aMimeType : string; aRevisionId : string) : TOperationServiceResult;

const
  lMethodURL = '/files/{fileId}/download';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TOperationServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lQuery:=ConcatRestParam(lQuery,'mimeType',aMimeType);
  lQuery:=ConcatRestParam(lQuery,'revisionId',aRevisionId);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('post',lURL,'');
  Result:=TOperationServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TOperation.Deserialize(lResponse.Content);
end;

Function TFilesProxy.EmptyTrash(aDriveId : string; aEnforceSingleParent : boolean = false) : TFilesEmptyTrashResult;

const
  lMethodURL = '/files/trash';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TFilesEmptyTrashResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lQuery:=ConcatRestParam(lQuery,'driveId',aDriveId);
  lQuery:=ConcatRestParam(lQuery,'enforceSingleParent',cRESTBooleans[aEnforceSingleParent]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result.FStatusCode:=lResponse.StatusCode;
  Result.FStatusText:=lResponse.StatusText;
  Result.FContentType:=lResponse.ContentType;
  Result.FRawContent:=lResponse.Content;
  Result.FRawStream:=lResponse.ContentStream;
  
  case lResponse.StatusCode of
    200: begin
      Result.FResponseKind:=TFilesEmptyTrashResponseKind.FilesEmptyTrashrkSuccess;
    end;
    else begin
      Result.FResponseKind:=TFilesEmptyTrashResponseKind.FilesEmptyTrashrkUnexpected;
    end;
  end;
end;

Function TFilesProxy.GenerateCseToken(aFileId : string; aParent : string) : TGenerateCseTokenResponseServiceResult;

const
  lMethodURL = '/files/generateCseToken';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TGenerateCseTokenResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lQuery:=ConcatRestParam(lQuery,'fileId',aFileId);
  lQuery:=ConcatRestParam(lQuery,'parent',aParent);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TGenerateCseTokenResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TGenerateCseTokenResponse.Deserialize(lResponse.Content);
end;

Function TFilesProxy.GenerateIds(aCount : integer = 10; aSpace : string = 'drive'; aType : string = 'files') : TGeneratedIdsServiceResult;

const
  lMethodURL = '/files/generateIds';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TGeneratedIdsServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lQuery:=ConcatRestParam(lQuery,'count',IntToStr(aCount));
  lQuery:=ConcatRestParam(lQuery,'space',aSpace);
  lQuery:=ConcatRestParam(lQuery,'type',aType);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TGeneratedIdsServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TGeneratedIds.Deserialize(lResponse.Content);
end;

Function TFilesProxy.Get(aFileId : string; aIncludeLabels : string; aIncludePermissionsForView : string; aAcknowledgeAbuse : boolean = false; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false) : TFileServiceResult;

const
  lMethodURL = '/files/{fileId}';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TFileServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lQuery:=ConcatRestParam(lQuery,'acknowledgeAbuse',cRESTBooleans[aAcknowledgeAbuse]);
  lQuery:=ConcatRestParam(lQuery,'includeLabels',aIncludeLabels);
  lQuery:=ConcatRestParam(lQuery,'includePermissionsForView',aIncludePermissionsForView);
  lQuery:=ConcatRestParam(lQuery,'supportsAllDrives',cRESTBooleans[aSupportsAllDrives]);
  lQuery:=ConcatRestParam(lQuery,'supportsTeamDrives',cRESTBooleans[aSupportsTeamDrives]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TFileServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TFile.Deserialize(lResponse.Content);
end;

Function TFilesProxy.List(aCorpora : string; aCorpus : string; aDriveId : string; aIncludeLabels : string; aIncludePermissionsForView : string; aOrderBy : string; aPageToken : string; aQ : string; aTeamDriveId : string; aIncludeItemsFromAllDrives : boolean = false; aIncludeTeamDriveItems : boolean = false; aPageSize : integer = 100; aSpaces : string = 'drive'; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false) : TFileListServiceResult;

const
  lMethodURL = '/files';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TFileListServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lQuery:=ConcatRestParam(lQuery,'corpora',aCorpora);
  lQuery:=ConcatRestParam(lQuery,'corpus',aCorpus);
  lQuery:=ConcatRestParam(lQuery,'driveId',aDriveId);
  lQuery:=ConcatRestParam(lQuery,'includeItemsFromAllDrives',cRESTBooleans[aIncludeItemsFromAllDrives]);
  lQuery:=ConcatRestParam(lQuery,'includeLabels',aIncludeLabels);
  lQuery:=ConcatRestParam(lQuery,'includePermissionsForView',aIncludePermissionsForView);
  lQuery:=ConcatRestParam(lQuery,'includeTeamDriveItems',cRESTBooleans[aIncludeTeamDriveItems]);
  lQuery:=ConcatRestParam(lQuery,'orderBy',aOrderBy);
  lQuery:=ConcatRestParam(lQuery,'pageSize',IntToStr(aPageSize));
  lQuery:=ConcatRestParam(lQuery,'pageToken',aPageToken);
  lQuery:=ConcatRestParam(lQuery,'q',aQ);
  lQuery:=ConcatRestParam(lQuery,'spaces',aSpaces);
  lQuery:=ConcatRestParam(lQuery,'supportsAllDrives',cRESTBooleans[aSupportsAllDrives]);
  lQuery:=ConcatRestParam(lQuery,'supportsTeamDrives',cRESTBooleans[aSupportsTeamDrives]);
  lQuery:=ConcatRestParam(lQuery,'teamDriveId',aTeamDriveId);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TFileListServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TFileList.Deserialize(lResponse.Content);
end;

Function TFilesProxy.ListLabels(aFileId : string; aPageToken : string; aMaxResults : integer = 100) : TLabelListServiceResult;

const
  lMethodURL = '/files/{fileId}/listLabels';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TLabelListServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lQuery:=ConcatRestParam(lQuery,'maxResults',IntToStr(aMaxResults));
  lQuery:=ConcatRestParam(lQuery,'pageToken',aPageToken);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TLabelListServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TLabelList.Deserialize(lResponse.Content);
end;

Function TFilesProxy.ModifyLabels(aFileId : string; aRequest : TModifyLabelsRequest) : TModifyLabelsResponseServiceResult;

const
  lMethodURL = '/files/{fileId}/modifyLabels';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TModifyLabelsResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TModifyLabelsResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TModifyLabelsResponse.Deserialize(lResponse.Content);
end;

Function TFilesProxy.Update(aAddParents : string; aFileId : string; aIncludeLabels : string; aIncludePermissionsForView : string; aOcrLanguage : string; aRemoveParents : string; aRequest : TFile; aEnforceSingleParent : boolean = false; aKeepRevisionForever : boolean = false; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false; aUseContentAsIndexableText : boolean = false) : TFileServiceResult;

const
  lMethodURL = '/files/{fileId}';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TFileServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lQuery:=ConcatRestParam(lQuery,'addParents',aAddParents);
  lQuery:=ConcatRestParam(lQuery,'enforceSingleParent',cRESTBooleans[aEnforceSingleParent]);
  lQuery:=ConcatRestParam(lQuery,'includeLabels',aIncludeLabels);
  lQuery:=ConcatRestParam(lQuery,'includePermissionsForView',aIncludePermissionsForView);
  lQuery:=ConcatRestParam(lQuery,'keepRevisionForever',cRESTBooleans[aKeepRevisionForever]);
  lQuery:=ConcatRestParam(lQuery,'ocrLanguage',aOcrLanguage);
  lQuery:=ConcatRestParam(lQuery,'removeParents',aRemoveParents);
  lQuery:=ConcatRestParam(lQuery,'supportsAllDrives',cRESTBooleans[aSupportsAllDrives]);
  lQuery:=ConcatRestParam(lQuery,'supportsTeamDrives',cRESTBooleans[aSupportsTeamDrives]);
  lQuery:=ConcatRestParam(lQuery,'useContentAsIndexableText',cRESTBooleans[aUseContentAsIndexableText]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('patch',lURL,aRequest.Serialize);
  Result:=TFileServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TFile.Deserialize(lResponse.Content);
end;

Function TFilesProxy.Watch(aFileId : string; aIncludeLabels : string; aIncludePermissionsForView : string; aRequest : TChannel; aAcknowledgeAbuse : boolean = false; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false) : TChannelServiceResult;

const
  lMethodURL = '/files/{fileId}/watch';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TChannelServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lQuery:=ConcatRestParam(lQuery,'acknowledgeAbuse',cRESTBooleans[aAcknowledgeAbuse]);
  lQuery:=ConcatRestParam(lQuery,'includeLabels',aIncludeLabels);
  lQuery:=ConcatRestParam(lQuery,'includePermissionsForView',aIncludePermissionsForView);
  lQuery:=ConcatRestParam(lQuery,'supportsAllDrives',cRESTBooleans[aSupportsAllDrives]);
  lQuery:=ConcatRestParam(lQuery,'supportsTeamDrives',cRESTBooleans[aSupportsTeamDrives]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TChannelServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TChannel.Deserialize(lResponse.Content);
end;

Function TOperationsProxy.Get(aName : string) : TOperationServiceResult;

const
  lMethodURL = '/operations/{name}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TOperationServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'name',aName);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TOperationServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TOperation.Deserialize(lResponse.Content);
end;

Function TPermissionsProxy.Create_(aEmailMessage : string; aFileId : string; aSendNotificationEmail : boolean; aRequest : TPermission; aEnforceExpansiveAccess : boolean = false; aEnforceSingleParent : boolean = false; aMoveToNewOwnersRoot : boolean = false; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false; aTransferOwnership : boolean = false; aUseDomainAdminAccess : boolean = false) : TPermissionServiceResult;

const
  lMethodURL = '/files/{fileId}/permissions';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TPermissionServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lQuery:=ConcatRestParam(lQuery,'emailMessage',aEmailMessage);
  lQuery:=ConcatRestParam(lQuery,'enforceExpansiveAccess',cRESTBooleans[aEnforceExpansiveAccess]);
  lQuery:=ConcatRestParam(lQuery,'enforceSingleParent',cRESTBooleans[aEnforceSingleParent]);
  lQuery:=ConcatRestParam(lQuery,'moveToNewOwnersRoot',cRESTBooleans[aMoveToNewOwnersRoot]);
  lQuery:=ConcatRestParam(lQuery,'sendNotificationEmail',cRESTBooleans[aSendNotificationEmail]);
  lQuery:=ConcatRestParam(lQuery,'supportsAllDrives',cRESTBooleans[aSupportsAllDrives]);
  lQuery:=ConcatRestParam(lQuery,'supportsTeamDrives',cRESTBooleans[aSupportsTeamDrives]);
  lQuery:=ConcatRestParam(lQuery,'transferOwnership',cRESTBooleans[aTransferOwnership]);
  lQuery:=ConcatRestParam(lQuery,'useDomainAdminAccess',cRESTBooleans[aUseDomainAdminAccess]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TPermissionServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TPermission.Deserialize(lResponse.Content);
end;

Function TPermissionsProxy.Delete(aFileId : string; aPermissionId : string; aEnforceExpansiveAccess : boolean = false; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false; aUseDomainAdminAccess : boolean = false) : TPermissionsDeleteResult;

const
  lMethodURL = '/files/{fileId}/permissions/{permissionId}';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TPermissionsDeleteResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lUrl:=ReplacePathParam(lURL,'permissionId',aPermissionId);
  lQuery:=ConcatRestParam(lQuery,'enforceExpansiveAccess',cRESTBooleans[aEnforceExpansiveAccess]);
  lQuery:=ConcatRestParam(lQuery,'supportsAllDrives',cRESTBooleans[aSupportsAllDrives]);
  lQuery:=ConcatRestParam(lQuery,'supportsTeamDrives',cRESTBooleans[aSupportsTeamDrives]);
  lQuery:=ConcatRestParam(lQuery,'useDomainAdminAccess',cRESTBooleans[aUseDomainAdminAccess]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result.FStatusCode:=lResponse.StatusCode;
  Result.FStatusText:=lResponse.StatusText;
  Result.FContentType:=lResponse.ContentType;
  Result.FRawContent:=lResponse.Content;
  Result.FRawStream:=lResponse.ContentStream;
  
  case lResponse.StatusCode of
    200: begin
      Result.FResponseKind:=TPermissionsDeleteResponseKind.PermissionsDeleterkSuccess;
    end;
    else begin
      Result.FResponseKind:=TPermissionsDeleteResponseKind.PermissionsDeleterkUnexpected;
    end;
  end;
end;

Function TPermissionsProxy.Get(aFileId : string; aPermissionId : string; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false; aUseDomainAdminAccess : boolean = false) : TPermissionServiceResult;

const
  lMethodURL = '/files/{fileId}/permissions/{permissionId}';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TPermissionServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lUrl:=ReplacePathParam(lURL,'permissionId',aPermissionId);
  lQuery:=ConcatRestParam(lQuery,'supportsAllDrives',cRESTBooleans[aSupportsAllDrives]);
  lQuery:=ConcatRestParam(lQuery,'supportsTeamDrives',cRESTBooleans[aSupportsTeamDrives]);
  lQuery:=ConcatRestParam(lQuery,'useDomainAdminAccess',cRESTBooleans[aUseDomainAdminAccess]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TPermissionServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TPermission.Deserialize(lResponse.Content);
end;

Function TPermissionsProxy.List(aFileId : string; aIncludePermissionsForView : string; aPageSize : integer; aPageToken : string; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false; aUseDomainAdminAccess : boolean = false) : TPermissionListServiceResult;

const
  lMethodURL = '/files/{fileId}/permissions';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TPermissionListServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lQuery:=ConcatRestParam(lQuery,'includePermissionsForView',aIncludePermissionsForView);
  lQuery:=ConcatRestParam(lQuery,'pageSize',IntToStr(aPageSize));
  lQuery:=ConcatRestParam(lQuery,'pageToken',aPageToken);
  lQuery:=ConcatRestParam(lQuery,'supportsAllDrives',cRESTBooleans[aSupportsAllDrives]);
  lQuery:=ConcatRestParam(lQuery,'supportsTeamDrives',cRESTBooleans[aSupportsTeamDrives]);
  lQuery:=ConcatRestParam(lQuery,'useDomainAdminAccess',cRESTBooleans[aUseDomainAdminAccess]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TPermissionListServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TPermissionList.Deserialize(lResponse.Content);
end;

Function TPermissionsProxy.Update(aFileId : string; aPermissionId : string; aRequest : TPermission; aEnforceExpansiveAccess : boolean = false; aRemoveExpiration : boolean = false; aSupportsAllDrives : boolean = false; aSupportsTeamDrives : boolean = false; aTransferOwnership : boolean = false; aUseDomainAdminAccess : boolean = false) : TPermissionServiceResult;

const
  lMethodURL = '/files/{fileId}/permissions/{permissionId}';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TPermissionServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lUrl:=ReplacePathParam(lURL,'permissionId',aPermissionId);
  lQuery:=ConcatRestParam(lQuery,'enforceExpansiveAccess',cRESTBooleans[aEnforceExpansiveAccess]);
  lQuery:=ConcatRestParam(lQuery,'removeExpiration',cRESTBooleans[aRemoveExpiration]);
  lQuery:=ConcatRestParam(lQuery,'supportsAllDrives',cRESTBooleans[aSupportsAllDrives]);
  lQuery:=ConcatRestParam(lQuery,'supportsTeamDrives',cRESTBooleans[aSupportsTeamDrives]);
  lQuery:=ConcatRestParam(lQuery,'transferOwnership',cRESTBooleans[aTransferOwnership]);
  lQuery:=ConcatRestParam(lQuery,'useDomainAdminAccess',cRESTBooleans[aUseDomainAdminAccess]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('patch',lURL,aRequest.Serialize);
  Result:=TPermissionServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TPermission.Deserialize(lResponse.Content);
end;

Function TRepliesProxy.Create_(aCommentId : string; aFileId : string; aRequest : TReply) : TReplyServiceResult;

const
  lMethodURL = '/files/{fileId}/comments/{commentId}/replies';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TReplyServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'commentId',aCommentId);
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TReplyServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TReply.Deserialize(lResponse.Content);
end;

Function TRepliesProxy.Delete(aCommentId : string; aFileId : string; aReplyId : string) : TRepliesDeleteResult;

const
  lMethodURL = '/files/{fileId}/comments/{commentId}/replies/{replyId}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TRepliesDeleteResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'commentId',aCommentId);
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lUrl:=ReplacePathParam(lURL,'replyId',aReplyId);
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result.FStatusCode:=lResponse.StatusCode;
  Result.FStatusText:=lResponse.StatusText;
  Result.FContentType:=lResponse.ContentType;
  Result.FRawContent:=lResponse.Content;
  Result.FRawStream:=lResponse.ContentStream;
  
  case lResponse.StatusCode of
    200: begin
      Result.FResponseKind:=TRepliesDeleteResponseKind.RepliesDeleterkSuccess;
    end;
    else begin
      Result.FResponseKind:=TRepliesDeleteResponseKind.RepliesDeleterkUnexpected;
    end;
  end;
end;

Function TRepliesProxy.Get(aCommentId : string; aFileId : string; aReplyId : string; aIncludeDeleted : boolean = false) : TReplyServiceResult;

const
  lMethodURL = '/files/{fileId}/comments/{commentId}/replies/{replyId}';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TReplyServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'commentId',aCommentId);
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lUrl:=ReplacePathParam(lURL,'replyId',aReplyId);
  lQuery:=ConcatRestParam(lQuery,'includeDeleted',cRESTBooleans[aIncludeDeleted]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TReplyServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TReply.Deserialize(lResponse.Content);
end;

Function TRepliesProxy.List(aCommentId : string; aFileId : string; aPageToken : string; aIncludeDeleted : boolean = false; aPageSize : integer = 20) : TReplyListServiceResult;

const
  lMethodURL = '/files/{fileId}/comments/{commentId}/replies';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TReplyListServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'commentId',aCommentId);
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lQuery:=ConcatRestParam(lQuery,'includeDeleted',cRESTBooleans[aIncludeDeleted]);
  lQuery:=ConcatRestParam(lQuery,'pageSize',IntToStr(aPageSize));
  lQuery:=ConcatRestParam(lQuery,'pageToken',aPageToken);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TReplyListServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TReplyList.Deserialize(lResponse.Content);
end;

Function TRepliesProxy.Update(aCommentId : string; aFileId : string; aReplyId : string; aRequest : TReply) : TReplyServiceResult;

const
  lMethodURL = '/files/{fileId}/comments/{commentId}/replies/{replyId}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TReplyServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'commentId',aCommentId);
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lUrl:=ReplacePathParam(lURL,'replyId',aReplyId);
  lResponse:=ExecuteRequest('patch',lURL,aRequest.Serialize);
  Result:=TReplyServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TReply.Deserialize(lResponse.Content);
end;

Function TRevisionsProxy.Delete(aFileId : string; aRevisionId : string) : TRevisionsDeleteResult;

const
  lMethodURL = '/files/{fileId}/revisions/{revisionId}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TRevisionsDeleteResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lUrl:=ReplacePathParam(lURL,'revisionId',aRevisionId);
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result.FStatusCode:=lResponse.StatusCode;
  Result.FStatusText:=lResponse.StatusText;
  Result.FContentType:=lResponse.ContentType;
  Result.FRawContent:=lResponse.Content;
  Result.FRawStream:=lResponse.ContentStream;
  
  case lResponse.StatusCode of
    200: begin
      Result.FResponseKind:=TRevisionsDeleteResponseKind.RevisionsDeleterkSuccess;
    end;
    else begin
      Result.FResponseKind:=TRevisionsDeleteResponseKind.RevisionsDeleterkUnexpected;
    end;
  end;
end;

Function TRevisionsProxy.Get(aFileId : string; aRevisionId : string; aAcknowledgeAbuse : boolean = false) : TRevisionServiceResult;

const
  lMethodURL = '/files/{fileId}/revisions/{revisionId}';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TRevisionServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lUrl:=ReplacePathParam(lURL,'revisionId',aRevisionId);
  lQuery:=ConcatRestParam(lQuery,'acknowledgeAbuse',cRESTBooleans[aAcknowledgeAbuse]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TRevisionServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TRevision.Deserialize(lResponse.Content);
end;

Function TRevisionsProxy.List(aFileId : string; aPageToken : string; aPageSize : integer = 200) : TRevisionListServiceResult;

const
  lMethodURL = '/files/{fileId}/revisions';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TRevisionListServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lQuery:=ConcatRestParam(lQuery,'pageSize',IntToStr(aPageSize));
  lQuery:=ConcatRestParam(lQuery,'pageToken',aPageToken);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TRevisionListServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TRevisionList.Deserialize(lResponse.Content);
end;

Function TRevisionsProxy.Update(aFileId : string; aRevisionId : string; aRequest : TRevision) : TRevisionServiceResult;

const
  lMethodURL = '/files/{fileId}/revisions/{revisionId}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TRevisionServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'fileId',aFileId);
  lUrl:=ReplacePathParam(lURL,'revisionId',aRevisionId);
  lResponse:=ExecuteRequest('patch',lURL,aRequest.Serialize);
  Result:=TRevisionServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TRevision.Deserialize(lResponse.Content);
end;

Function TTeamdrivesProxy.Create_(aRequestId : string; aRequest : TTeamDrive) : TTeamDriveServiceResult;

const
  lMethodURL = '/teamdrives';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TTeamDriveServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lQuery:=ConcatRestParam(lQuery,'requestId',aRequestId);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TTeamDriveServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TTeamDrive.Deserialize(lResponse.Content);
end;

Function TTeamdrivesProxy.Delete(aTeamDriveId : string) : TTeamdrivesDeleteResult;

const
  lMethodURL = '/teamdrives/{teamDriveId}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TTeamdrivesDeleteResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'teamDriveId',aTeamDriveId);
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result.FStatusCode:=lResponse.StatusCode;
  Result.FStatusText:=lResponse.StatusText;
  Result.FContentType:=lResponse.ContentType;
  Result.FRawContent:=lResponse.Content;
  Result.FRawStream:=lResponse.ContentStream;
  
  case lResponse.StatusCode of
    200: begin
      Result.FResponseKind:=TTeamdrivesDeleteResponseKind.TeamdrivesDeleterkSuccess;
    end;
    else begin
      Result.FResponseKind:=TTeamdrivesDeleteResponseKind.TeamdrivesDeleterkUnexpected;
    end;
  end;
end;

Function TTeamdrivesProxy.Get(aTeamDriveId : string; aUseDomainAdminAccess : boolean = false) : TTeamDriveServiceResult;

const
  lMethodURL = '/teamdrives/{teamDriveId}';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TTeamDriveServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'teamDriveId',aTeamDriveId);
  lQuery:=ConcatRestParam(lQuery,'useDomainAdminAccess',cRESTBooleans[aUseDomainAdminAccess]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TTeamDriveServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TTeamDrive.Deserialize(lResponse.Content);
end;

Function TTeamdrivesProxy.List(aPageToken : string; aQ : string; aPageSize : integer = 10; aUseDomainAdminAccess : boolean = false) : TTeamDriveListServiceResult;

const
  lMethodURL = '/teamdrives';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TTeamDriveListServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lQuery:=ConcatRestParam(lQuery,'pageSize',IntToStr(aPageSize));
  lQuery:=ConcatRestParam(lQuery,'pageToken',aPageToken);
  lQuery:=ConcatRestParam(lQuery,'q',aQ);
  lQuery:=ConcatRestParam(lQuery,'useDomainAdminAccess',cRESTBooleans[aUseDomainAdminAccess]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TTeamDriveListServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TTeamDriveList.Deserialize(lResponse.Content);
end;

Function TTeamdrivesProxy.Update(aTeamDriveId : string; aRequest : TTeamDrive; aUseDomainAdminAccess : boolean = false) : TTeamDriveServiceResult;

const
  lMethodURL = '/teamdrives/{teamDriveId}';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TTeamDriveServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'teamDriveId',aTeamDriveId);
  lQuery:=ConcatRestParam(lQuery,'useDomainAdminAccess',cRESTBooleans[aUseDomainAdminAccess]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('patch',lURL,aRequest.Serialize);
  Result:=TTeamDriveServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TTeamDrive.Deserialize(lResponse.Content);
end;


end.
