{ -----------------------------------------------------------------------
  Do not edit !
  
  This file was automatically generated on 2026-10-03 10:20.
  Used command-line parameters:
     -s drive -o drive -q
  Source OpenAPI document data:
    Title: Google Drive API
    Version: v3
  -----------------------------------------------------------------------}
unit drive.Serializer;

interface

{$mode objfpc}
{$h+}
{$modeswitch typehelpers}


uses
  Types,
  fpJSON,
  drive.Dto;

Type
  TUserSerializer = class helper for TUser
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TUser; overload; static;
    class function Deserialize(aJSON : String) : TUser; overload; static;
  end;
  
  TAboutSerializer = class helper for TAbout
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TAbout; overload; static;
    class function Deserialize(aJSON : String) : TAbout; overload; static;
  end;
  
  TAccessProposalRoleAndViewSerializer = class helper for TAccessProposalRoleAndView
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TAccessProposalRoleAndView; overload; static;
    class function Deserialize(aJSON : String) : TAccessProposalRoleAndView; overload; static;
  end;
  
  TAccessProposalSerializer = class helper for TAccessProposal
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TAccessProposal; overload; static;
    class function Deserialize(aJSON : String) : TAccessProposal; overload; static;
  end;
  
  TAddReviewerSerializer = class helper for TAddReviewer
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TAddReviewer; overload; static;
    class function Deserialize(aJSON : String) : TAddReviewer; overload; static;
  end;
  
  TAppIconsSerializer = class helper for TAppIcons
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TAppIcons; overload; static;
    class function Deserialize(aJSON : String) : TAppIcons; overload; static;
  end;
  
  TAppSerializer = class helper for TApp
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TApp; overload; static;
    class function Deserialize(aJSON : String) : TApp; overload; static;
  end;
  
  TAppListSerializer = class helper for TAppList
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TAppList; overload; static;
    class function Deserialize(aJSON : String) : TAppList; overload; static;
  end;
  
  TReviewerResponseSerializer = class helper for TReviewerResponse
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TReviewerResponse; overload; static;
    class function Deserialize(aJSON : String) : TReviewerResponse; overload; static;
  end;
  
  TApprovalSerializer = class helper for TApproval
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TApproval; overload; static;
    class function Deserialize(aJSON : String) : TApproval; overload; static;
  end;
  
  TApprovalListSerializer = class helper for TApprovalList
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TApprovalList; overload; static;
    class function Deserialize(aJSON : String) : TApprovalList; overload; static;
  end;
  
  TApproveApprovalRequestSerializer = class helper for TApproveApprovalRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TApproveApprovalRequest; overload; static;
    class function Deserialize(aJSON : String) : TApproveApprovalRequest; overload; static;
  end;
  
  TCancelApprovalRequestSerializer = class helper for TCancelApprovalRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TCancelApprovalRequest; overload; static;
    class function Deserialize(aJSON : String) : TCancelApprovalRequest; overload; static;
  end;
  
  TDriveSerializer = class helper for TDrive
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TDrive; overload; static;
    class function Deserialize(aJSON : String) : TDrive; overload; static;
  end;
  
  TDecryptionMetadataSerializer = class helper for TDecryptionMetadata
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TDecryptionMetadata; overload; static;
    class function Deserialize(aJSON : String) : TDecryptionMetadata; overload; static;
  end;
  
  TClientEncryptionDetailsSerializer = class helper for TClientEncryptionDetails
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TClientEncryptionDetails; overload; static;
    class function Deserialize(aJSON : String) : TClientEncryptionDetails; overload; static;
  end;
  
  TContentRestrictionSerializer = class helper for TContentRestriction
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TContentRestriction; overload; static;
    class function Deserialize(aJSON : String) : TContentRestriction; overload; static;
  end;
  
  TDownloadRestrictionSerializer = class helper for TDownloadRestriction
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TDownloadRestriction; overload; static;
    class function Deserialize(aJSON : String) : TDownloadRestriction; overload; static;
  end;
  
  TDownloadRestrictionsMetadataSerializer = class helper for TDownloadRestrictionsMetadata
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TDownloadRestrictionsMetadata; overload; static;
    class function Deserialize(aJSON : String) : TDownloadRestrictionsMetadata; overload; static;
  end;
  
  TPermissionSerializer = class helper for TPermission
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TPermission; overload; static;
    class function Deserialize(aJSON : String) : TPermission; overload; static;
  end;
  
  TFileSerializer = class helper for TFile
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TFile; overload; static;
    class function Deserialize(aJSON : String) : TFile; overload; static;
  end;
  
  TTeamDriveSerializer = class helper for TTeamDrive
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TTeamDrive; overload; static;
    class function Deserialize(aJSON : String) : TTeamDrive; overload; static;
  end;
  
  TChangeSerializer = class helper for TChange
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TChange; overload; static;
    class function Deserialize(aJSON : String) : TChange; overload; static;
  end;
  
  TChangeListSerializer = class helper for TChangeList
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TChangeList; overload; static;
    class function Deserialize(aJSON : String) : TChangeList; overload; static;
  end;
  
  TChannelSerializer = class helper for TChannel
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TChannel; overload; static;
    class function Deserialize(aJSON : String) : TChannel; overload; static;
  end;
  
  TReplySerializer = class helper for TReply
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TReply; overload; static;
    class function Deserialize(aJSON : String) : TReply; overload; static;
  end;
  
  TCommentSerializer = class helper for TComment
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TComment; overload; static;
    class function Deserialize(aJSON : String) : TComment; overload; static;
  end;
  
  TCommentApprovalRequestSerializer = class helper for TCommentApprovalRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TCommentApprovalRequest; overload; static;
    class function Deserialize(aJSON : String) : TCommentApprovalRequest; overload; static;
  end;
  
  TCommentListSerializer = class helper for TCommentList
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TCommentList; overload; static;
    class function Deserialize(aJSON : String) : TCommentList; overload; static;
  end;
  
  TDeclineApprovalRequestSerializer = class helper for TDeclineApprovalRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TDeclineApprovalRequest; overload; static;
    class function Deserialize(aJSON : String) : TDeclineApprovalRequest; overload; static;
  end;
  
  TDriveListSerializer = class helper for TDriveList
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TDriveList; overload; static;
    class function Deserialize(aJSON : String) : TDriveList; overload; static;
  end;
  
  TFileListSerializer = class helper for TFileList
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TFileList; overload; static;
    class function Deserialize(aJSON : String) : TFileList; overload; static;
  end;
  
  TGenerateCseTokenResponseSerializer = class helper for TGenerateCseTokenResponse
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TGenerateCseTokenResponse; overload; static;
    class function Deserialize(aJSON : String) : TGenerateCseTokenResponse; overload; static;
  end;
  
  TGeneratedIdsSerializer = class helper for TGeneratedIds
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TGeneratedIds; overload; static;
    class function Deserialize(aJSON : String) : TGeneratedIds; overload; static;
  end;
  
  TLabelSerializer = class helper for TLabel
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TLabel; overload; static;
    class function Deserialize(aJSON : String) : TLabel; overload; static;
  end;
  
  TLabelFieldSerializer = class helper for TLabelField
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TLabelField; overload; static;
    class function Deserialize(aJSON : String) : TLabelField; overload; static;
  end;
  
  TLabelFieldModificationSerializer = class helper for TLabelFieldModification
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TLabelFieldModification; overload; static;
    class function Deserialize(aJSON : String) : TLabelFieldModification; overload; static;
  end;
  
  TLabelListSerializer = class helper for TLabelList
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TLabelList; overload; static;
    class function Deserialize(aJSON : String) : TLabelList; overload; static;
  end;
  
  TLabelModificationSerializer = class helper for TLabelModification
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TLabelModification; overload; static;
    class function Deserialize(aJSON : String) : TLabelModification; overload; static;
  end;
  
  TListAccessProposalsResponseSerializer = class helper for TListAccessProposalsResponse
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TListAccessProposalsResponse; overload; static;
    class function Deserialize(aJSON : String) : TListAccessProposalsResponse; overload; static;
  end;
  
  TModifyLabelsRequestSerializer = class helper for TModifyLabelsRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TModifyLabelsRequest; overload; static;
    class function Deserialize(aJSON : String) : TModifyLabelsRequest; overload; static;
  end;
  
  TModifyLabelsResponseSerializer = class helper for TModifyLabelsResponse
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TModifyLabelsResponse; overload; static;
    class function Deserialize(aJSON : String) : TModifyLabelsResponse; overload; static;
  end;
  
  TStatusSerializer = class helper for TStatus
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TStatus; overload; static;
    class function Deserialize(aJSON : String) : TStatus; overload; static;
  end;
  
  TOperationSerializer = class helper for TOperation
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TOperation; overload; static;
    class function Deserialize(aJSON : String) : TOperation; overload; static;
  end;
  
  TPermissionListSerializer = class helper for TPermissionList
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TPermissionList; overload; static;
    class function Deserialize(aJSON : String) : TPermissionList; overload; static;
  end;
  
  TReplaceReviewerSerializer = class helper for TReplaceReviewer
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TReplaceReviewer; overload; static;
    class function Deserialize(aJSON : String) : TReplaceReviewer; overload; static;
  end;
  
  TReassignApprovalRequestSerializer = class helper for TReassignApprovalRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TReassignApprovalRequest; overload; static;
    class function Deserialize(aJSON : String) : TReassignApprovalRequest; overload; static;
  end;
  
  TReplyListSerializer = class helper for TReplyList
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TReplyList; overload; static;
    class function Deserialize(aJSON : String) : TReplyList; overload; static;
  end;
  
  TResolveAccessProposalRequestSerializer = class helper for TResolveAccessProposalRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TResolveAccessProposalRequest; overload; static;
    class function Deserialize(aJSON : String) : TResolveAccessProposalRequest; overload; static;
  end;
  
  TRevisionSerializer = class helper for TRevision
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TRevision; overload; static;
    class function Deserialize(aJSON : String) : TRevision; overload; static;
  end;
  
  TRevisionListSerializer = class helper for TRevisionList
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TRevisionList; overload; static;
    class function Deserialize(aJSON : String) : TRevisionList; overload; static;
  end;
  
  TStartApprovalRequestSerializer = class helper for TStartApprovalRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TStartApprovalRequest; overload; static;
    class function Deserialize(aJSON : String) : TStartApprovalRequest; overload; static;
  end;
  
  TStartPageTokenSerializer = class helper for TStartPageToken
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TStartPageToken; overload; static;
    class function Deserialize(aJSON : String) : TStartPageToken; overload; static;
  end;
  
  TTeamDriveListSerializer = class helper for TTeamDriveList
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TTeamDriveList; overload; static;
    class function Deserialize(aJSON : String) : TTeamDriveList; overload; static;
  end;
  
  TAccessProposalRoleAndViewArraySerializer = type helper for TAccessProposalRoleAndViewArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TAccessProposalRoleAndViewArray; overload; static;
    class function Deserialize(aJSON : String) : TAccessProposalRoleAndViewArray; overload; static;
  end;
  TAccessProposalArraySerializer = type helper for TAccessProposalArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TAccessProposalArray; overload; static;
    class function Deserialize(aJSON : String) : TAccessProposalArray; overload; static;
  end;
  TAddReviewerArraySerializer = type helper for TAddReviewerArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TAddReviewerArray; overload; static;
    class function Deserialize(aJSON : String) : TAddReviewerArray; overload; static;
  end;
  TAppIconsArraySerializer = type helper for TAppIconsArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TAppIconsArray; overload; static;
    class function Deserialize(aJSON : String) : TAppIconsArray; overload; static;
  end;
  TApprovalArraySerializer = type helper for TApprovalArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TApprovalArray; overload; static;
    class function Deserialize(aJSON : String) : TApprovalArray; overload; static;
  end;
  TAppArraySerializer = type helper for TAppArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TAppArray; overload; static;
    class function Deserialize(aJSON : String) : TAppArray; overload; static;
  end;
  TChangeArraySerializer = type helper for TChangeArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TChangeArray; overload; static;
    class function Deserialize(aJSON : String) : TChangeArray; overload; static;
  end;
  TCommentArraySerializer = type helper for TCommentArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TCommentArray; overload; static;
    class function Deserialize(aJSON : String) : TCommentArray; overload; static;
  end;
  TContentRestrictionArraySerializer = type helper for TContentRestrictionArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TContentRestrictionArray; overload; static;
    class function Deserialize(aJSON : String) : TContentRestrictionArray; overload; static;
  end;
  TDriveArraySerializer = type helper for TDriveArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TDriveArray; overload; static;
    class function Deserialize(aJSON : String) : TDriveArray; overload; static;
  end;
  TFileArraySerializer = type helper for TFileArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TFileArray; overload; static;
    class function Deserialize(aJSON : String) : TFileArray; overload; static;
  end;
  TLabelFieldModificationArraySerializer = type helper for TLabelFieldModificationArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TLabelFieldModificationArray; overload; static;
    class function Deserialize(aJSON : String) : TLabelFieldModificationArray; overload; static;
  end;
  TLabelModificationArraySerializer = type helper for TLabelModificationArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TLabelModificationArray; overload; static;
    class function Deserialize(aJSON : String) : TLabelModificationArray; overload; static;
  end;
  TLabelArraySerializer = type helper for TLabelArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TLabelArray; overload; static;
    class function Deserialize(aJSON : String) : TLabelArray; overload; static;
  end;
  stringArraySerializer = type helper for stringArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : stringArray; overload; static;
    class function Deserialize(aJSON : String) : stringArray; overload; static;
  end;
  TPermissionArraySerializer = type helper for TPermissionArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TPermissionArray; overload; static;
    class function Deserialize(aJSON : String) : TPermissionArray; overload; static;
  end;
  TReplaceReviewerArraySerializer = type helper for TReplaceReviewerArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TReplaceReviewerArray; overload; static;
    class function Deserialize(aJSON : String) : TReplaceReviewerArray; overload; static;
  end;
  TReplyArraySerializer = type helper for TReplyArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TReplyArray; overload; static;
    class function Deserialize(aJSON : String) : TReplyArray; overload; static;
  end;
  TReviewerResponseArraySerializer = type helper for TReviewerResponseArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TReviewerResponseArray; overload; static;
    class function Deserialize(aJSON : String) : TReviewerResponseArray; overload; static;
  end;
  TRevisionArraySerializer = type helper for TRevisionArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TRevisionArray; overload; static;
    class function Deserialize(aJSON : String) : TRevisionArray; overload; static;
  end;
  TTeamDriveArraySerializer = type helper for TTeamDriveArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TTeamDriveArray; overload; static;
    class function Deserialize(aJSON : String) : TTeamDriveArray; overload; static;
  end;
  TUserArraySerializer = type helper for TUserArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TUserArray; overload; static;
    class function Deserialize(aJSON : String) : TUserArray; overload; static;
  end;
implementation

uses Generics.Collections, SysUtils, DateUtils, StrUtils;

function ISO8601ToDateDef(S: String; aDefault : TDateTime; aConvertUTC: Boolean = True) : TDateTime;

begin
  if (S='') then
    Exit(aDefault);
  try
    Result:=ISO8601ToDate(S,aConvertUTC);
  except
    Result:=aDefault;
  end;
end;

function ISO8601ToDateOnlyDef(S: String; aDefault : TDateTime) : TDateTime;

begin
  Result:=DateOf(ISO8601ToDateDef(S,aDefault,True));
end;

function DateOnlyToISO8601(aDate : TDateTime) : String;

begin
  Result:=FormatDateTime('yyyy"-"mm"-"dd',aDate);
end;

function JSONDataAsString(aData: TJSONData) : String;

begin
  if aData=Nil then
    Result:=''
  else
    Result:=aData.AsJSON;
end;

function TUserSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
  except
    Result.Free;
    raise;
  end;
end;

function TUserSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TUserSerializer.Deserialize(aJSON : TJSONObject) : TUser;

begin
  Result := TUser.Create;
  If (aJSON=Nil) then
    exit;
  Result.displayName:=aJSON.Get('displayName','');
  Result.emailAddress:=aJSON.Get('emailAddress','');
  Result.kind:=aJSON.Get('kind','');
  Result.me:=aJSON.Get('me',False);
  Result.permissionId:=aJSON.Get('permissionId','');
  Result.photoLink:=aJSON.Get('photoLink','');
end;

class function TUserSerializer.Deserialize(aJSON : String) : TUser;

var
  lObj : TJSONObject;
begin
  Result := Default(TUser);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TAboutSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('appInstalled',appInstalled);
    Result.Add('canCreateDrives',canCreateDrives);
    Result.Add('canCreateTeamDrives',canCreateTeamDrives);
    Arr:=TJSONArray.Create;
    Result.Add('driveThemes',Arr);
    For I:=0 to Length(driveThemes)-1 do
      Arr.Add(GetJSON(driveThemes[i]));
    if (exportFormats<>'') then
      Result.Add('exportFormats',GetJSON(exportFormats));
    Arr:=TJSONArray.Create;
    Result.Add('folderColorPalette',Arr);
    For I:=0 to Length(folderColorPalette)-1 do
      Arr.Add(folderColorPalette[i]);
    if (importFormats<>'') then
      Result.Add('importFormats',GetJSON(importFormats));
    Result.Add('kind',kind);
    if (maxImportSizes<>'') then
      Result.Add('maxImportSizes',GetJSON(maxImportSizes));
    Result.Add('maxUploadSize',maxUploadSize);
    if (storageQuota<>'') then
      Result.Add('storageQuota',GetJSON(storageQuota));
    Arr:=TJSONArray.Create;
    Result.Add('teamDriveThemes',Arr);
    For I:=0 to Length(teamDriveThemes)-1 do
      Arr.Add(GetJSON(teamDriveThemes[i]));
    if Assigned(user) then
      Result.Add('user',user.SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TAboutSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TAboutSerializer.Deserialize(aJSON : TJSONObject) : TAbout;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TAbout.Create;
  If (aJSON=Nil) then
    exit;
  Result.appInstalled:=aJSON.Get('appInstalled',False);
  Result.canCreateDrives:=aJSON.Get('canCreateDrives',False);
  Result.canCreateTeamDrives:=aJSON.Get('canCreateTeamDrives',False);
  lArr:=aJSON.Get('driveThemes',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.driveThemes,lArr.Count);
    For I:=0 to Length(Result.driveThemes)-1 do
      Result.driveThemes[i]:=lArr[i].AsJSON;
    end;
  Result.exportFormats:=JSONDataAsString(aJSON.Get('exportFormats',TJSONObject(Nil)));
  lArr:=aJSON.Get('folderColorPalette',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.folderColorPalette,lArr.Count);
    For I:=0 to Length(Result.folderColorPalette)-1 do
      Result.folderColorPalette[i]:=lArr[i].Asstring;
    end;
  Result.importFormats:=JSONDataAsString(aJSON.Get('importFormats',TJSONObject(Nil)));
  Result.kind:=aJSON.Get('kind','');
  Result.maxImportSizes:=JSONDataAsString(aJSON.Get('maxImportSizes',TJSONObject(Nil)));
  Result.maxUploadSize:=aJSON.Get('maxUploadSize','');
  Result.storageQuota:=JSONDataAsString(aJSON.Get('storageQuota',TJSONObject(Nil)));
  lArr:=aJSON.Get('teamDriveThemes',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.teamDriveThemes,lArr.Count);
    For I:=0 to Length(Result.teamDriveThemes)-1 do
      Result.teamDriveThemes[i]:=lArr[i].AsJSON;
    end;
  Result.user:=TUser.Deserialize(aJSON.Get('user',TJSONObject(Nil)));
end;

class function TAboutSerializer.Deserialize(aJSON : String) : TAbout;

var
  lObj : TJSONObject;
begin
  Result := Default(TAbout);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TAccessProposalRoleAndViewSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('role',role);
    Result.Add('view',view);
  except
    Result.Free;
    raise;
  end;
end;

function TAccessProposalRoleAndViewSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TAccessProposalRoleAndViewSerializer.Deserialize(aJSON : TJSONObject) : TAccessProposalRoleAndView;

begin
  Result := TAccessProposalRoleAndView.Create;
  If (aJSON=Nil) then
    exit;
  Result.role:=aJSON.Get('role','');
  Result.view:=aJSON.Get('view','');
end;

class function TAccessProposalRoleAndViewSerializer.Deserialize(aJSON : String) : TAccessProposalRoleAndView;

var
  lObj : TJSONObject;
begin
  Result := Default(TAccessProposalRoleAndView);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TAccessProposalSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('createTime',createTime);
    Result.Add('fileId',fileId);
    Result.Add('proposalId',proposalId);
    Result.Add('recipientEmailAddress',recipientEmailAddress);
    Result.Add('requesterEmailAddress',requesterEmailAddress);
    Result.Add('requestMessage',requestMessage);
    Arr:=TJSONArray.Create;
    Result.Add('rolesAndViews',Arr);
    For I:=0 to Length(rolesAndViews)-1 do
      Arr.Add(rolesAndViews[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TAccessProposalSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TAccessProposalSerializer.Deserialize(aJSON : TJSONObject) : TAccessProposal;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TAccessProposal.Create;
  If (aJSON=Nil) then
    exit;
  Result.createTime:=aJSON.Get('createTime','');
  Result.fileId:=aJSON.Get('fileId','');
  Result.proposalId:=aJSON.Get('proposalId','');
  Result.recipientEmailAddress:=aJSON.Get('recipientEmailAddress','');
  Result.requesterEmailAddress:=aJSON.Get('requesterEmailAddress','');
  Result.requestMessage:=aJSON.Get('requestMessage','');
  lArr:=aJSON.Get('rolesAndViews',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.rolesAndViews,lArr.Count);
    For I:=0 to Length(Result.rolesAndViews)-1 do
      Result.rolesAndViews[i]:=TAccessProposalRoleAndView.Deserialize(lArr[i] as TJSONObject);
    end;
end;

class function TAccessProposalSerializer.Deserialize(aJSON : String) : TAccessProposal;

var
  lObj : TJSONObject;
begin
  Result := Default(TAccessProposal);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TAddReviewerSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('addedReviewerEmail',addedReviewerEmail);
  except
    Result.Free;
    raise;
  end;
end;

function TAddReviewerSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TAddReviewerSerializer.Deserialize(aJSON : TJSONObject) : TAddReviewer;

begin
  Result := TAddReviewer.Create;
  If (aJSON=Nil) then
    exit;
  Result.addedReviewerEmail:=aJSON.Get('addedReviewerEmail','');
end;

class function TAddReviewerSerializer.Deserialize(aJSON : String) : TAddReviewer;

var
  lObj : TJSONObject;
begin
  Result := Default(TAddReviewer);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TAppIconsSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('category',category);
    Result.Add('iconUrl',iconUrl);
    Result.Add('size',size);
  except
    Result.Free;
    raise;
  end;
end;

function TAppIconsSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TAppIconsSerializer.Deserialize(aJSON : TJSONObject) : TAppIcons;

begin
  Result := TAppIcons.Create;
  If (aJSON=Nil) then
    exit;
  Result.category:=aJSON.Get('category','');
  Result.iconUrl:=aJSON.Get('iconUrl','');
  Result.size:=aJSON.Get('size',0);
end;

class function TAppIconsSerializer.Deserialize(aJSON : String) : TAppIcons;

var
  lObj : TJSONObject;
begin
  Result := Default(TAppIcons);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TAppSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('authorized',authorized);
    Result.Add('createInFolderTemplate',createInFolderTemplate);
    Result.Add('createUrl',createUrl);
    Result.Add('hasDriveWideScope',hasDriveWideScope);
    Arr:=TJSONArray.Create;
    Result.Add('icons',Arr);
    For I:=0 to Length(icons)-1 do
      Arr.Add(icons[i].SerializeObject);
    Result.Add('id',id);
    Result.Add('installed',installed);
    Result.Add('longDescription',longDescription);
    Result.Add('name',name);
    Result.Add('objectType',objectType);
    Result.Add('openUrlTemplate',openUrlTemplate);
    Arr:=TJSONArray.Create;
    Result.Add('primaryFileExtensions',Arr);
    For I:=0 to Length(primaryFileExtensions)-1 do
      Arr.Add(primaryFileExtensions[i]);
    Arr:=TJSONArray.Create;
    Result.Add('primaryMimeTypes',Arr);
    For I:=0 to Length(primaryMimeTypes)-1 do
      Arr.Add(primaryMimeTypes[i]);
    Result.Add('productId',productId);
    Result.Add('productUrl',productUrl);
    Arr:=TJSONArray.Create;
    Result.Add('secondaryFileExtensions',Arr);
    For I:=0 to Length(secondaryFileExtensions)-1 do
      Arr.Add(secondaryFileExtensions[i]);
    Arr:=TJSONArray.Create;
    Result.Add('secondaryMimeTypes',Arr);
    For I:=0 to Length(secondaryMimeTypes)-1 do
      Arr.Add(secondaryMimeTypes[i]);
    Result.Add('shortDescription',shortDescription);
    Result.Add('supportsCreate',supportsCreate);
    Result.Add('supportsImport',supportsImport);
    Result.Add('supportsMultiOpen',supportsMultiOpen);
    Result.Add('supportsOfflineCreate',supportsOfflineCreate);
    Result.Add('useByDefault',useByDefault);
  except
    Result.Free;
    raise;
  end;
end;

function TAppSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TAppSerializer.Deserialize(aJSON : TJSONObject) : TApp;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TApp.Create;
  If (aJSON=Nil) then
    exit;
  Result.authorized:=aJSON.Get('authorized',False);
  Result.createInFolderTemplate:=aJSON.Get('createInFolderTemplate','');
  Result.createUrl:=aJSON.Get('createUrl','');
  Result.hasDriveWideScope:=aJSON.Get('hasDriveWideScope',False);
  lArr:=aJSON.Get('icons',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.icons,lArr.Count);
    For I:=0 to Length(Result.icons)-1 do
      Result.icons[i]:=TAppIcons.Deserialize(lArr[i] as TJSONObject);
    end;
  Result.id:=aJSON.Get('id','');
  Result.installed:=aJSON.Get('installed',False);
  Result.kind:=aJSON.Get('kind','');
  Result.longDescription:=aJSON.Get('longDescription','');
  Result.name:=aJSON.Get('name','');
  Result.objectType:=aJSON.Get('objectType','');
  Result.openUrlTemplate:=aJSON.Get('openUrlTemplate','');
  lArr:=aJSON.Get('primaryFileExtensions',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.primaryFileExtensions,lArr.Count);
    For I:=0 to Length(Result.primaryFileExtensions)-1 do
      Result.primaryFileExtensions[i]:=lArr[i].Asstring;
    end;
  lArr:=aJSON.Get('primaryMimeTypes',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.primaryMimeTypes,lArr.Count);
    For I:=0 to Length(Result.primaryMimeTypes)-1 do
      Result.primaryMimeTypes[i]:=lArr[i].Asstring;
    end;
  Result.productId:=aJSON.Get('productId','');
  Result.productUrl:=aJSON.Get('productUrl','');
  lArr:=aJSON.Get('secondaryFileExtensions',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.secondaryFileExtensions,lArr.Count);
    For I:=0 to Length(Result.secondaryFileExtensions)-1 do
      Result.secondaryFileExtensions[i]:=lArr[i].Asstring;
    end;
  lArr:=aJSON.Get('secondaryMimeTypes',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.secondaryMimeTypes,lArr.Count);
    For I:=0 to Length(Result.secondaryMimeTypes)-1 do
      Result.secondaryMimeTypes[i]:=lArr[i].Asstring;
    end;
  Result.shortDescription:=aJSON.Get('shortDescription','');
  Result.supportsCreate:=aJSON.Get('supportsCreate',False);
  Result.supportsImport:=aJSON.Get('supportsImport',False);
  Result.supportsMultiOpen:=aJSON.Get('supportsMultiOpen',False);
  Result.supportsOfflineCreate:=aJSON.Get('supportsOfflineCreate',False);
  Result.useByDefault:=aJSON.Get('useByDefault',False);
end;

class function TAppSerializer.Deserialize(aJSON : String) : TApp;

var
  lObj : TJSONObject;
begin
  Result := Default(TApp);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TAppListSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('defaultAppIds',Arr);
    For I:=0 to Length(defaultAppIds)-1 do
      Arr.Add(defaultAppIds[i]);
    Arr:=TJSONArray.Create;
    Result.Add('items',Arr);
    For I:=0 to Length(items)-1 do
      Arr.Add(items[i].SerializeObject);
    Result.Add('selfLink',selfLink);
  except
    Result.Free;
    raise;
  end;
end;

function TAppListSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TAppListSerializer.Deserialize(aJSON : TJSONObject) : TAppList;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TAppList.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('defaultAppIds',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.defaultAppIds,lArr.Count);
    For I:=0 to Length(Result.defaultAppIds)-1 do
      Result.defaultAppIds[i]:=lArr[i].Asstring;
    end;
  lArr:=aJSON.Get('items',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.items,lArr.Count);
    For I:=0 to Length(Result.items)-1 do
      Result.items[i]:=TApp.Deserialize(lArr[i] as TJSONObject);
    end;
  Result.kind:=aJSON.Get('kind','');
  Result.selfLink:=aJSON.Get('selfLink','');
end;

class function TAppListSerializer.Deserialize(aJSON : String) : TAppList;

var
  lObj : TJSONObject;
begin
  Result := Default(TAppList);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TReviewerResponseSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('kind',kind);
    Result.Add('response',response);
    if Assigned(reviewer) then
      Result.Add('reviewer',reviewer.SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TReviewerResponseSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TReviewerResponseSerializer.Deserialize(aJSON : TJSONObject) : TReviewerResponse;

begin
  Result := TReviewerResponse.Create;
  If (aJSON=Nil) then
    exit;
  Result.kind:=aJSON.Get('kind','');
  Result.response:=aJSON.Get('response','');
  Result.reviewer:=TUser.Deserialize(aJSON.Get('reviewer',TJSONObject(Nil)));
end;

class function TReviewerResponseSerializer.Deserialize(aJSON : String) : TReviewerResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TReviewerResponse);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TApprovalSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('approvalId',approvalId);
    Result.Add('dueTime',dueTime);
    if Assigned(initiator) then
      Result.Add('initiator',initiator.SerializeObject);
    Result.Add('kind',kind);
    Arr:=TJSONArray.Create;
    Result.Add('reviewerResponses',Arr);
    For I:=0 to Length(reviewerResponses)-1 do
      Arr.Add(reviewerResponses[i].SerializeObject);
    Result.Add('targetFileId',targetFileId);
  except
    Result.Free;
    raise;
  end;
end;

function TApprovalSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TApprovalSerializer.Deserialize(aJSON : TJSONObject) : TApproval;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TApproval.Create;
  If (aJSON=Nil) then
    exit;
  Result.approvalId:=aJSON.Get('approvalId','');
  Result.completeTime:=aJSON.Get('completeTime','');
  Result.createTime:=aJSON.Get('createTime','');
  Result.dueTime:=aJSON.Get('dueTime','');
  Result.fileContentChangeBehavior:=aJSON.Get('fileContentChangeBehavior','');
  Result.initiator:=TUser.Deserialize(aJSON.Get('initiator',TJSONObject(Nil)));
  Result.kind:=aJSON.Get('kind','');
  Result.modifyTime:=aJSON.Get('modifyTime','');
  lArr:=aJSON.Get('reviewerResponses',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.reviewerResponses,lArr.Count);
    For I:=0 to Length(Result.reviewerResponses)-1 do
      Result.reviewerResponses[i]:=TReviewerResponse.Deserialize(lArr[i] as TJSONObject);
    end;
  Result.status:=aJSON.Get('status','');
  Result.targetFileId:=aJSON.Get('targetFileId','');
end;

class function TApprovalSerializer.Deserialize(aJSON : String) : TApproval;

var
  lObj : TJSONObject;
begin
  Result := Default(TApproval);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TApprovalListSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('items',Arr);
    For I:=0 to Length(items)-1 do
      Arr.Add(items[i].SerializeObject);
    Result.Add('kind',kind);
    Result.Add('nextPageToken',nextPageToken);
  except
    Result.Free;
    raise;
  end;
end;

function TApprovalListSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TApprovalListSerializer.Deserialize(aJSON : TJSONObject) : TApprovalList;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TApprovalList.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('items',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.items,lArr.Count);
    For I:=0 to Length(Result.items)-1 do
      Result.items[i]:=TApproval.Deserialize(lArr[i] as TJSONObject);
    end;
  Result.kind:=aJSON.Get('kind','');
  Result.nextPageToken:=aJSON.Get('nextPageToken','');
end;

class function TApprovalListSerializer.Deserialize(aJSON : String) : TApprovalList;

var
  lObj : TJSONObject;
begin
  Result := Default(TApprovalList);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TApproveApprovalRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('message',message);
  except
    Result.Free;
    raise;
  end;
end;

function TApproveApprovalRequestSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TApproveApprovalRequestSerializer.Deserialize(aJSON : TJSONObject) : TApproveApprovalRequest;

begin
  Result := TApproveApprovalRequest.Create;
  If (aJSON=Nil) then
    exit;
  Result.message:=aJSON.Get('message','');
end;

class function TApproveApprovalRequestSerializer.Deserialize(aJSON : String) : TApproveApprovalRequest;

var
  lObj : TJSONObject;
begin
  Result := Default(TApproveApprovalRequest);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TCancelApprovalRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('message',message);
  except
    Result.Free;
    raise;
  end;
end;

function TCancelApprovalRequestSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TCancelApprovalRequestSerializer.Deserialize(aJSON : TJSONObject) : TCancelApprovalRequest;

begin
  Result := TCancelApprovalRequest.Create;
  If (aJSON=Nil) then
    exit;
  Result.message:=aJSON.Get('message','');
end;

class function TCancelApprovalRequestSerializer.Deserialize(aJSON : String) : TCancelApprovalRequest;

var
  lObj : TJSONObject;
begin
  Result := Default(TCancelApprovalRequest);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TDriveSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    if (backgroundImageFile<>'') then
      Result.Add('backgroundImageFile',GetJSON(backgroundImageFile));
    Result.Add('colorRgb',colorRgb);
    if (createdTime<>0) then
      Result.Add('createdTime',DateToISO8601(createdTime,True));
    Result.Add('hidden',hidden);
    Result.Add('name',name);
    if (restrictions<>'') then
      Result.Add('restrictions',GetJSON(restrictions));
    Result.Add('themeId',themeId);
  except
    Result.Free;
    raise;
  end;
end;

function TDriveSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TDriveSerializer.Deserialize(aJSON : TJSONObject) : TDrive;

begin
  Result := TDrive.Create;
  If (aJSON=Nil) then
    exit;
  Result.backgroundImageFile:=JSONDataAsString(aJSON.Get('backgroundImageFile',TJSONObject(Nil)));
  Result.backgroundImageLink:=aJSON.Get('backgroundImageLink','');
  Result.capabilities:=JSONDataAsString(aJSON.Get('capabilities',TJSONObject(Nil)));
  Result.colorRgb:=aJSON.Get('colorRgb','');
  Result.createdTime:=ISO8601ToDateDef(aJSON.Get('createdTime',''),0,True);
  Result.hidden:=aJSON.Get('hidden',False);
  Result.id:=aJSON.Get('id','');
  Result.kind:=aJSON.Get('kind','');
  Result.name:=aJSON.Get('name','');
  Result.orgUnitId:=aJSON.Get('orgUnitId','');
  Result.restrictions:=JSONDataAsString(aJSON.Get('restrictions',TJSONObject(Nil)));
  Result.themeId:=aJSON.Get('themeId','');
end;

class function TDriveSerializer.Deserialize(aJSON : String) : TDrive;

var
  lObj : TJSONObject;
begin
  Result := Default(TDrive);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TDecryptionMetadataSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('aes256GcmChunkSize',aes256GcmChunkSize);
    Result.Add('encryptionResourceKeyHash',encryptionResourceKeyHash);
    Result.Add('jwt',jwt);
    Result.Add('kaclsId',kaclsId);
    Result.Add('kaclsName',kaclsName);
    Result.Add('keyFormat',keyFormat);
    Result.Add('wrappedKey',wrappedKey);
  except
    Result.Free;
    raise;
  end;
end;

function TDecryptionMetadataSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TDecryptionMetadataSerializer.Deserialize(aJSON : TJSONObject) : TDecryptionMetadata;

begin
  Result := TDecryptionMetadata.Create;
  If (aJSON=Nil) then
    exit;
  Result.aes256GcmChunkSize:=aJSON.Get('aes256GcmChunkSize','');
  Result.encryptionResourceKeyHash:=aJSON.Get('encryptionResourceKeyHash','');
  Result.jwt:=aJSON.Get('jwt','');
  Result.kaclsId:=aJSON.Get('kaclsId','');
  Result.kaclsName:=aJSON.Get('kaclsName','');
  Result.keyFormat:=aJSON.Get('keyFormat','');
  Result.wrappedKey:=aJSON.Get('wrappedKey','');
end;

class function TDecryptionMetadataSerializer.Deserialize(aJSON : String) : TDecryptionMetadata;

var
  lObj : TJSONObject;
begin
  Result := Default(TDecryptionMetadata);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TClientEncryptionDetailsSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    if Assigned(decryptionMetadata) then
      Result.Add('decryptionMetadata',decryptionMetadata.SerializeObject);
    Result.Add('encryptionState',encryptionState);
  except
    Result.Free;
    raise;
  end;
end;

function TClientEncryptionDetailsSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TClientEncryptionDetailsSerializer.Deserialize(aJSON : TJSONObject) : TClientEncryptionDetails;

begin
  Result := TClientEncryptionDetails.Create;
  If (aJSON=Nil) then
    exit;
  Result.decryptionMetadata:=TDecryptionMetadata.Deserialize(aJSON.Get('decryptionMetadata',TJSONObject(Nil)));
  Result.encryptionState:=aJSON.Get('encryptionState','');
end;

class function TClientEncryptionDetailsSerializer.Deserialize(aJSON : String) : TClientEncryptionDetails;

var
  lObj : TJSONObject;
begin
  Result := Default(TClientEncryptionDetails);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TContentRestrictionSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('ownerRestricted',ownerRestricted);
    Result.Add('readOnly',readOnly);
    Result.Add('reason',reason);
    if (restrictionTime<>0) then
      Result.Add('restrictionTime',DateToISO8601(restrictionTime,True));
  except
    Result.Free;
    raise;
  end;
end;

function TContentRestrictionSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TContentRestrictionSerializer.Deserialize(aJSON : TJSONObject) : TContentRestriction;

begin
  Result := TContentRestriction.Create;
  If (aJSON=Nil) then
    exit;
  Result.ownerRestricted:=aJSON.Get('ownerRestricted',False);
  Result.readOnly:=aJSON.Get('readOnly',False);
  Result.reason:=aJSON.Get('reason','');
  Result.restrictingUser:=TUser.Deserialize(aJSON.Get('restrictingUser',TJSONObject(Nil)));
  Result.restrictionTime:=ISO8601ToDateDef(aJSON.Get('restrictionTime',''),0,True);
  Result.systemRestricted:=aJSON.Get('systemRestricted',False);
  Result.type_:=aJSON.Get('type','');
end;

class function TContentRestrictionSerializer.Deserialize(aJSON : String) : TContentRestriction;

var
  lObj : TJSONObject;
begin
  Result := Default(TContentRestriction);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TDownloadRestrictionSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('restrictedForReaders',restrictedForReaders);
    Result.Add('restrictedForWriters',restrictedForWriters);
  except
    Result.Free;
    raise;
  end;
end;

function TDownloadRestrictionSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TDownloadRestrictionSerializer.Deserialize(aJSON : TJSONObject) : TDownloadRestriction;

begin
  Result := TDownloadRestriction.Create;
  If (aJSON=Nil) then
    exit;
  Result.restrictedForReaders:=aJSON.Get('restrictedForReaders',False);
  Result.restrictedForWriters:=aJSON.Get('restrictedForWriters',False);
end;

class function TDownloadRestrictionSerializer.Deserialize(aJSON : String) : TDownloadRestriction;

var
  lObj : TJSONObject;
begin
  Result := Default(TDownloadRestriction);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TDownloadRestrictionsMetadataSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    if Assigned(itemDownloadRestriction) then
      Result.Add('itemDownloadRestriction',itemDownloadRestriction.SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TDownloadRestrictionsMetadataSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TDownloadRestrictionsMetadataSerializer.Deserialize(aJSON : TJSONObject) : TDownloadRestrictionsMetadata;

begin
  Result := TDownloadRestrictionsMetadata.Create;
  If (aJSON=Nil) then
    exit;
  Result.effectiveDownloadRestrictionWithContext:=TDownloadRestriction.Deserialize(aJSON.Get('effectiveDownloadRestrictionWithContext',TJSONObject(Nil)));
  Result.itemDownloadRestriction:=TDownloadRestriction.Deserialize(aJSON.Get('itemDownloadRestriction',TJSONObject(Nil)));
end;

class function TDownloadRestrictionsMetadataSerializer.Deserialize(aJSON : String) : TDownloadRestrictionsMetadata;

var
  lObj : TJSONObject;
begin
  Result := Default(TDownloadRestrictionsMetadata);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TPermissionSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('allowFileDiscovery',allowFileDiscovery);
    if (expirationTime<>0) then
      Result.Add('expirationTime',DateToISO8601(expirationTime,True));
    Result.Add('inheritedPermissionsDisabled',inheritedPermissionsDisabled);
    Result.Add('pendingOwner',pendingOwner);
    Result.Add('role',role);
    Result.Add('type',type_);
    Result.Add('view',view);
  except
    Result.Free;
    raise;
  end;
end;

function TPermissionSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TPermissionSerializer.Deserialize(aJSON : TJSONObject) : TPermission;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TPermission.Create;
  If (aJSON=Nil) then
    exit;
  Result.allowFileDiscovery:=aJSON.Get('allowFileDiscovery',False);
  Result.deleted:=aJSON.Get('deleted',False);
  Result.displayName:=aJSON.Get('displayName','');
  Result.domain:=aJSON.Get('domain','');
  Result.emailAddress:=aJSON.Get('emailAddress','');
  Result.expirationTime:=ISO8601ToDateDef(aJSON.Get('expirationTime',''),0,True);
  Result.id:=aJSON.Get('id','');
  Result.inheritedPermissionsDisabled:=aJSON.Get('inheritedPermissionsDisabled',False);
  Result.kind:=aJSON.Get('kind','');
  Result.pendingOwner:=aJSON.Get('pendingOwner',False);
  lArr:=aJSON.Get('permissionDetails',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.permissionDetails,lArr.Count);
    For I:=0 to Length(Result.permissionDetails)-1 do
      Result.permissionDetails[i]:=lArr[i].AsJSON;
    end;
  Result.photoLink:=aJSON.Get('photoLink','');
  Result.role:=aJSON.Get('role','');
  lArr:=aJSON.Get('teamDrivePermissionDetails',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.teamDrivePermissionDetails,lArr.Count);
    For I:=0 to Length(Result.teamDrivePermissionDetails)-1 do
      Result.teamDrivePermissionDetails[i]:=lArr[i].AsJSON;
    end;
  Result.type_:=aJSON.Get('type','');
  Result.view:=aJSON.Get('view','');
end;

class function TPermissionSerializer.Deserialize(aJSON : String) : TPermission;

var
  lObj : TJSONObject;
begin
  Result := Default(TPermission);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TFileSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    if (appProperties<>'') then
      Result.Add('appProperties',GetJSON(appProperties));
    if Assigned(clientEncryptionDetails) then
      Result.Add('clientEncryptionDetails',clientEncryptionDetails.SerializeObject);
    if (contentHints<>'') then
      Result.Add('contentHints',GetJSON(contentHints));
    Arr:=TJSONArray.Create;
    Result.Add('contentRestrictions',Arr);
    For I:=0 to Length(contentRestrictions)-1 do
      Arr.Add(contentRestrictions[i].SerializeObject);
    Result.Add('copyRequiresWriterPermission',copyRequiresWriterPermission);
    if (createdTime<>0) then
      Result.Add('createdTime',DateToISO8601(createdTime,True));
    Result.Add('description',description);
    if Assigned(downloadRestrictions) then
      Result.Add('downloadRestrictions',downloadRestrictions.SerializeObject);
    Result.Add('folderColorRgb',folderColorRgb);
    Result.Add('id',id);
    Result.Add('inheritedPermissionsDisabled',inheritedPermissionsDisabled);
    if (labelInfo<>'') then
      Result.Add('labelInfo',GetJSON(labelInfo));
    if (linkShareMetadata<>'') then
      Result.Add('linkShareMetadata',GetJSON(linkShareMetadata));
    Result.Add('mimeType',mimeType);
    if (modifiedByMeTime<>0) then
      Result.Add('modifiedByMeTime',DateToISO8601(modifiedByMeTime,True));
    if (modifiedTime<>0) then
      Result.Add('modifiedTime',DateToISO8601(modifiedTime,True));
    Result.Add('name',name);
    Result.Add('originalFilename',originalFilename);
    Arr:=TJSONArray.Create;
    Result.Add('parents',Arr);
    For I:=0 to Length(parents)-1 do
      Arr.Add(parents[i]);
    if (properties<>'') then
      Result.Add('properties',GetJSON(properties));
    if (sharedWithMeTime<>0) then
      Result.Add('sharedWithMeTime',DateToISO8601(sharedWithMeTime,True));
    if (shortcutDetails<>'') then
      Result.Add('shortcutDetails',GetJSON(shortcutDetails));
    Result.Add('starred',starred);
    Result.Add('trashed',trashed);
    if (trashedTime<>0) then
      Result.Add('trashedTime',DateToISO8601(trashedTime,True));
    if (viewedByMeTime<>0) then
      Result.Add('viewedByMeTime',DateToISO8601(viewedByMeTime,True));
    Result.Add('viewersCanCopyContent',viewersCanCopyContent);
    Result.Add('writersCanShare',writersCanShare);
  except
    Result.Free;
    raise;
  end;
end;

function TFileSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TFileSerializer.Deserialize(aJSON : TJSONObject) : TFile;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TFile.Create;
  If (aJSON=Nil) then
    exit;
  Result.appProperties:=JSONDataAsString(aJSON.Get('appProperties',TJSONObject(Nil)));
  Result.capabilities:=JSONDataAsString(aJSON.Get('capabilities',TJSONObject(Nil)));
  Result.clientEncryptionDetails:=TClientEncryptionDetails.Deserialize(aJSON.Get('clientEncryptionDetails',TJSONObject(Nil)));
  Result.contentHints:=JSONDataAsString(aJSON.Get('contentHints',TJSONObject(Nil)));
  lArr:=aJSON.Get('contentRestrictions',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.contentRestrictions,lArr.Count);
    For I:=0 to Length(Result.contentRestrictions)-1 do
      Result.contentRestrictions[i]:=TContentRestriction.Deserialize(lArr[i] as TJSONObject);
    end;
  Result.copyRequiresWriterPermission:=aJSON.Get('copyRequiresWriterPermission',False);
  Result.createdTime:=ISO8601ToDateDef(aJSON.Get('createdTime',''),0,True);
  Result.description:=aJSON.Get('description','');
  Result.downloadRestrictions:=TDownloadRestrictionsMetadata.Deserialize(aJSON.Get('downloadRestrictions',TJSONObject(Nil)));
  Result.driveId:=aJSON.Get('driveId','');
  Result.explicitlyTrashed:=aJSON.Get('explicitlyTrashed',False);
  Result.exportLinks:=JSONDataAsString(aJSON.Get('exportLinks',TJSONObject(Nil)));
  Result.fileExtension:=aJSON.Get('fileExtension','');
  Result.folderColorRgb:=aJSON.Get('folderColorRgb','');
  Result.fullFileExtension:=aJSON.Get('fullFileExtension','');
  Result.hasAugmentedPermissions:=aJSON.Get('hasAugmentedPermissions',False);
  Result.hasThumbnail:=aJSON.Get('hasThumbnail',False);
  Result.headRevisionId:=aJSON.Get('headRevisionId','');
  Result.iconLink:=aJSON.Get('iconLink','');
  Result.id:=aJSON.Get('id','');
  Result.imageMediaMetadata:=JSONDataAsString(aJSON.Get('imageMediaMetadata',TJSONObject(Nil)));
  Result.inheritedPermissionsDisabled:=aJSON.Get('inheritedPermissionsDisabled',False);
  Result.isAppAuthorized:=aJSON.Get('isAppAuthorized',False);
  Result.kind:=aJSON.Get('kind','');
  Result.labelInfo:=JSONDataAsString(aJSON.Get('labelInfo',TJSONObject(Nil)));
  Result.lastModifyingUser:=TUser.Deserialize(aJSON.Get('lastModifyingUser',TJSONObject(Nil)));
  Result.linkShareMetadata:=JSONDataAsString(aJSON.Get('linkShareMetadata',TJSONObject(Nil)));
  Result.md5Checksum:=aJSON.Get('md5Checksum','');
  Result.mimeType:=aJSON.Get('mimeType','');
  Result.modifiedByMe:=aJSON.Get('modifiedByMe',False);
  Result.modifiedByMeTime:=ISO8601ToDateDef(aJSON.Get('modifiedByMeTime',''),0,True);
  Result.modifiedTime:=ISO8601ToDateDef(aJSON.Get('modifiedTime',''),0,True);
  Result.name:=aJSON.Get('name','');
  Result.originalFilename:=aJSON.Get('originalFilename','');
  Result.ownedByMe:=aJSON.Get('ownedByMe',False);
  lArr:=aJSON.Get('owners',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.owners,lArr.Count);
    For I:=0 to Length(Result.owners)-1 do
      Result.owners[i]:=TUser.Deserialize(lArr[i] as TJSONObject);
    end;
  lArr:=aJSON.Get('parents',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.parents,lArr.Count);
    For I:=0 to Length(Result.parents)-1 do
      Result.parents[i]:=lArr[i].Asstring;
    end;
  lArr:=aJSON.Get('permissionIds',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.permissionIds,lArr.Count);
    For I:=0 to Length(Result.permissionIds)-1 do
      Result.permissionIds[i]:=lArr[i].Asstring;
    end;
  lArr:=aJSON.Get('permissions',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.permissions,lArr.Count);
    For I:=0 to Length(Result.permissions)-1 do
      Result.permissions[i]:=TPermission.Deserialize(lArr[i] as TJSONObject);
    end;
  Result.properties:=JSONDataAsString(aJSON.Get('properties',TJSONObject(Nil)));
  Result.quotaBytesUsed:=aJSON.Get('quotaBytesUsed','');
  Result.resourceKey:=aJSON.Get('resourceKey','');
  Result.sha1Checksum:=aJSON.Get('sha1Checksum','');
  Result.sha256Checksum:=aJSON.Get('sha256Checksum','');
  Result.shared:=aJSON.Get('shared',False);
  Result.sharedWithMeTime:=ISO8601ToDateDef(aJSON.Get('sharedWithMeTime',''),0,True);
  Result.sharingUser:=TUser.Deserialize(aJSON.Get('sharingUser',TJSONObject(Nil)));
  Result.shortcutDetails:=JSONDataAsString(aJSON.Get('shortcutDetails',TJSONObject(Nil)));
  Result.size:=aJSON.Get('size','');
  lArr:=aJSON.Get('spaces',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.spaces,lArr.Count);
    For I:=0 to Length(Result.spaces)-1 do
      Result.spaces[i]:=lArr[i].Asstring;
    end;
  Result.starred:=aJSON.Get('starred',False);
  Result.teamDriveId:=aJSON.Get('teamDriveId','');
  Result.thumbnailLink:=aJSON.Get('thumbnailLink','');
  Result.thumbnailVersion:=aJSON.Get('thumbnailVersion','');
  Result.trashed:=aJSON.Get('trashed',False);
  Result.trashedTime:=ISO8601ToDateDef(aJSON.Get('trashedTime',''),0,True);
  Result.trashingUser:=TUser.Deserialize(aJSON.Get('trashingUser',TJSONObject(Nil)));
  Result.version:=aJSON.Get('version','');
  Result.videoMediaMetadata:=JSONDataAsString(aJSON.Get('videoMediaMetadata',TJSONObject(Nil)));
  Result.viewedByMe:=aJSON.Get('viewedByMe',False);
  Result.viewedByMeTime:=ISO8601ToDateDef(aJSON.Get('viewedByMeTime',''),0,True);
  Result.viewersCanCopyContent:=aJSON.Get('viewersCanCopyContent',False);
  Result.webContentLink:=aJSON.Get('webContentLink','');
  Result.webViewLink:=aJSON.Get('webViewLink','');
  Result.writersCanShare:=aJSON.Get('writersCanShare',False);
end;

class function TFileSerializer.Deserialize(aJSON : String) : TFile;

var
  lObj : TJSONObject;
begin
  Result := Default(TFile);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TTeamDriveSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    if (backgroundImageFile<>'') then
      Result.Add('backgroundImageFile',GetJSON(backgroundImageFile));
    Result.Add('backgroundImageLink',backgroundImageLink);
    if (capabilities<>'') then
      Result.Add('capabilities',GetJSON(capabilities));
    Result.Add('colorRgb',colorRgb);
    if (createdTime<>0) then
      Result.Add('createdTime',DateToISO8601(createdTime,True));
    Result.Add('id',id);
    Result.Add('kind',kind);
    Result.Add('name',name);
    Result.Add('orgUnitId',orgUnitId);
    if (restrictions<>'') then
      Result.Add('restrictions',GetJSON(restrictions));
    Result.Add('themeId',themeId);
  except
    Result.Free;
    raise;
  end;
end;

function TTeamDriveSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TTeamDriveSerializer.Deserialize(aJSON : TJSONObject) : TTeamDrive;

begin
  Result := TTeamDrive.Create;
  If (aJSON=Nil) then
    exit;
  Result.backgroundImageFile:=JSONDataAsString(aJSON.Get('backgroundImageFile',TJSONObject(Nil)));
  Result.backgroundImageLink:=aJSON.Get('backgroundImageLink','');
  Result.capabilities:=JSONDataAsString(aJSON.Get('capabilities',TJSONObject(Nil)));
  Result.colorRgb:=aJSON.Get('colorRgb','');
  Result.createdTime:=ISO8601ToDateDef(aJSON.Get('createdTime',''),0,True);
  Result.id:=aJSON.Get('id','');
  Result.kind:=aJSON.Get('kind','');
  Result.name:=aJSON.Get('name','');
  Result.orgUnitId:=aJSON.Get('orgUnitId','');
  Result.restrictions:=JSONDataAsString(aJSON.Get('restrictions',TJSONObject(Nil)));
  Result.themeId:=aJSON.Get('themeId','');
end;

class function TTeamDriveSerializer.Deserialize(aJSON : String) : TTeamDrive;

var
  lObj : TJSONObject;
begin
  Result := Default(TTeamDrive);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TChangeSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('changeType',changeType);
    if Assigned(drive) then
      Result.Add('drive',drive.SerializeObject);
    Result.Add('driveId',driveId);
    Result.Add('fileId',fileId);
    if Assigned(file_) then
      Result.Add('file',file_.SerializeObject);
    Result.Add('kind',kind);
    Result.Add('removed',removed);
    if Assigned(teamDrive) then
      Result.Add('teamDrive',teamDrive.SerializeObject);
    Result.Add('teamDriveId',teamDriveId);
    if (time<>0) then
      Result.Add('time',DateToISO8601(time,True));
    Result.Add('type',type_);
  except
    Result.Free;
    raise;
  end;
end;

function TChangeSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TChangeSerializer.Deserialize(aJSON : TJSONObject) : TChange;

begin
  Result := TChange.Create;
  If (aJSON=Nil) then
    exit;
  Result.changeType:=aJSON.Get('changeType','');
  Result.drive:=TDrive.Deserialize(aJSON.Get('drive',TJSONObject(Nil)));
  Result.driveId:=aJSON.Get('driveId','');
  Result.fileId:=aJSON.Get('fileId','');
  Result.file_:=TFile.Deserialize(aJSON.Get('file',TJSONObject(Nil)));
  Result.kind:=aJSON.Get('kind','');
  Result.removed:=aJSON.Get('removed',False);
  Result.teamDrive:=TTeamDrive.Deserialize(aJSON.Get('teamDrive',TJSONObject(Nil)));
  Result.teamDriveId:=aJSON.Get('teamDriveId','');
  Result.time:=ISO8601ToDateDef(aJSON.Get('time',''),0,True);
  Result.type_:=aJSON.Get('type','');
end;

class function TChangeSerializer.Deserialize(aJSON : String) : TChange;

var
  lObj : TJSONObject;
begin
  Result := Default(TChange);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TChangeListSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('changes',Arr);
    For I:=0 to Length(changes)-1 do
      Arr.Add(changes[i].SerializeObject);
    Result.Add('kind',kind);
    Result.Add('newStartPageToken',newStartPageToken);
    Result.Add('nextPageToken',nextPageToken);
  except
    Result.Free;
    raise;
  end;
end;

function TChangeListSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TChangeListSerializer.Deserialize(aJSON : TJSONObject) : TChangeList;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TChangeList.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('changes',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.changes,lArr.Count);
    For I:=0 to Length(Result.changes)-1 do
      Result.changes[i]:=TChange.Deserialize(lArr[i] as TJSONObject);
    end;
  Result.kind:=aJSON.Get('kind','');
  Result.newStartPageToken:=aJSON.Get('newStartPageToken','');
  Result.nextPageToken:=aJSON.Get('nextPageToken','');
end;

class function TChangeListSerializer.Deserialize(aJSON : String) : TChangeList;

var
  lObj : TJSONObject;
begin
  Result := Default(TChangeList);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TChannelSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('address',address);
    Result.Add('expiration',expiration);
    Result.Add('id',id);
    Result.Add('kind',kind);
    if (params<>'') then
      Result.Add('params',GetJSON(params));
    Result.Add('payload',payload);
    Result.Add('resourceId',resourceId);
    Result.Add('resourceUri',resourceUri);
    Result.Add('token',token);
    Result.Add('type',type_);
  except
    Result.Free;
    raise;
  end;
end;

function TChannelSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TChannelSerializer.Deserialize(aJSON : TJSONObject) : TChannel;

begin
  Result := TChannel.Create;
  If (aJSON=Nil) then
    exit;
  Result.address:=aJSON.Get('address','');
  Result.expiration:=aJSON.Get('expiration','');
  Result.id:=aJSON.Get('id','');
  Result.kind:=aJSON.Get('kind','');
  Result.params:=JSONDataAsString(aJSON.Get('params',TJSONObject(Nil)));
  Result.payload:=aJSON.Get('payload',False);
  Result.resourceId:=aJSON.Get('resourceId','');
  Result.resourceUri:=aJSON.Get('resourceUri','');
  Result.token:=aJSON.Get('token','');
  Result.type_:=aJSON.Get('type','');
end;

class function TChannelSerializer.Deserialize(aJSON : String) : TChannel;

var
  lObj : TJSONObject;
begin
  Result := Default(TChannel);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TReplySerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('action',action);
    Result.Add('content',content);
    if (createdTime<>0) then
      Result.Add('createdTime',DateToISO8601(createdTime,True));
    if (modifiedTime<>0) then
      Result.Add('modifiedTime',DateToISO8601(modifiedTime,True));
  except
    Result.Free;
    raise;
  end;
end;

function TReplySerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TReplySerializer.Deserialize(aJSON : TJSONObject) : TReply;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TReply.Create;
  If (aJSON=Nil) then
    exit;
  Result.action:=aJSON.Get('action','');
  Result.assigneeEmailAddress:=aJSON.Get('assigneeEmailAddress','');
  Result.author:=TUser.Deserialize(aJSON.Get('author',TJSONObject(Nil)));
  Result.content:=aJSON.Get('content','');
  Result.createdTime:=ISO8601ToDateDef(aJSON.Get('createdTime',''),0,True);
  Result.deleted:=aJSON.Get('deleted',False);
  Result.htmlContent:=aJSON.Get('htmlContent','');
  Result.id:=aJSON.Get('id','');
  Result.kind:=aJSON.Get('kind','');
  lArr:=aJSON.Get('mentionedEmailAddresses',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.mentionedEmailAddresses,lArr.Count);
    For I:=0 to Length(Result.mentionedEmailAddresses)-1 do
      Result.mentionedEmailAddresses[i]:=lArr[i].Asstring;
    end;
  Result.modifiedTime:=ISO8601ToDateDef(aJSON.Get('modifiedTime',''),0,True);
end;

class function TReplySerializer.Deserialize(aJSON : String) : TReply;

var
  lObj : TJSONObject;
begin
  Result := Default(TReply);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TCommentSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('anchor',anchor);
    Result.Add('content',content);
    if (createdTime<>0) then
      Result.Add('createdTime',DateToISO8601(createdTime,True));
    if (modifiedTime<>0) then
      Result.Add('modifiedTime',DateToISO8601(modifiedTime,True));
    if (quotedFileContent<>'') then
      Result.Add('quotedFileContent',GetJSON(quotedFileContent));
  except
    Result.Free;
    raise;
  end;
end;

function TCommentSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TCommentSerializer.Deserialize(aJSON : TJSONObject) : TComment;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TComment.Create;
  If (aJSON=Nil) then
    exit;
  Result.anchor:=aJSON.Get('anchor','');
  Result.assigneeEmailAddress:=aJSON.Get('assigneeEmailAddress','');
  Result.author:=TUser.Deserialize(aJSON.Get('author',TJSONObject(Nil)));
  Result.content:=aJSON.Get('content','');
  Result.createdTime:=ISO8601ToDateDef(aJSON.Get('createdTime',''),0,True);
  Result.deleted:=aJSON.Get('deleted',False);
  Result.htmlContent:=aJSON.Get('htmlContent','');
  Result.id:=aJSON.Get('id','');
  Result.kind:=aJSON.Get('kind','');
  lArr:=aJSON.Get('mentionedEmailAddresses',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.mentionedEmailAddresses,lArr.Count);
    For I:=0 to Length(Result.mentionedEmailAddresses)-1 do
      Result.mentionedEmailAddresses[i]:=lArr[i].Asstring;
    end;
  Result.modifiedTime:=ISO8601ToDateDef(aJSON.Get('modifiedTime',''),0,True);
  Result.quotedFileContent:=JSONDataAsString(aJSON.Get('quotedFileContent',TJSONObject(Nil)));
  lArr:=aJSON.Get('replies',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.replies,lArr.Count);
    For I:=0 to Length(Result.replies)-1 do
      Result.replies[i]:=TReply.Deserialize(lArr[i] as TJSONObject);
    end;
  Result.resolved:=aJSON.Get('resolved',False);
end;

class function TCommentSerializer.Deserialize(aJSON : String) : TComment;

var
  lObj : TJSONObject;
begin
  Result := Default(TComment);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TCommentApprovalRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('message',message);
  except
    Result.Free;
    raise;
  end;
end;

function TCommentApprovalRequestSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TCommentApprovalRequestSerializer.Deserialize(aJSON : TJSONObject) : TCommentApprovalRequest;

begin
  Result := TCommentApprovalRequest.Create;
  If (aJSON=Nil) then
    exit;
  Result.message:=aJSON.Get('message','');
end;

class function TCommentApprovalRequestSerializer.Deserialize(aJSON : String) : TCommentApprovalRequest;

var
  lObj : TJSONObject;
begin
  Result := Default(TCommentApprovalRequest);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TCommentListSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('comments',Arr);
    For I:=0 to Length(comments)-1 do
      Arr.Add(comments[i].SerializeObject);
    Result.Add('kind',kind);
    Result.Add('nextPageToken',nextPageToken);
  except
    Result.Free;
    raise;
  end;
end;

function TCommentListSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TCommentListSerializer.Deserialize(aJSON : TJSONObject) : TCommentList;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TCommentList.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('comments',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.comments,lArr.Count);
    For I:=0 to Length(Result.comments)-1 do
      Result.comments[i]:=TComment.Deserialize(lArr[i] as TJSONObject);
    end;
  Result.kind:=aJSON.Get('kind','');
  Result.nextPageToken:=aJSON.Get('nextPageToken','');
end;

class function TCommentListSerializer.Deserialize(aJSON : String) : TCommentList;

var
  lObj : TJSONObject;
begin
  Result := Default(TCommentList);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TDeclineApprovalRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('message',message);
  except
    Result.Free;
    raise;
  end;
end;

function TDeclineApprovalRequestSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TDeclineApprovalRequestSerializer.Deserialize(aJSON : TJSONObject) : TDeclineApprovalRequest;

begin
  Result := TDeclineApprovalRequest.Create;
  If (aJSON=Nil) then
    exit;
  Result.message:=aJSON.Get('message','');
end;

class function TDeclineApprovalRequestSerializer.Deserialize(aJSON : String) : TDeclineApprovalRequest;

var
  lObj : TJSONObject;
begin
  Result := Default(TDeclineApprovalRequest);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TDriveListSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('drives',Arr);
    For I:=0 to Length(drives)-1 do
      Arr.Add(drives[i].SerializeObject);
    Result.Add('kind',kind);
    Result.Add('nextPageToken',nextPageToken);
  except
    Result.Free;
    raise;
  end;
end;

function TDriveListSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TDriveListSerializer.Deserialize(aJSON : TJSONObject) : TDriveList;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TDriveList.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('drives',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.drives,lArr.Count);
    For I:=0 to Length(Result.drives)-1 do
      Result.drives[i]:=TDrive.Deserialize(lArr[i] as TJSONObject);
    end;
  Result.kind:=aJSON.Get('kind','');
  Result.nextPageToken:=aJSON.Get('nextPageToken','');
end;

class function TDriveListSerializer.Deserialize(aJSON : String) : TDriveList;

var
  lObj : TJSONObject;
begin
  Result := Default(TDriveList);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TFileListSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('files',Arr);
    For I:=0 to Length(files)-1 do
      Arr.Add(files[i].SerializeObject);
    Result.Add('incompleteSearch',incompleteSearch);
    Result.Add('kind',kind);
    Result.Add('nextPageToken',nextPageToken);
  except
    Result.Free;
    raise;
  end;
end;

function TFileListSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TFileListSerializer.Deserialize(aJSON : TJSONObject) : TFileList;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TFileList.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('files',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.files,lArr.Count);
    For I:=0 to Length(Result.files)-1 do
      Result.files[i]:=TFile.Deserialize(lArr[i] as TJSONObject);
    end;
  Result.incompleteSearch:=aJSON.Get('incompleteSearch',False);
  Result.kind:=aJSON.Get('kind','');
  Result.nextPageToken:=aJSON.Get('nextPageToken','');
end;

class function TFileListSerializer.Deserialize(aJSON : String) : TFileList;

var
  lObj : TJSONObject;
begin
  Result := Default(TFileList);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TGenerateCseTokenResponseSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('currentKaclsId',currentKaclsId);
    Result.Add('currentKaclsName',currentKaclsName);
    Result.Add('fileId',fileId);
    Result.Add('jwt',jwt);
  except
    Result.Free;
    raise;
  end;
end;

function TGenerateCseTokenResponseSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TGenerateCseTokenResponseSerializer.Deserialize(aJSON : TJSONObject) : TGenerateCseTokenResponse;

begin
  Result := TGenerateCseTokenResponse.Create;
  If (aJSON=Nil) then
    exit;
  Result.currentKaclsId:=aJSON.Get('currentKaclsId','');
  Result.currentKaclsName:=aJSON.Get('currentKaclsName','');
  Result.fileId:=aJSON.Get('fileId','');
  Result.jwt:=aJSON.Get('jwt','');
  Result.kind:=aJSON.Get('kind','');
end;

class function TGenerateCseTokenResponseSerializer.Deserialize(aJSON : String) : TGenerateCseTokenResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TGenerateCseTokenResponse);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TGeneratedIdsSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('ids',Arr);
    For I:=0 to Length(ids)-1 do
      Arr.Add(ids[i]);
    Result.Add('kind',kind);
    Result.Add('space',space);
  except
    Result.Free;
    raise;
  end;
end;

function TGeneratedIdsSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TGeneratedIdsSerializer.Deserialize(aJSON : TJSONObject) : TGeneratedIds;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TGeneratedIds.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('ids',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.ids,lArr.Count);
    For I:=0 to Length(Result.ids)-1 do
      Result.ids[i]:=lArr[i].Asstring;
    end;
  Result.kind:=aJSON.Get('kind','');
  Result.space:=aJSON.Get('space','');
end;

class function TGeneratedIdsSerializer.Deserialize(aJSON : String) : TGeneratedIds;

var
  lObj : TJSONObject;
begin
  Result := Default(TGeneratedIds);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TLabelSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    if (fields<>'') then
      Result.Add('fields',GetJSON(fields));
    Result.Add('id',id);
    Result.Add('kind',kind);
    Result.Add('revisionId',revisionId);
  except
    Result.Free;
    raise;
  end;
end;

function TLabelSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TLabelSerializer.Deserialize(aJSON : TJSONObject) : TLabel;

begin
  Result := TLabel.Create;
  If (aJSON=Nil) then
    exit;
  Result.fields:=JSONDataAsString(aJSON.Get('fields',TJSONObject(Nil)));
  Result.id:=aJSON.Get('id','');
  Result.kind:=aJSON.Get('kind','');
  Result.revisionId:=aJSON.Get('revisionId','');
end;

class function TLabelSerializer.Deserialize(aJSON : String) : TLabel;

var
  lObj : TJSONObject;
begin
  Result := Default(TLabel);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TLabelFieldSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('dateString',Arr);
    For I:=0 to Length(dateString)-1 do
      Arr.Add(dateString[i]);
    Result.Add('id',id);
    Arr:=TJSONArray.Create;
    Result.Add('integer',Arr);
    For I:=0 to Length(integer)-1 do
      Arr.Add(integer[i]);
    Result.Add('kind',kind);
    Arr:=TJSONArray.Create;
    Result.Add('selection',Arr);
    For I:=0 to Length(selection)-1 do
      Arr.Add(selection[i]);
    Arr:=TJSONArray.Create;
    Result.Add('text',Arr);
    For I:=0 to Length(text)-1 do
      Arr.Add(text[i]);
    Arr:=TJSONArray.Create;
    Result.Add('user',Arr);
    For I:=0 to Length(user)-1 do
      Arr.Add(user[i].SerializeObject);
    Result.Add('valueType',valueType);
  except
    Result.Free;
    raise;
  end;
end;

function TLabelFieldSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TLabelFieldSerializer.Deserialize(aJSON : TJSONObject) : TLabelField;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TLabelField.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('dateString',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.dateString,lArr.Count);
    For I:=0 to Length(Result.dateString)-1 do
      Result.dateString[i]:=lArr[i].Asstring;
    end;
  Result.id:=aJSON.Get('id','');
  lArr:=aJSON.Get('integer',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.integer,lArr.Count);
    For I:=0 to Length(Result.integer)-1 do
      Result.integer[i]:=lArr[i].Asstring;
    end;
  Result.kind:=aJSON.Get('kind','');
  lArr:=aJSON.Get('selection',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.selection,lArr.Count);
    For I:=0 to Length(Result.selection)-1 do
      Result.selection[i]:=lArr[i].Asstring;
    end;
  lArr:=aJSON.Get('text',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.text,lArr.Count);
    For I:=0 to Length(Result.text)-1 do
      Result.text[i]:=lArr[i].Asstring;
    end;
  lArr:=aJSON.Get('user',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.user,lArr.Count);
    For I:=0 to Length(Result.user)-1 do
      Result.user[i]:=TUser.Deserialize(lArr[i] as TJSONObject);
    end;
  Result.valueType:=aJSON.Get('valueType','');
end;

class function TLabelFieldSerializer.Deserialize(aJSON : String) : TLabelField;

var
  lObj : TJSONObject;
begin
  Result := Default(TLabelField);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TLabelFieldModificationSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('fieldId',fieldId);
    Result.Add('kind',kind);
    Arr:=TJSONArray.Create;
    Result.Add('setDateValues',Arr);
    For I:=0 to Length(setDateValues)-1 do
      Arr.Add(setDateValues[i]);
    Arr:=TJSONArray.Create;
    Result.Add('setIntegerValues',Arr);
    For I:=0 to Length(setIntegerValues)-1 do
      Arr.Add(setIntegerValues[i]);
    Arr:=TJSONArray.Create;
    Result.Add('setSelectionValues',Arr);
    For I:=0 to Length(setSelectionValues)-1 do
      Arr.Add(setSelectionValues[i]);
    Arr:=TJSONArray.Create;
    Result.Add('setTextValues',Arr);
    For I:=0 to Length(setTextValues)-1 do
      Arr.Add(setTextValues[i]);
    Arr:=TJSONArray.Create;
    Result.Add('setUserValues',Arr);
    For I:=0 to Length(setUserValues)-1 do
      Arr.Add(setUserValues[i]);
    Result.Add('unsetValues',unsetValues);
  except
    Result.Free;
    raise;
  end;
end;

function TLabelFieldModificationSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TLabelFieldModificationSerializer.Deserialize(aJSON : TJSONObject) : TLabelFieldModification;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TLabelFieldModification.Create;
  If (aJSON=Nil) then
    exit;
  Result.fieldId:=aJSON.Get('fieldId','');
  Result.kind:=aJSON.Get('kind','');
  lArr:=aJSON.Get('setDateValues',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.setDateValues,lArr.Count);
    For I:=0 to Length(Result.setDateValues)-1 do
      Result.setDateValues[i]:=lArr[i].Asstring;
    end;
  lArr:=aJSON.Get('setIntegerValues',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.setIntegerValues,lArr.Count);
    For I:=0 to Length(Result.setIntegerValues)-1 do
      Result.setIntegerValues[i]:=lArr[i].Asstring;
    end;
  lArr:=aJSON.Get('setSelectionValues',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.setSelectionValues,lArr.Count);
    For I:=0 to Length(Result.setSelectionValues)-1 do
      Result.setSelectionValues[i]:=lArr[i].Asstring;
    end;
  lArr:=aJSON.Get('setTextValues',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.setTextValues,lArr.Count);
    For I:=0 to Length(Result.setTextValues)-1 do
      Result.setTextValues[i]:=lArr[i].Asstring;
    end;
  lArr:=aJSON.Get('setUserValues',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.setUserValues,lArr.Count);
    For I:=0 to Length(Result.setUserValues)-1 do
      Result.setUserValues[i]:=lArr[i].Asstring;
    end;
  Result.unsetValues:=aJSON.Get('unsetValues',False);
end;

class function TLabelFieldModificationSerializer.Deserialize(aJSON : String) : TLabelFieldModification;

var
  lObj : TJSONObject;
begin
  Result := Default(TLabelFieldModification);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TLabelListSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('kind',kind);
    Arr:=TJSONArray.Create;
    Result.Add('labels',Arr);
    For I:=0 to Length(labels)-1 do
      Arr.Add(labels[i].SerializeObject);
    Result.Add('nextPageToken',nextPageToken);
  except
    Result.Free;
    raise;
  end;
end;

function TLabelListSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TLabelListSerializer.Deserialize(aJSON : TJSONObject) : TLabelList;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TLabelList.Create;
  If (aJSON=Nil) then
    exit;
  Result.kind:=aJSON.Get('kind','');
  lArr:=aJSON.Get('labels',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.labels,lArr.Count);
    For I:=0 to Length(Result.labels)-1 do
      Result.labels[i]:=TLabel.Deserialize(lArr[i] as TJSONObject);
    end;
  Result.nextPageToken:=aJSON.Get('nextPageToken','');
end;

class function TLabelListSerializer.Deserialize(aJSON : String) : TLabelList;

var
  lObj : TJSONObject;
begin
  Result := Default(TLabelList);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TLabelModificationSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('fieldModifications',Arr);
    For I:=0 to Length(fieldModifications)-1 do
      Arr.Add(fieldModifications[i].SerializeObject);
    Result.Add('kind',kind);
    Result.Add('labelId',labelId);
    Result.Add('removeLabel',removeLabel);
  except
    Result.Free;
    raise;
  end;
end;

function TLabelModificationSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TLabelModificationSerializer.Deserialize(aJSON : TJSONObject) : TLabelModification;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TLabelModification.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('fieldModifications',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.fieldModifications,lArr.Count);
    For I:=0 to Length(Result.fieldModifications)-1 do
      Result.fieldModifications[i]:=TLabelFieldModification.Deserialize(lArr[i] as TJSONObject);
    end;
  Result.kind:=aJSON.Get('kind','');
  Result.labelId:=aJSON.Get('labelId','');
  Result.removeLabel:=aJSON.Get('removeLabel',False);
end;

class function TLabelModificationSerializer.Deserialize(aJSON : String) : TLabelModification;

var
  lObj : TJSONObject;
begin
  Result := Default(TLabelModification);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TListAccessProposalsResponseSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('accessProposals',Arr);
    For I:=0 to Length(accessProposals)-1 do
      Arr.Add(accessProposals[i].SerializeObject);
    Result.Add('nextPageToken',nextPageToken);
  except
    Result.Free;
    raise;
  end;
end;

function TListAccessProposalsResponseSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TListAccessProposalsResponseSerializer.Deserialize(aJSON : TJSONObject) : TListAccessProposalsResponse;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TListAccessProposalsResponse.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('accessProposals',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.accessProposals,lArr.Count);
    For I:=0 to Length(Result.accessProposals)-1 do
      Result.accessProposals[i]:=TAccessProposal.Deserialize(lArr[i] as TJSONObject);
    end;
  Result.nextPageToken:=aJSON.Get('nextPageToken','');
end;

class function TListAccessProposalsResponseSerializer.Deserialize(aJSON : String) : TListAccessProposalsResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TListAccessProposalsResponse);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TModifyLabelsRequestSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('kind',kind);
    Arr:=TJSONArray.Create;
    Result.Add('labelModifications',Arr);
    For I:=0 to Length(labelModifications)-1 do
      Arr.Add(labelModifications[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TModifyLabelsRequestSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TModifyLabelsRequestSerializer.Deserialize(aJSON : TJSONObject) : TModifyLabelsRequest;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TModifyLabelsRequest.Create;
  If (aJSON=Nil) then
    exit;
  Result.kind:=aJSON.Get('kind','');
  lArr:=aJSON.Get('labelModifications',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.labelModifications,lArr.Count);
    For I:=0 to Length(Result.labelModifications)-1 do
      Result.labelModifications[i]:=TLabelModification.Deserialize(lArr[i] as TJSONObject);
    end;
end;

class function TModifyLabelsRequestSerializer.Deserialize(aJSON : String) : TModifyLabelsRequest;

var
  lObj : TJSONObject;
begin
  Result := Default(TModifyLabelsRequest);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TModifyLabelsResponseSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('kind',kind);
    Arr:=TJSONArray.Create;
    Result.Add('modifiedLabels',Arr);
    For I:=0 to Length(modifiedLabels)-1 do
      Arr.Add(modifiedLabels[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TModifyLabelsResponseSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TModifyLabelsResponseSerializer.Deserialize(aJSON : TJSONObject) : TModifyLabelsResponse;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TModifyLabelsResponse.Create;
  If (aJSON=Nil) then
    exit;
  Result.kind:=aJSON.Get('kind','');
  lArr:=aJSON.Get('modifiedLabels',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.modifiedLabels,lArr.Count);
    For I:=0 to Length(Result.modifiedLabels)-1 do
      Result.modifiedLabels[i]:=TLabel.Deserialize(lArr[i] as TJSONObject);
    end;
end;

class function TModifyLabelsResponseSerializer.Deserialize(aJSON : String) : TModifyLabelsResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TModifyLabelsResponse);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TStatusSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('code',code);
    Arr:=TJSONArray.Create;
    Result.Add('details',Arr);
    For I:=0 to Length(details)-1 do
      Arr.Add(GetJSON(details[i]));
    Result.Add('message',message);
  except
    Result.Free;
    raise;
  end;
end;

function TStatusSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TStatusSerializer.Deserialize(aJSON : TJSONObject) : TStatus;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TStatus.Create;
  If (aJSON=Nil) then
    exit;
  Result.code:=aJSON.Get('code',0);
  lArr:=aJSON.Get('details',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.details,lArr.Count);
    For I:=0 to Length(Result.details)-1 do
      Result.details[i]:=lArr[i].AsJSON;
    end;
  Result.message:=aJSON.Get('message','');
end;

class function TStatusSerializer.Deserialize(aJSON : String) : TStatus;

var
  lObj : TJSONObject;
begin
  Result := Default(TStatus);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TOperationSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('done',done);
    if Assigned(error) then
      Result.Add('error',error.SerializeObject);
    if (metadata<>'') then
      Result.Add('metadata',GetJSON(metadata));
    Result.Add('name',name);
    if (response<>'') then
      Result.Add('response',GetJSON(response));
  except
    Result.Free;
    raise;
  end;
end;

function TOperationSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TOperationSerializer.Deserialize(aJSON : TJSONObject) : TOperation;

begin
  Result := TOperation.Create;
  If (aJSON=Nil) then
    exit;
  Result.done:=aJSON.Get('done',False);
  Result.error:=TStatus.Deserialize(aJSON.Get('error',TJSONObject(Nil)));
  Result.metadata:=JSONDataAsString(aJSON.Get('metadata',TJSONObject(Nil)));
  Result.name:=aJSON.Get('name','');
  Result.response:=JSONDataAsString(aJSON.Get('response',TJSONObject(Nil)));
end;

class function TOperationSerializer.Deserialize(aJSON : String) : TOperation;

var
  lObj : TJSONObject;
begin
  Result := Default(TOperation);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TPermissionListSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('kind',kind);
    Result.Add('nextPageToken',nextPageToken);
    Arr:=TJSONArray.Create;
    Result.Add('permissions',Arr);
    For I:=0 to Length(permissions)-1 do
      Arr.Add(permissions[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TPermissionListSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TPermissionListSerializer.Deserialize(aJSON : TJSONObject) : TPermissionList;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TPermissionList.Create;
  If (aJSON=Nil) then
    exit;
  Result.kind:=aJSON.Get('kind','');
  Result.nextPageToken:=aJSON.Get('nextPageToken','');
  lArr:=aJSON.Get('permissions',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.permissions,lArr.Count);
    For I:=0 to Length(Result.permissions)-1 do
      Result.permissions[i]:=TPermission.Deserialize(lArr[i] as TJSONObject);
    end;
end;

class function TPermissionListSerializer.Deserialize(aJSON : String) : TPermissionList;

var
  lObj : TJSONObject;
begin
  Result := Default(TPermissionList);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TReplaceReviewerSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('addedReviewerEmail',addedReviewerEmail);
    Result.Add('removedReviewerEmail',removedReviewerEmail);
  except
    Result.Free;
    raise;
  end;
end;

function TReplaceReviewerSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TReplaceReviewerSerializer.Deserialize(aJSON : TJSONObject) : TReplaceReviewer;

begin
  Result := TReplaceReviewer.Create;
  If (aJSON=Nil) then
    exit;
  Result.addedReviewerEmail:=aJSON.Get('addedReviewerEmail','');
  Result.removedReviewerEmail:=aJSON.Get('removedReviewerEmail','');
end;

class function TReplaceReviewerSerializer.Deserialize(aJSON : String) : TReplaceReviewer;

var
  lObj : TJSONObject;
begin
  Result := Default(TReplaceReviewer);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TReassignApprovalRequestSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('addReviewers',Arr);
    For I:=0 to Length(addReviewers)-1 do
      Arr.Add(addReviewers[i].SerializeObject);
    Result.Add('message',message);
    Arr:=TJSONArray.Create;
    Result.Add('replaceReviewers',Arr);
    For I:=0 to Length(replaceReviewers)-1 do
      Arr.Add(replaceReviewers[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TReassignApprovalRequestSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TReassignApprovalRequestSerializer.Deserialize(aJSON : TJSONObject) : TReassignApprovalRequest;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TReassignApprovalRequest.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('addReviewers',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.addReviewers,lArr.Count);
    For I:=0 to Length(Result.addReviewers)-1 do
      Result.addReviewers[i]:=TAddReviewer.Deserialize(lArr[i] as TJSONObject);
    end;
  Result.message:=aJSON.Get('message','');
  lArr:=aJSON.Get('replaceReviewers',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.replaceReviewers,lArr.Count);
    For I:=0 to Length(Result.replaceReviewers)-1 do
      Result.replaceReviewers[i]:=TReplaceReviewer.Deserialize(lArr[i] as TJSONObject);
    end;
end;

class function TReassignApprovalRequestSerializer.Deserialize(aJSON : String) : TReassignApprovalRequest;

var
  lObj : TJSONObject;
begin
  Result := Default(TReassignApprovalRequest);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TReplyListSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('kind',kind);
    Result.Add('nextPageToken',nextPageToken);
    Arr:=TJSONArray.Create;
    Result.Add('replies',Arr);
    For I:=0 to Length(replies)-1 do
      Arr.Add(replies[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TReplyListSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TReplyListSerializer.Deserialize(aJSON : TJSONObject) : TReplyList;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TReplyList.Create;
  If (aJSON=Nil) then
    exit;
  Result.kind:=aJSON.Get('kind','');
  Result.nextPageToken:=aJSON.Get('nextPageToken','');
  lArr:=aJSON.Get('replies',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.replies,lArr.Count);
    For I:=0 to Length(Result.replies)-1 do
      Result.replies[i]:=TReply.Deserialize(lArr[i] as TJSONObject);
    end;
end;

class function TReplyListSerializer.Deserialize(aJSON : String) : TReplyList;

var
  lObj : TJSONObject;
begin
  Result := Default(TReplyList);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TResolveAccessProposalRequestSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('action',action);
    Arr:=TJSONArray.Create;
    Result.Add('role',Arr);
    For I:=0 to Length(role)-1 do
      Arr.Add(role[i]);
    Result.Add('sendNotification',sendNotification);
    Result.Add('view',view);
  except
    Result.Free;
    raise;
  end;
end;

function TResolveAccessProposalRequestSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TResolveAccessProposalRequestSerializer.Deserialize(aJSON : TJSONObject) : TResolveAccessProposalRequest;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TResolveAccessProposalRequest.Create;
  If (aJSON=Nil) then
    exit;
  Result.action:=aJSON.Get('action','');
  lArr:=aJSON.Get('role',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.role,lArr.Count);
    For I:=0 to Length(Result.role)-1 do
      Result.role[i]:=lArr[i].Asstring;
    end;
  Result.sendNotification:=aJSON.Get('sendNotification',False);
  Result.view:=aJSON.Get('view','');
end;

class function TResolveAccessProposalRequestSerializer.Deserialize(aJSON : String) : TResolveAccessProposalRequest;

var
  lObj : TJSONObject;
begin
  Result := Default(TResolveAccessProposalRequest);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TRevisionSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('keepForever',keepForever);
    if (modifiedTime<>0) then
      Result.Add('modifiedTime',DateToISO8601(modifiedTime,True));
    Result.Add('publishAuto',publishAuto);
    Result.Add('publishedOutsideDomain',publishedOutsideDomain);
    Result.Add('published',published_);
  except
    Result.Free;
    raise;
  end;
end;

function TRevisionSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TRevisionSerializer.Deserialize(aJSON : TJSONObject) : TRevision;

begin
  Result := TRevision.Create;
  If (aJSON=Nil) then
    exit;
  Result.exportLinks:=JSONDataAsString(aJSON.Get('exportLinks',TJSONObject(Nil)));
  Result.id:=aJSON.Get('id','');
  Result.keepForever:=aJSON.Get('keepForever',False);
  Result.kind:=aJSON.Get('kind','');
  Result.lastModifyingUser:=TUser.Deserialize(aJSON.Get('lastModifyingUser',TJSONObject(Nil)));
  Result.md5Checksum:=aJSON.Get('md5Checksum','');
  Result.mimeType:=aJSON.Get('mimeType','');
  Result.modifiedTime:=ISO8601ToDateDef(aJSON.Get('modifiedTime',''),0,True);
  Result.originalFilename:=aJSON.Get('originalFilename','');
  Result.publishAuto:=aJSON.Get('publishAuto',False);
  Result.publishedLink:=aJSON.Get('publishedLink','');
  Result.publishedOutsideDomain:=aJSON.Get('publishedOutsideDomain',False);
  Result.published_:=aJSON.Get('published',False);
  Result.size:=aJSON.Get('size','');
end;

class function TRevisionSerializer.Deserialize(aJSON : String) : TRevision;

var
  lObj : TJSONObject;
begin
  Result := Default(TRevision);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TRevisionListSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('kind',kind);
    Result.Add('nextPageToken',nextPageToken);
    Arr:=TJSONArray.Create;
    Result.Add('revisions',Arr);
    For I:=0 to Length(revisions)-1 do
      Arr.Add(revisions[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TRevisionListSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TRevisionListSerializer.Deserialize(aJSON : TJSONObject) : TRevisionList;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TRevisionList.Create;
  If (aJSON=Nil) then
    exit;
  Result.kind:=aJSON.Get('kind','');
  Result.nextPageToken:=aJSON.Get('nextPageToken','');
  lArr:=aJSON.Get('revisions',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.revisions,lArr.Count);
    For I:=0 to Length(Result.revisions)-1 do
      Result.revisions[i]:=TRevision.Deserialize(lArr[i] as TJSONObject);
    end;
end;

class function TRevisionListSerializer.Deserialize(aJSON : String) : TRevisionList;

var
  lObj : TJSONObject;
begin
  Result := Default(TRevisionList);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TStartApprovalRequestSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('dueTime',dueTime);
    Result.Add('fileContentChangeBehavior',fileContentChangeBehavior);
    Result.Add('lockFile',lockFile);
    Result.Add('message',message);
    Arr:=TJSONArray.Create;
    Result.Add('reviewerEmails',Arr);
    For I:=0 to Length(reviewerEmails)-1 do
      Arr.Add(reviewerEmails[i]);
  except
    Result.Free;
    raise;
  end;
end;

function TStartApprovalRequestSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TStartApprovalRequestSerializer.Deserialize(aJSON : TJSONObject) : TStartApprovalRequest;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TStartApprovalRequest.Create;
  If (aJSON=Nil) then
    exit;
  Result.dueTime:=aJSON.Get('dueTime','');
  Result.fileContentChangeBehavior:=aJSON.Get('fileContentChangeBehavior','');
  Result.lockFile:=aJSON.Get('lockFile',False);
  Result.message:=aJSON.Get('message','');
  lArr:=aJSON.Get('reviewerEmails',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.reviewerEmails,lArr.Count);
    For I:=0 to Length(Result.reviewerEmails)-1 do
      Result.reviewerEmails[i]:=lArr[i].Asstring;
    end;
end;

class function TStartApprovalRequestSerializer.Deserialize(aJSON : String) : TStartApprovalRequest;

var
  lObj : TJSONObject;
begin
  Result := Default(TStartApprovalRequest);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TStartPageTokenSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('kind',kind);
    Result.Add('startPageToken',startPageToken);
  except
    Result.Free;
    raise;
  end;
end;

function TStartPageTokenSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TStartPageTokenSerializer.Deserialize(aJSON : TJSONObject) : TStartPageToken;

begin
  Result := TStartPageToken.Create;
  If (aJSON=Nil) then
    exit;
  Result.kind:=aJSON.Get('kind','');
  Result.startPageToken:=aJSON.Get('startPageToken','');
end;

class function TStartPageTokenSerializer.Deserialize(aJSON : String) : TStartPageToken;

var
  lObj : TJSONObject;
begin
  Result := Default(TStartPageToken);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TTeamDriveListSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('kind',kind);
    Result.Add('nextPageToken',nextPageToken);
    Arr:=TJSONArray.Create;
    Result.Add('teamDrives',Arr);
    For I:=0 to Length(teamDrives)-1 do
      Arr.Add(teamDrives[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TTeamDriveListSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TTeamDriveListSerializer.Deserialize(aJSON : TJSONObject) : TTeamDriveList;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TTeamDriveList.Create;
  If (aJSON=Nil) then
    exit;
  Result.kind:=aJSON.Get('kind','');
  Result.nextPageToken:=aJSON.Get('nextPageToken','');
  lArr:=aJSON.Get('teamDrives',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.teamDrives,lArr.Count);
    For I:=0 to Length(Result.teamDrives)-1 do
      Result.teamDrives[i]:=TTeamDrive.Deserialize(lArr[i] as TJSONObject);
    end;
end;

class function TTeamDriveListSerializer.Deserialize(aJSON : String) : TTeamDriveList;

var
  lObj : TJSONObject;
begin
  Result := Default(TTeamDriveList);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;


function TAccessProposalRoleAndViewArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TAccessProposalRoleAndViewArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TAccessProposalRoleAndViewArraySerializer.Deserialize(aJSON : TJSONArray) : TAccessProposalRoleAndViewArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TAccessProposalRoleAndView.Deserialize(aJSON[i] as TJSONObject);
end;

class function TAccessProposalRoleAndViewArraySerializer.Deserialize(aJSON : String) : TAccessProposalRoleAndViewArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TAccessProposalArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TAccessProposalArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TAccessProposalArraySerializer.Deserialize(aJSON : TJSONArray) : TAccessProposalArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TAccessProposal.Deserialize(aJSON[i] as TJSONObject);
end;

class function TAccessProposalArraySerializer.Deserialize(aJSON : String) : TAccessProposalArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TAddReviewerArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TAddReviewerArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TAddReviewerArraySerializer.Deserialize(aJSON : TJSONArray) : TAddReviewerArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TAddReviewer.Deserialize(aJSON[i] as TJSONObject);
end;

class function TAddReviewerArraySerializer.Deserialize(aJSON : String) : TAddReviewerArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TAppIconsArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TAppIconsArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TAppIconsArraySerializer.Deserialize(aJSON : TJSONArray) : TAppIconsArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TAppIcons.Deserialize(aJSON[i] as TJSONObject);
end;

class function TAppIconsArraySerializer.Deserialize(aJSON : String) : TAppIconsArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TApprovalArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TApprovalArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TApprovalArraySerializer.Deserialize(aJSON : TJSONArray) : TApprovalArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TApproval.Deserialize(aJSON[i] as TJSONObject);
end;

class function TApprovalArraySerializer.Deserialize(aJSON : String) : TApprovalArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TAppArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TAppArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TAppArraySerializer.Deserialize(aJSON : TJSONArray) : TAppArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TApp.Deserialize(aJSON[i] as TJSONObject);
end;

class function TAppArraySerializer.Deserialize(aJSON : String) : TAppArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TChangeArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TChangeArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TChangeArraySerializer.Deserialize(aJSON : TJSONArray) : TChangeArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TChange.Deserialize(aJSON[i] as TJSONObject);
end;

class function TChangeArraySerializer.Deserialize(aJSON : String) : TChangeArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TCommentArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TCommentArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TCommentArraySerializer.Deserialize(aJSON : TJSONArray) : TCommentArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TComment.Deserialize(aJSON[i] as TJSONObject);
end;

class function TCommentArraySerializer.Deserialize(aJSON : String) : TCommentArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TContentRestrictionArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TContentRestrictionArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TContentRestrictionArraySerializer.Deserialize(aJSON : TJSONArray) : TContentRestrictionArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TContentRestriction.Deserialize(aJSON[i] as TJSONObject);
end;

class function TContentRestrictionArraySerializer.Deserialize(aJSON : String) : TContentRestrictionArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TDriveArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TDriveArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TDriveArraySerializer.Deserialize(aJSON : TJSONArray) : TDriveArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TDrive.Deserialize(aJSON[i] as TJSONObject);
end;

class function TDriveArraySerializer.Deserialize(aJSON : String) : TDriveArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TFileArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TFileArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TFileArraySerializer.Deserialize(aJSON : TJSONArray) : TFileArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TFile.Deserialize(aJSON[i] as TJSONObject);
end;

class function TFileArraySerializer.Deserialize(aJSON : String) : TFileArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TLabelFieldModificationArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TLabelFieldModificationArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TLabelFieldModificationArraySerializer.Deserialize(aJSON : TJSONArray) : TLabelFieldModificationArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TLabelFieldModification.Deserialize(aJSON[i] as TJSONObject);
end;

class function TLabelFieldModificationArraySerializer.Deserialize(aJSON : String) : TLabelFieldModificationArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TLabelModificationArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TLabelModificationArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TLabelModificationArraySerializer.Deserialize(aJSON : TJSONArray) : TLabelModificationArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TLabelModification.Deserialize(aJSON[i] as TJSONObject);
end;

class function TLabelModificationArraySerializer.Deserialize(aJSON : String) : TLabelModificationArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TLabelArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TLabelArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TLabelArraySerializer.Deserialize(aJSON : TJSONArray) : TLabelArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TLabel.Deserialize(aJSON[i] as TJSONObject);
end;

class function TLabelArraySerializer.Deserialize(aJSON : String) : TLabelArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function stringArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i]);
  except
    Result.Free;
    raise;
  end;
end;


function stringArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function stringArraySerializer.Deserialize(aJSON : TJSONArray) : stringArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=aJSON[i].AsJSON;
end;

class function stringArraySerializer.Deserialize(aJSON : String) : stringArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TPermissionArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TPermissionArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TPermissionArraySerializer.Deserialize(aJSON : TJSONArray) : TPermissionArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TPermission.Deserialize(aJSON[i] as TJSONObject);
end;

class function TPermissionArraySerializer.Deserialize(aJSON : String) : TPermissionArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TReplaceReviewerArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TReplaceReviewerArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TReplaceReviewerArraySerializer.Deserialize(aJSON : TJSONArray) : TReplaceReviewerArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TReplaceReviewer.Deserialize(aJSON[i] as TJSONObject);
end;

class function TReplaceReviewerArraySerializer.Deserialize(aJSON : String) : TReplaceReviewerArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TReplyArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TReplyArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TReplyArraySerializer.Deserialize(aJSON : TJSONArray) : TReplyArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TReply.Deserialize(aJSON[i] as TJSONObject);
end;

class function TReplyArraySerializer.Deserialize(aJSON : String) : TReplyArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TReviewerResponseArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TReviewerResponseArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TReviewerResponseArraySerializer.Deserialize(aJSON : TJSONArray) : TReviewerResponseArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TReviewerResponse.Deserialize(aJSON[i] as TJSONObject);
end;

class function TReviewerResponseArraySerializer.Deserialize(aJSON : String) : TReviewerResponseArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TRevisionArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TRevisionArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TRevisionArraySerializer.Deserialize(aJSON : TJSONArray) : TRevisionArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TRevision.Deserialize(aJSON[i] as TJSONObject);
end;

class function TRevisionArraySerializer.Deserialize(aJSON : String) : TRevisionArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TTeamDriveArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TTeamDriveArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TTeamDriveArraySerializer.Deserialize(aJSON : TJSONArray) : TTeamDriveArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TTeamDrive.Deserialize(aJSON[i] as TJSONObject);
end;

class function TTeamDriveArraySerializer.Deserialize(aJSON : String) : TTeamDriveArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TUserArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TUserArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TUserArraySerializer.Deserialize(aJSON : TJSONArray) : TUserArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TUser.Deserialize(aJSON[i] as TJSONObject);
end;

class function TUserArraySerializer.Deserialize(aJSON : String) : TUserArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


end.
