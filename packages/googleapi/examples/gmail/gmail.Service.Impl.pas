{ -----------------------------------------------------------------------
  Do not edit !
  
  This file was automatically generated on 2026-10-03 10:20.
  Used command-line parameters:
     -s gmail -C codegen.ini -o gmail -q
  Source OpenAPI document data:
    Title: Gmail API
    Version: v1
  -----------------------------------------------------------------------}
unit gmail.Service.Impl;

{$mode objfpc}
{$h+}

interface

uses
  classes, fpopenapiclient
  , gmail.Service.Intf                     // Service definition 
  , gmail.Dto;

Type
  // Service IAttachments
  
  TAttachmentsProxy = Class (TFPOpenAPIServiceClient,IAttachments)
    Function Get(aId : string; aMessageId : string; aUserId : string = 'me') : TMessagePartBodyServiceResult;
  end;
  
  // Service IDelegates
  
  TDelegatesProxy = Class (TFPOpenAPIServiceClient,IDelegates)
    Function Create_(aRequest : TDelegate; aUserId : string = 'me') : TDelegateServiceResult;
    Function Delete(aDelegateEmail : string; aUserId : string = 'me') : TDelegatesDeleteResult;
    Function Get(aDelegateEmail : string; aUserId : string = 'me') : TDelegateServiceResult;
    Function List(aUserId : string = 'me') : TListDelegatesResponseServiceResult;
  end;
  
  // Service IDrafts
  
  TDraftsProxy = Class (TFPOpenAPIServiceClient,IDrafts)
    Function Create_(aRequest : TDraft; aUserId : string = 'me') : TDraftServiceResult;
    Function Delete(aId : string; aUserId : string = 'me') : TDraftsDeleteResult;
    Function Get(aId : string; aFormat : string = 'full'; aUserId : string = 'me') : TDraftServiceResult;
    Function List(aPageToken : string; aQ : string; aIncludeSpamTrash : boolean = false; aMaxResults : integer = 100; aUserId : string = 'me') : TListDraftsResponseServiceResult;
    Function Send(aRequest : TDraft; aUserId : string = 'me') : TMessageServiceResult;
    Function Update(aId : string; aRequest : TDraft; aUserId : string = 'me') : TDraftServiceResult;
  end;
  
  // Service IFilters
  
  TFiltersProxy = Class (TFPOpenAPIServiceClient,IFilters)
    Function Create_(aRequest : TFilter; aUserId : string = 'me') : TFilterServiceResult;
    Function Delete(aId : string; aUserId : string = 'me') : TFiltersDeleteResult;
    Function Get(aId : string; aUserId : string = 'me') : TFilterServiceResult;
    Function List(aUserId : string = 'me') : TListFiltersResponseServiceResult;
  end;
  
  // Service IForwardingAddresses
  
  TForwardingAddressesProxy = Class (TFPOpenAPIServiceClient,IForwardingAddresses)
    Function Create_(aRequest : TForwardingAddress; aUserId : string = 'me') : TForwardingAddressServiceResult;
    Function Delete(aForwardingEmail : string; aUserId : string = 'me') : TForwardingAddressesDeleteResult;
    Function Get(aForwardingEmail : string; aUserId : string = 'me') : TForwardingAddressServiceResult;
    Function List(aUserId : string = 'me') : TListForwardingAddressesResponseServiceResult;
  end;
  
  // Service IHistory
  
  THistoryProxy = Class (TFPOpenAPIServiceClient,IHistory)
    Function List(aHistoryTypes : string; aLabelId : string; aPageToken : string; aStartHistoryId : string; aMaxResults : integer = 100; aUserId : string = 'me') : TListHistoryResponseServiceResult;
  end;
  
  // Service IIdentities
  
  TIdentitiesProxy = Class (TFPOpenAPIServiceClient,IIdentities)
    Function Create_(aRequest : TCseIdentity; aUserId : string = 'me') : TCseIdentityServiceResult;
    Function Delete(aCseEmailAddress : string; aUserId : string = 'me') : TIdentitiesDeleteResult;
    Function Get(aCseEmailAddress : string; aUserId : string = 'me') : TCseIdentityServiceResult;
    Function List(aPageToken : string; aPageSize : integer = 20; aUserId : string = 'me') : TListCseIdentitiesResponseServiceResult;
    Function Patch(aEmailAddress : string; aRequest : TCseIdentity; aUserId : string = 'me') : TCseIdentityServiceResult;
  end;
  
  // Service IKeypairs
  
  TKeypairsProxy = Class (TFPOpenAPIServiceClient,IKeypairs)
    Function Create_(aRequest : TCseKeyPair; aChainValidation : string = 'all'; aUserId : string = 'me') : TCseKeyPairServiceResult;
    Function Disable(aKeyPairId : string; aRequest : TDisableCseKeyPairRequest; aUserId : string = 'me') : TCseKeyPairServiceResult;
    Function Enable(aKeyPairId : string; aRequest : TEnableCseKeyPairRequest; aUserId : string = 'me') : TCseKeyPairServiceResult;
    Function Get(aKeyPairId : string; aUserId : string = 'me') : TCseKeyPairServiceResult;
    Function List(aPageToken : string; aPageSize : integer = 20; aUserId : string = 'me') : TListCseKeyPairsResponseServiceResult;
  end;
  
  // Service ILabels
  
  TLabelsProxy = Class (TFPOpenAPIServiceClient,ILabels)
    Function Create_(aRequest : TLabel; aUserId : string = 'me') : TLabelServiceResult;
    Function Delete(aId : string; aUserId : string = 'me') : TLabelsDeleteResult;
    Function Get(aId : string; aUserId : string = 'me') : TLabelServiceResult;
    Function List(aUserId : string = 'me') : TListLabelsResponseServiceResult;
    Function Patch(aId : string; aRequest : TLabel; aUserId : string = 'me') : TLabelServiceResult;
    Function Update(aId : string; aRequest : TLabel; aUserId : string = 'me') : TLabelServiceResult;
  end;
  
  // Service IMessages
  
  TMessagesProxy = Class (TFPOpenAPIServiceClient,IMessages)
    Function Delete(aId : string; aUserId : string = 'me') : TMessagesDeleteResult;
    Function Get(aId : string; aMetadataHeaders : string; aFormat : string = 'full'; aUserId : string = 'me') : TMessageServiceResult;
    Function Import(aRequest : TMessage; aDeleted : boolean = false; aInternalDateSource : string = 'dateHeader'; aNeverMarkSpam : boolean = false; aProcessForCalendar : boolean = false; aUserId : string = 'me') : TMessageServiceResult;
    Function Insert(aRequest : TMessage; aDeleted : boolean = false; aInternalDateSource : string = 'receivedTime'; aUserId : string = 'me') : TMessageServiceResult;
    Function List(aLabelIds : string; aPageToken : string; aQ : string; aIncludeSpamTrash : boolean = false; aMaxResults : integer = 100; aUserId : string = 'me') : TListMessagesResponseServiceResult;
    Function Modify(aId : string; aRequest : TModifyMessageRequest; aUserId : string = 'me') : TMessageServiceResult;
    Function Send(aRequest : TMessage; aUserId : string = 'me') : TMessageServiceResult;
    Function Trash(aId : string; aUserId : string = 'me') : TMessageServiceResult;
    Function Untrash(aId : string; aUserId : string = 'me') : TMessageServiceResult;
  end;
  
  // Service ISendAs
  
  TSendAsProxy = Class (TFPOpenAPIServiceClient,ISendAs)
    Function Create_(aRequest : TSendAs; aUserId : string = 'me') : TSendAsServiceResult;
    Function Delete(aSendAsEmail : string; aUserId : string = 'me') : TSendAsDeleteResult;
    Function Get(aSendAsEmail : string; aUserId : string = 'me') : TSendAsServiceResult;
    Function List(aUserId : string = 'me') : TListSendAsResponseServiceResult;
    Function Patch(aSendAsEmail : string; aRequest : TSendAs; aUserId : string = 'me') : TSendAsServiceResult;
    Function Update(aSendAsEmail : string; aRequest : TSendAs; aUserId : string = 'me') : TSendAsServiceResult;
  end;
  
  // Service ISettings
  
  TSettingsProxy = Class (TFPOpenAPIServiceClient,ISettings)
    Function GetAutoForwarding(aUserId : string = 'me') : TAutoForwardingServiceResult;
    Function GetImap(aUserId : string = 'me') : TImapSettingsServiceResult;
    Function GetLanguage(aUserId : string = 'me') : TLanguageSettingsServiceResult;
    Function GetPop(aUserId : string = 'me') : TPopSettingsServiceResult;
    Function GetVacation(aUserId : string = 'me') : TVacationSettingsServiceResult;
    Function UpdateAutoForwarding(aRequest : TAutoForwarding; aUserId : string = 'me') : TAutoForwardingServiceResult;
    Function UpdateImap(aRequest : TImapSettings; aUserId : string = 'me') : TImapSettingsServiceResult;
    Function UpdateLanguage(aRequest : TLanguageSettings; aUserId : string = 'me') : TLanguageSettingsServiceResult;
    Function UpdatePop(aRequest : TPopSettings; aUserId : string = 'me') : TPopSettingsServiceResult;
    Function UpdateVacation(aRequest : TVacationSettings; aUserId : string = 'me') : TVacationSettingsServiceResult;
  end;
  
  // Service ISmimeInfo
  
  TSmimeInfoProxy = Class (TFPOpenAPIServiceClient,ISmimeInfo)
    Function Delete(aId : string; aSendAsEmail : string; aUserId : string = 'me') : TSmimeInfoDeleteResult;
    Function Get(aId : string; aSendAsEmail : string; aUserId : string = 'me') : TSmimeInfoServiceResult;
    Function Insert(aSendAsEmail : string; aRequest : TSmimeInfo; aUserId : string = 'me') : TSmimeInfoServiceResult;
    Function List(aSendAsEmail : string; aUserId : string = 'me') : TListSmimeInfoResponseServiceResult;
  end;
  
  // Service IThreads
  
  TThreadsProxy = Class (TFPOpenAPIServiceClient,IThreads)
    Function Delete(aId : string; aUserId : string = 'me') : TThreadsDeleteResult;
    Function Get(aId : string; aMetadataHeaders : string; aFormat : string = 'full'; aUserId : string = 'me') : TThread_ServiceResult;
    Function List(aLabelIds : string; aPageToken : string; aQ : string; aIncludeSpamTrash : boolean = false; aMaxResults : integer = 100; aUserId : string = 'me') : TListThreadsResponseServiceResult;
    Function Modify(aId : string; aRequest : TModifyThreadRequest; aUserId : string = 'me') : TThread_ServiceResult;
    Function Trash(aId : string; aUserId : string = 'me') : TThread_ServiceResult;
    Function Untrash(aId : string; aUserId : string = 'me') : TThread_ServiceResult;
  end;
  
  // Service IUsers
  
  TUsersProxy = Class (TFPOpenAPIServiceClient,IUsers)
    Function GetProfile(aUserId : string = 'me') : TProfileServiceResult;
    Function Watch(aRequest : TWatchRequest; aUserId : string = 'me') : TWatchResponseServiceResult;
  end;
  

implementation

uses
  SysUtils, DateUtils
  , gmail.Serializer;

Function TAttachmentsProxy.Get(aId : string; aMessageId : string; aUserId : string = 'me') : TMessagePartBodyServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/messages/{messageId}/attachments/{id}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TMessagePartBodyServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'id',aId);
  lUrl:=ReplacePathParam(lURL,'messageId',aMessageId);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TMessagePartBodyServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TMessagePartBody.Deserialize(lResponse.Content);
end;

Function TDelegatesProxy.Create_(aRequest : TDelegate; aUserId : string = 'me') : TDelegateServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/delegates';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TDelegateServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TDelegateServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TDelegate.Deserialize(lResponse.Content);
end;

Function TDelegatesProxy.Delete(aDelegateEmail : string; aUserId : string = 'me') : TDelegatesDeleteResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/delegates/{delegateEmail}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TDelegatesDeleteResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'delegateEmail',aDelegateEmail);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result.FStatusCode:=lResponse.StatusCode;
  Result.FStatusText:=lResponse.StatusText;
  Result.FContentType:=lResponse.ContentType;
  Result.FRawContent:=lResponse.Content;
  Result.FRawStream:=lResponse.ContentStream;
  
  case lResponse.StatusCode of
    200: begin
      Result.FResponseKind:=TDelegatesDeleteResponseKind.DelegatesDeleterkSuccess;
    end;
    else begin
      Result.FResponseKind:=TDelegatesDeleteResponseKind.DelegatesDeleterkUnexpected;
    end;
  end;
end;

Function TDelegatesProxy.Get(aDelegateEmail : string; aUserId : string = 'me') : TDelegateServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/delegates/{delegateEmail}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TDelegateServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'delegateEmail',aDelegateEmail);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TDelegateServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TDelegate.Deserialize(lResponse.Content);
end;

Function TDelegatesProxy.List(aUserId : string = 'me') : TListDelegatesResponseServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/delegates';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TListDelegatesResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TListDelegatesResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TListDelegatesResponse.Deserialize(lResponse.Content);
end;

Function TDraftsProxy.Create_(aRequest : TDraft; aUserId : string = 'me') : TDraftServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/drafts';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TDraftServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TDraftServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TDraft.Deserialize(lResponse.Content);
end;

Function TDraftsProxy.Delete(aId : string; aUserId : string = 'me') : TDraftsDeleteResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/drafts/{id}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TDraftsDeleteResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'id',aId);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result.FStatusCode:=lResponse.StatusCode;
  Result.FStatusText:=lResponse.StatusText;
  Result.FContentType:=lResponse.ContentType;
  Result.FRawContent:=lResponse.Content;
  Result.FRawStream:=lResponse.ContentStream;
  
  case lResponse.StatusCode of
    200: begin
      Result.FResponseKind:=TDraftsDeleteResponseKind.DraftsDeleterkSuccess;
    end;
    else begin
      Result.FResponseKind:=TDraftsDeleteResponseKind.DraftsDeleterkUnexpected;
    end;
  end;
end;

Function TDraftsProxy.Get(aId : string; aFormat : string = 'full'; aUserId : string = 'me') : TDraftServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/drafts/{id}';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TDraftServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'id',aId);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lQuery:=ConcatRestParam(lQuery,'format',aFormat);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TDraftServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TDraft.Deserialize(lResponse.Content);
end;

Function TDraftsProxy.List(aPageToken : string; aQ : string; aIncludeSpamTrash : boolean = false; aMaxResults : integer = 100; aUserId : string = 'me') : TListDraftsResponseServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/drafts';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TListDraftsResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lQuery:=ConcatRestParam(lQuery,'includeSpamTrash',cRESTBooleans[aIncludeSpamTrash]);
  lQuery:=ConcatRestParam(lQuery,'maxResults',IntToStr(aMaxResults));
  lQuery:=ConcatRestParam(lQuery,'pageToken',aPageToken);
  lQuery:=ConcatRestParam(lQuery,'q',aQ);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TListDraftsResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TListDraftsResponse.Deserialize(lResponse.Content);
end;

Function TDraftsProxy.Send(aRequest : TDraft; aUserId : string = 'me') : TMessageServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/drafts/send';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TMessageServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TMessageServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TMessage.Deserialize(lResponse.Content);
end;

Function TDraftsProxy.Update(aId : string; aRequest : TDraft; aUserId : string = 'me') : TDraftServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/drafts/{id}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TDraftServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'id',aId);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('put',lURL,aRequest.Serialize);
  Result:=TDraftServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TDraft.Deserialize(lResponse.Content);
end;

Function TFiltersProxy.Create_(aRequest : TFilter; aUserId : string = 'me') : TFilterServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/filters';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TFilterServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TFilterServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TFilter.Deserialize(lResponse.Content);
end;

Function TFiltersProxy.Delete(aId : string; aUserId : string = 'me') : TFiltersDeleteResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/filters/{id}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TFiltersDeleteResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'id',aId);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result.FStatusCode:=lResponse.StatusCode;
  Result.FStatusText:=lResponse.StatusText;
  Result.FContentType:=lResponse.ContentType;
  Result.FRawContent:=lResponse.Content;
  Result.FRawStream:=lResponse.ContentStream;
  
  case lResponse.StatusCode of
    200: begin
      Result.FResponseKind:=TFiltersDeleteResponseKind.FiltersDeleterkSuccess;
    end;
    else begin
      Result.FResponseKind:=TFiltersDeleteResponseKind.FiltersDeleterkUnexpected;
    end;
  end;
end;

Function TFiltersProxy.Get(aId : string; aUserId : string = 'me') : TFilterServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/filters/{id}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TFilterServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'id',aId);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TFilterServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TFilter.Deserialize(lResponse.Content);
end;

Function TFiltersProxy.List(aUserId : string = 'me') : TListFiltersResponseServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/filters';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TListFiltersResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TListFiltersResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TListFiltersResponse.Deserialize(lResponse.Content);
end;

Function TForwardingAddressesProxy.Create_(aRequest : TForwardingAddress; aUserId : string = 'me') : TForwardingAddressServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/forwardingAddresses';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TForwardingAddressServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TForwardingAddressServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TForwardingAddress.Deserialize(lResponse.Content);
end;

Function TForwardingAddressesProxy.Delete(aForwardingEmail : string; aUserId : string = 'me') : TForwardingAddressesDeleteResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/forwardingAddresses/{forwardingEmail}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TForwardingAddressesDeleteResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'forwardingEmail',aForwardingEmail);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result.FStatusCode:=lResponse.StatusCode;
  Result.FStatusText:=lResponse.StatusText;
  Result.FContentType:=lResponse.ContentType;
  Result.FRawContent:=lResponse.Content;
  Result.FRawStream:=lResponse.ContentStream;
  
  case lResponse.StatusCode of
    200: begin
      Result.FResponseKind:=TForwardingAddressesDeleteResponseKind.ForwardingAddressesDeleterkSuccess;
    end;
    else begin
      Result.FResponseKind:=TForwardingAddressesDeleteResponseKind.ForwardingAddressesDeleterkUnexpected;
    end;
  end;
end;

Function TForwardingAddressesProxy.Get(aForwardingEmail : string; aUserId : string = 'me') : TForwardingAddressServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/forwardingAddresses/{forwardingEmail}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TForwardingAddressServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'forwardingEmail',aForwardingEmail);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TForwardingAddressServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TForwardingAddress.Deserialize(lResponse.Content);
end;

Function TForwardingAddressesProxy.List(aUserId : string = 'me') : TListForwardingAddressesResponseServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/forwardingAddresses';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TListForwardingAddressesResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TListForwardingAddressesResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TListForwardingAddressesResponse.Deserialize(lResponse.Content);
end;

Function THistoryProxy.List(aHistoryTypes : string; aLabelId : string; aPageToken : string; aStartHistoryId : string; aMaxResults : integer = 100; aUserId : string = 'me') : TListHistoryResponseServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/history';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TListHistoryResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lQuery:=ConcatRestParam(lQuery,'historyTypes',aHistoryTypes);
  lQuery:=ConcatRestParam(lQuery,'labelId',aLabelId);
  lQuery:=ConcatRestParam(lQuery,'maxResults',IntToStr(aMaxResults));
  lQuery:=ConcatRestParam(lQuery,'pageToken',aPageToken);
  lQuery:=ConcatRestParam(lQuery,'startHistoryId',aStartHistoryId);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TListHistoryResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TListHistoryResponse.Deserialize(lResponse.Content);
end;

Function TIdentitiesProxy.Create_(aRequest : TCseIdentity; aUserId : string = 'me') : TCseIdentityServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/cse/identities';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TCseIdentityServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TCseIdentityServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TCseIdentity.Deserialize(lResponse.Content);
end;

Function TIdentitiesProxy.Delete(aCseEmailAddress : string; aUserId : string = 'me') : TIdentitiesDeleteResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/cse/identities/{cseEmailAddress}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TIdentitiesDeleteResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'cseEmailAddress',aCseEmailAddress);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result.FStatusCode:=lResponse.StatusCode;
  Result.FStatusText:=lResponse.StatusText;
  Result.FContentType:=lResponse.ContentType;
  Result.FRawContent:=lResponse.Content;
  Result.FRawStream:=lResponse.ContentStream;
  
  case lResponse.StatusCode of
    200: begin
      Result.FResponseKind:=TIdentitiesDeleteResponseKind.IdentitiesDeleterkSuccess;
    end;
    else begin
      Result.FResponseKind:=TIdentitiesDeleteResponseKind.IdentitiesDeleterkUnexpected;
    end;
  end;
end;

Function TIdentitiesProxy.Get(aCseEmailAddress : string; aUserId : string = 'me') : TCseIdentityServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/cse/identities/{cseEmailAddress}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TCseIdentityServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'cseEmailAddress',aCseEmailAddress);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TCseIdentityServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TCseIdentity.Deserialize(lResponse.Content);
end;

Function TIdentitiesProxy.List(aPageToken : string; aPageSize : integer = 20; aUserId : string = 'me') : TListCseIdentitiesResponseServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/cse/identities';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TListCseIdentitiesResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lQuery:=ConcatRestParam(lQuery,'pageSize',IntToStr(aPageSize));
  lQuery:=ConcatRestParam(lQuery,'pageToken',aPageToken);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TListCseIdentitiesResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TListCseIdentitiesResponse.Deserialize(lResponse.Content);
end;

Function TIdentitiesProxy.Patch(aEmailAddress : string; aRequest : TCseIdentity; aUserId : string = 'me') : TCseIdentityServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/cse/identities/{emailAddress}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TCseIdentityServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'emailAddress',aEmailAddress);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('patch',lURL,aRequest.Serialize);
  Result:=TCseIdentityServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TCseIdentity.Deserialize(lResponse.Content);
end;

Function TKeypairsProxy.Create_(aRequest : TCseKeyPair; aChainValidation : string = 'all'; aUserId : string = 'me') : TCseKeyPairServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/cse/keypairs';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TCseKeyPairServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lQuery:=ConcatRestParam(lQuery,'chainValidation',aChainValidation);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TCseKeyPairServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TCseKeyPair.Deserialize(lResponse.Content);
end;

Function TKeypairsProxy.Disable(aKeyPairId : string; aRequest : TDisableCseKeyPairRequest; aUserId : string = 'me') : TCseKeyPairServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/cse/keypairs/{keyPairId}:disable';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TCseKeyPairServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'keyPairId',aKeyPairId);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TCseKeyPairServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TCseKeyPair.Deserialize(lResponse.Content);
end;

Function TKeypairsProxy.Enable(aKeyPairId : string; aRequest : TEnableCseKeyPairRequest; aUserId : string = 'me') : TCseKeyPairServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/cse/keypairs/{keyPairId}:enable';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TCseKeyPairServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'keyPairId',aKeyPairId);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TCseKeyPairServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TCseKeyPair.Deserialize(lResponse.Content);
end;

Function TKeypairsProxy.Get(aKeyPairId : string; aUserId : string = 'me') : TCseKeyPairServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/cse/keypairs/{keyPairId}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TCseKeyPairServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'keyPairId',aKeyPairId);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TCseKeyPairServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TCseKeyPair.Deserialize(lResponse.Content);
end;

Function TKeypairsProxy.List(aPageToken : string; aPageSize : integer = 20; aUserId : string = 'me') : TListCseKeyPairsResponseServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/cse/keypairs';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TListCseKeyPairsResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lQuery:=ConcatRestParam(lQuery,'pageSize',IntToStr(aPageSize));
  lQuery:=ConcatRestParam(lQuery,'pageToken',aPageToken);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TListCseKeyPairsResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TListCseKeyPairsResponse.Deserialize(lResponse.Content);
end;

Function TLabelsProxy.Create_(aRequest : TLabel; aUserId : string = 'me') : TLabelServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/labels';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TLabelServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TLabelServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TLabel.Deserialize(lResponse.Content);
end;

Function TLabelsProxy.Delete(aId : string; aUserId : string = 'me') : TLabelsDeleteResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/labels/{id}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TLabelsDeleteResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'id',aId);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result.FStatusCode:=lResponse.StatusCode;
  Result.FStatusText:=lResponse.StatusText;
  Result.FContentType:=lResponse.ContentType;
  Result.FRawContent:=lResponse.Content;
  Result.FRawStream:=lResponse.ContentStream;
  
  case lResponse.StatusCode of
    200: begin
      Result.FResponseKind:=TLabelsDeleteResponseKind.LabelsDeleterkSuccess;
    end;
    else begin
      Result.FResponseKind:=TLabelsDeleteResponseKind.LabelsDeleterkUnexpected;
    end;
  end;
end;

Function TLabelsProxy.Get(aId : string; aUserId : string = 'me') : TLabelServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/labels/{id}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TLabelServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'id',aId);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TLabelServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TLabel.Deserialize(lResponse.Content);
end;

Function TLabelsProxy.List(aUserId : string = 'me') : TListLabelsResponseServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/labels';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TListLabelsResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TListLabelsResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TListLabelsResponse.Deserialize(lResponse.Content);
end;

Function TLabelsProxy.Patch(aId : string; aRequest : TLabel; aUserId : string = 'me') : TLabelServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/labels/{id}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TLabelServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'id',aId);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('patch',lURL,aRequest.Serialize);
  Result:=TLabelServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TLabel.Deserialize(lResponse.Content);
end;

Function TLabelsProxy.Update(aId : string; aRequest : TLabel; aUserId : string = 'me') : TLabelServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/labels/{id}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TLabelServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'id',aId);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('put',lURL,aRequest.Serialize);
  Result:=TLabelServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TLabel.Deserialize(lResponse.Content);
end;

Function TMessagesProxy.Delete(aId : string; aUserId : string = 'me') : TMessagesDeleteResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/messages/{id}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TMessagesDeleteResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'id',aId);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result.FStatusCode:=lResponse.StatusCode;
  Result.FStatusText:=lResponse.StatusText;
  Result.FContentType:=lResponse.ContentType;
  Result.FRawContent:=lResponse.Content;
  Result.FRawStream:=lResponse.ContentStream;
  
  case lResponse.StatusCode of
    200: begin
      Result.FResponseKind:=TMessagesDeleteResponseKind.MessagesDeleterkSuccess;
    end;
    else begin
      Result.FResponseKind:=TMessagesDeleteResponseKind.MessagesDeleterkUnexpected;
    end;
  end;
end;

Function TMessagesProxy.Get(aId : string; aMetadataHeaders : string; aFormat : string = 'full'; aUserId : string = 'me') : TMessageServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/messages/{id}';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TMessageServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'id',aId);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lQuery:=ConcatRestParam(lQuery,'format',aFormat);
  lQuery:=ConcatRestParam(lQuery,'metadataHeaders',aMetadataHeaders);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TMessageServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TMessage.Deserialize(lResponse.Content);
end;

Function TMessagesProxy.Import(aRequest : TMessage; aDeleted : boolean = false; aInternalDateSource : string = 'dateHeader'; aNeverMarkSpam : boolean = false; aProcessForCalendar : boolean = false; aUserId : string = 'me') : TMessageServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/messages/import';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TMessageServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lQuery:=ConcatRestParam(lQuery,'deleted',cRESTBooleans[aDeleted]);
  lQuery:=ConcatRestParam(lQuery,'internalDateSource',aInternalDateSource);
  lQuery:=ConcatRestParam(lQuery,'neverMarkSpam',cRESTBooleans[aNeverMarkSpam]);
  lQuery:=ConcatRestParam(lQuery,'processForCalendar',cRESTBooleans[aProcessForCalendar]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TMessageServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TMessage.Deserialize(lResponse.Content);
end;

Function TMessagesProxy.Insert(aRequest : TMessage; aDeleted : boolean = false; aInternalDateSource : string = 'receivedTime'; aUserId : string = 'me') : TMessageServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/messages';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TMessageServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lQuery:=ConcatRestParam(lQuery,'deleted',cRESTBooleans[aDeleted]);
  lQuery:=ConcatRestParam(lQuery,'internalDateSource',aInternalDateSource);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TMessageServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TMessage.Deserialize(lResponse.Content);
end;

Function TMessagesProxy.List(aLabelIds : string; aPageToken : string; aQ : string; aIncludeSpamTrash : boolean = false; aMaxResults : integer = 100; aUserId : string = 'me') : TListMessagesResponseServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/messages';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TListMessagesResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lQuery:=ConcatRestParam(lQuery,'includeSpamTrash',cRESTBooleans[aIncludeSpamTrash]);
  lQuery:=ConcatRestParam(lQuery,'labelIds',aLabelIds);
  lQuery:=ConcatRestParam(lQuery,'maxResults',IntToStr(aMaxResults));
  lQuery:=ConcatRestParam(lQuery,'pageToken',aPageToken);
  lQuery:=ConcatRestParam(lQuery,'q',aQ);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TListMessagesResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TListMessagesResponse.Deserialize(lResponse.Content);
end;

Function TMessagesProxy.Modify(aId : string; aRequest : TModifyMessageRequest; aUserId : string = 'me') : TMessageServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/messages/{id}/modify';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TMessageServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'id',aId);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TMessageServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TMessage.Deserialize(lResponse.Content);
end;

Function TMessagesProxy.Send(aRequest : TMessage; aUserId : string = 'me') : TMessageServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/messages/send';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TMessageServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TMessageServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TMessage.Deserialize(lResponse.Content);
end;

Function TMessagesProxy.Trash(aId : string; aUserId : string = 'me') : TMessageServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/messages/{id}/trash';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TMessageServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'id',aId);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('post',lURL,'');
  Result:=TMessageServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TMessage.Deserialize(lResponse.Content);
end;

Function TMessagesProxy.Untrash(aId : string; aUserId : string = 'me') : TMessageServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/messages/{id}/untrash';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TMessageServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'id',aId);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('post',lURL,'');
  Result:=TMessageServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TMessage.Deserialize(lResponse.Content);
end;

Function TSendAsProxy.Create_(aRequest : TSendAs; aUserId : string = 'me') : TSendAsServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/sendAs';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TSendAsServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TSendAsServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TSendAs.Deserialize(lResponse.Content);
end;

Function TSendAsProxy.Delete(aSendAsEmail : string; aUserId : string = 'me') : TSendAsDeleteResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/sendAs/{sendAsEmail}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TSendAsDeleteResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'sendAsEmail',aSendAsEmail);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result.FStatusCode:=lResponse.StatusCode;
  Result.FStatusText:=lResponse.StatusText;
  Result.FContentType:=lResponse.ContentType;
  Result.FRawContent:=lResponse.Content;
  Result.FRawStream:=lResponse.ContentStream;
  
  case lResponse.StatusCode of
    200: begin
      Result.FResponseKind:=TSendAsDeleteResponseKind.SendAsDeleterkSuccess;
    end;
    else begin
      Result.FResponseKind:=TSendAsDeleteResponseKind.SendAsDeleterkUnexpected;
    end;
  end;
end;

Function TSendAsProxy.Get(aSendAsEmail : string; aUserId : string = 'me') : TSendAsServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/sendAs/{sendAsEmail}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TSendAsServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'sendAsEmail',aSendAsEmail);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TSendAsServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TSendAs.Deserialize(lResponse.Content);
end;

Function TSendAsProxy.List(aUserId : string = 'me') : TListSendAsResponseServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/sendAs';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TListSendAsResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TListSendAsResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TListSendAsResponse.Deserialize(lResponse.Content);
end;

Function TSendAsProxy.Patch(aSendAsEmail : string; aRequest : TSendAs; aUserId : string = 'me') : TSendAsServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/sendAs/{sendAsEmail}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TSendAsServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'sendAsEmail',aSendAsEmail);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('patch',lURL,aRequest.Serialize);
  Result:=TSendAsServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TSendAs.Deserialize(lResponse.Content);
end;

Function TSendAsProxy.Update(aSendAsEmail : string; aRequest : TSendAs; aUserId : string = 'me') : TSendAsServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/sendAs/{sendAsEmail}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TSendAsServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'sendAsEmail',aSendAsEmail);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('put',lURL,aRequest.Serialize);
  Result:=TSendAsServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TSendAs.Deserialize(lResponse.Content);
end;

Function TSettingsProxy.GetAutoForwarding(aUserId : string = 'me') : TAutoForwardingServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/autoForwarding';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TAutoForwardingServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TAutoForwardingServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TAutoForwarding.Deserialize(lResponse.Content);
end;

Function TSettingsProxy.GetImap(aUserId : string = 'me') : TImapSettingsServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/imap';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TImapSettingsServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TImapSettingsServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TImapSettings.Deserialize(lResponse.Content);
end;

Function TSettingsProxy.GetLanguage(aUserId : string = 'me') : TLanguageSettingsServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/language';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TLanguageSettingsServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TLanguageSettingsServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TLanguageSettings.Deserialize(lResponse.Content);
end;

Function TSettingsProxy.GetPop(aUserId : string = 'me') : TPopSettingsServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/pop';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TPopSettingsServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TPopSettingsServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TPopSettings.Deserialize(lResponse.Content);
end;

Function TSettingsProxy.GetVacation(aUserId : string = 'me') : TVacationSettingsServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/vacation';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TVacationSettingsServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TVacationSettingsServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TVacationSettings.Deserialize(lResponse.Content);
end;

Function TSettingsProxy.UpdateAutoForwarding(aRequest : TAutoForwarding; aUserId : string = 'me') : TAutoForwardingServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/autoForwarding';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TAutoForwardingServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('put',lURL,aRequest.Serialize);
  Result:=TAutoForwardingServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TAutoForwarding.Deserialize(lResponse.Content);
end;

Function TSettingsProxy.UpdateImap(aRequest : TImapSettings; aUserId : string = 'me') : TImapSettingsServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/imap';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TImapSettingsServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('put',lURL,aRequest.Serialize);
  Result:=TImapSettingsServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TImapSettings.Deserialize(lResponse.Content);
end;

Function TSettingsProxy.UpdateLanguage(aRequest : TLanguageSettings; aUserId : string = 'me') : TLanguageSettingsServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/language';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TLanguageSettingsServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('put',lURL,aRequest.Serialize);
  Result:=TLanguageSettingsServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TLanguageSettings.Deserialize(lResponse.Content);
end;

Function TSettingsProxy.UpdatePop(aRequest : TPopSettings; aUserId : string = 'me') : TPopSettingsServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/pop';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TPopSettingsServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('put',lURL,aRequest.Serialize);
  Result:=TPopSettingsServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TPopSettings.Deserialize(lResponse.Content);
end;

Function TSettingsProxy.UpdateVacation(aRequest : TVacationSettings; aUserId : string = 'me') : TVacationSettingsServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/vacation';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TVacationSettingsServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('put',lURL,aRequest.Serialize);
  Result:=TVacationSettingsServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TVacationSettings.Deserialize(lResponse.Content);
end;

Function TSmimeInfoProxy.Delete(aId : string; aSendAsEmail : string; aUserId : string = 'me') : TSmimeInfoDeleteResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/sendAs/{sendAsEmail}/smimeInfo/{id}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TSmimeInfoDeleteResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'id',aId);
  lUrl:=ReplacePathParam(lURL,'sendAsEmail',aSendAsEmail);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result.FStatusCode:=lResponse.StatusCode;
  Result.FStatusText:=lResponse.StatusText;
  Result.FContentType:=lResponse.ContentType;
  Result.FRawContent:=lResponse.Content;
  Result.FRawStream:=lResponse.ContentStream;
  
  case lResponse.StatusCode of
    200: begin
      Result.FResponseKind:=TSmimeInfoDeleteResponseKind.SmimeInfoDeleterkSuccess;
    end;
    else begin
      Result.FResponseKind:=TSmimeInfoDeleteResponseKind.SmimeInfoDeleterkUnexpected;
    end;
  end;
end;

Function TSmimeInfoProxy.Get(aId : string; aSendAsEmail : string; aUserId : string = 'me') : TSmimeInfoServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/sendAs/{sendAsEmail}/smimeInfo/{id}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TSmimeInfoServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'id',aId);
  lUrl:=ReplacePathParam(lURL,'sendAsEmail',aSendAsEmail);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TSmimeInfoServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TSmimeInfo.Deserialize(lResponse.Content);
end;

Function TSmimeInfoProxy.Insert(aSendAsEmail : string; aRequest : TSmimeInfo; aUserId : string = 'me') : TSmimeInfoServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/sendAs/{sendAsEmail}/smimeInfo';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TSmimeInfoServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'sendAsEmail',aSendAsEmail);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TSmimeInfoServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TSmimeInfo.Deserialize(lResponse.Content);
end;

Function TSmimeInfoProxy.List(aSendAsEmail : string; aUserId : string = 'me') : TListSmimeInfoResponseServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/settings/sendAs/{sendAsEmail}/smimeInfo';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TListSmimeInfoResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'sendAsEmail',aSendAsEmail);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TListSmimeInfoResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TListSmimeInfoResponse.Deserialize(lResponse.Content);
end;

Function TThreadsProxy.Delete(aId : string; aUserId : string = 'me') : TThreadsDeleteResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/threads/{id}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TThreadsDeleteResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'id',aId);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result.FStatusCode:=lResponse.StatusCode;
  Result.FStatusText:=lResponse.StatusText;
  Result.FContentType:=lResponse.ContentType;
  Result.FRawContent:=lResponse.Content;
  Result.FRawStream:=lResponse.ContentStream;
  
  case lResponse.StatusCode of
    200: begin
      Result.FResponseKind:=TThreadsDeleteResponseKind.ThreadsDeleterkSuccess;
    end;
    else begin
      Result.FResponseKind:=TThreadsDeleteResponseKind.ThreadsDeleterkUnexpected;
    end;
  end;
end;

Function TThreadsProxy.Get(aId : string; aMetadataHeaders : string; aFormat : string = 'full'; aUserId : string = 'me') : TThread_ServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/threads/{id}';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TThread_ServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'id',aId);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lQuery:=ConcatRestParam(lQuery,'format',aFormat);
  lQuery:=ConcatRestParam(lQuery,'metadataHeaders',aMetadataHeaders);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TThread_ServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TThread_.Deserialize(lResponse.Content);
end;

Function TThreadsProxy.List(aLabelIds : string; aPageToken : string; aQ : string; aIncludeSpamTrash : boolean = false; aMaxResults : integer = 100; aUserId : string = 'me') : TListThreadsResponseServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/threads';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TListThreadsResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lQuery:=ConcatRestParam(lQuery,'includeSpamTrash',cRESTBooleans[aIncludeSpamTrash]);
  lQuery:=ConcatRestParam(lQuery,'labelIds',aLabelIds);
  lQuery:=ConcatRestParam(lQuery,'maxResults',IntToStr(aMaxResults));
  lQuery:=ConcatRestParam(lQuery,'pageToken',aPageToken);
  lQuery:=ConcatRestParam(lQuery,'q',aQ);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TListThreadsResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TListThreadsResponse.Deserialize(lResponse.Content);
end;

Function TThreadsProxy.Modify(aId : string; aRequest : TModifyThreadRequest; aUserId : string = 'me') : TThread_ServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/threads/{id}/modify';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TThread_ServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'id',aId);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TThread_ServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TThread_.Deserialize(lResponse.Content);
end;

Function TThreadsProxy.Trash(aId : string; aUserId : string = 'me') : TThread_ServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/threads/{id}/trash';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TThread_ServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'id',aId);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('post',lURL,'');
  Result:=TThread_ServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TThread_.Deserialize(lResponse.Content);
end;

Function TThreadsProxy.Untrash(aId : string; aUserId : string = 'me') : TThread_ServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/threads/{id}/untrash';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TThread_ServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'id',aId);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('post',lURL,'');
  Result:=TThread_ServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TThread_.Deserialize(lResponse.Content);
end;

Function TUsersProxy.GetProfile(aUserId : string = 'me') : TProfileServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/profile';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TProfileServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TProfileServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TProfile.Deserialize(lResponse.Content);
end;

Function TUsersProxy.Watch(aRequest : TWatchRequest; aUserId : string = 'me') : TWatchResponseServiceResult;

const
  lMethodURL = '/gmail/v1/users/{userId}/watch';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TWatchResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'userId',aUserId);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TWatchResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TWatchResponse.Deserialize(lResponse.Content);
end;


end.
