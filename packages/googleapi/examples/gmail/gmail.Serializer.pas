{ -----------------------------------------------------------------------
  Do not edit !
  
  This file was automatically generated on 2026-10-03 10:20.
  Used command-line parameters:
     -s gmail -C codegen.ini -o gmail -q
  Source OpenAPI document data:
    Title: Gmail API
    Version: v1
  -----------------------------------------------------------------------}
unit gmail.Serializer;

interface

{$mode objfpc}
{$h+}
{$modeswitch typehelpers}


uses
  Types,
  fpJSON,
  gmail.Dto;

Type
  TAutoForwardingSerializer = class helper for TAutoForwarding
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TAutoForwarding; overload; static;
    class function Deserialize(aJSON : String) : TAutoForwarding; overload; static;
  end;
  
  TBatchDeleteMessagesRequestSerializer = class helper for TBatchDeleteMessagesRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TBatchDeleteMessagesRequest; overload; static;
    class function Deserialize(aJSON : String) : TBatchDeleteMessagesRequest; overload; static;
  end;
  
  TClassificationLabelFieldValueSerializer = class helper for TClassificationLabelFieldValue
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TClassificationLabelFieldValue; overload; static;
    class function Deserialize(aJSON : String) : TClassificationLabelFieldValue; overload; static;
  end;
  
  TClassificationLabelValueSerializer = class helper for TClassificationLabelValue
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TClassificationLabelValue; overload; static;
    class function Deserialize(aJSON : String) : TClassificationLabelValue; overload; static;
  end;
  
  TBatchModifyMessagesRequestSerializer = class helper for TBatchModifyMessagesRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TBatchModifyMessagesRequest; overload; static;
    class function Deserialize(aJSON : String) : TBatchModifyMessagesRequest; overload; static;
  end;
  
  TSignAndEncryptKeyPairsSerializer = class helper for TSignAndEncryptKeyPairs
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TSignAndEncryptKeyPairs; overload; static;
    class function Deserialize(aJSON : String) : TSignAndEncryptKeyPairs; overload; static;
  end;
  
  TCseIdentitySerializer = class helper for TCseIdentity
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TCseIdentity; overload; static;
    class function Deserialize(aJSON : String) : TCseIdentity; overload; static;
  end;
  
  THardwareKeyMetadataSerializer = class helper for THardwareKeyMetadata
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : THardwareKeyMetadata; overload; static;
    class function Deserialize(aJSON : String) : THardwareKeyMetadata; overload; static;
  end;
  
  TKaclsKeyMetadataSerializer = class helper for TKaclsKeyMetadata
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TKaclsKeyMetadata; overload; static;
    class function Deserialize(aJSON : String) : TKaclsKeyMetadata; overload; static;
  end;
  
  TCsePrivateKeyMetadataSerializer = class helper for TCsePrivateKeyMetadata
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TCsePrivateKeyMetadata; overload; static;
    class function Deserialize(aJSON : String) : TCsePrivateKeyMetadata; overload; static;
  end;
  
  TCseKeyPairSerializer = class helper for TCseKeyPair
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TCseKeyPair; overload; static;
    class function Deserialize(aJSON : String) : TCseKeyPair; overload; static;
  end;
  
  TDelegateSerializer = class helper for TDelegate
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TDelegate; overload; static;
    class function Deserialize(aJSON : String) : TDelegate; overload; static;
  end;
  
  TDisableCseKeyPairRequestSerializer = class helper for TDisableCseKeyPairRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TDisableCseKeyPairRequest; overload; static;
    class function Deserialize(aJSON : String) : TDisableCseKeyPairRequest; overload; static;
  end;
  
  TMessagePartBodySerializer = class helper for TMessagePartBody
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TMessagePartBody; overload; static;
    class function Deserialize(aJSON : String) : TMessagePartBody; overload; static;
  end;
  
  TMessagePartHeaderSerializer = class helper for TMessagePartHeader
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TMessagePartHeader; overload; static;
    class function Deserialize(aJSON : String) : TMessagePartHeader; overload; static;
  end;
  
  TMessagePartSerializer = class helper for TMessagePart
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TMessagePart; overload; static;
    class function Deserialize(aJSON : String) : TMessagePart; overload; static;
  end;
  
  TMessageSerializer = class helper for TMessage
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TMessage; overload; static;
    class function Deserialize(aJSON : String) : TMessage; overload; static;
  end;
  
  TDraftSerializer = class helper for TDraft
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TDraft; overload; static;
    class function Deserialize(aJSON : String) : TDraft; overload; static;
  end;
  
  TEnableCseKeyPairRequestSerializer = class helper for TEnableCseKeyPairRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TEnableCseKeyPairRequest; overload; static;
    class function Deserialize(aJSON : String) : TEnableCseKeyPairRequest; overload; static;
  end;
  
  TFilterActionSerializer = class helper for TFilterAction
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TFilterAction; overload; static;
    class function Deserialize(aJSON : String) : TFilterAction; overload; static;
  end;
  
  TFilterCriteriaSerializer = class helper for TFilterCriteria
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TFilterCriteria; overload; static;
    class function Deserialize(aJSON : String) : TFilterCriteria; overload; static;
  end;
  
  TFilterSerializer = class helper for TFilter
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TFilter; overload; static;
    class function Deserialize(aJSON : String) : TFilter; overload; static;
  end;
  
  TForwardingAddressSerializer = class helper for TForwardingAddress
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TForwardingAddress; overload; static;
    class function Deserialize(aJSON : String) : TForwardingAddress; overload; static;
  end;
  
  THistoryLabelAddedSerializer = class helper for THistoryLabelAdded
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : THistoryLabelAdded; overload; static;
    class function Deserialize(aJSON : String) : THistoryLabelAdded; overload; static;
  end;
  
  THistoryLabelRemovedSerializer = class helper for THistoryLabelRemoved
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : THistoryLabelRemoved; overload; static;
    class function Deserialize(aJSON : String) : THistoryLabelRemoved; overload; static;
  end;
  
  THistoryMessageAddedSerializer = class helper for THistoryMessageAdded
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : THistoryMessageAdded; overload; static;
    class function Deserialize(aJSON : String) : THistoryMessageAdded; overload; static;
  end;
  
  THistoryMessageDeletedSerializer = class helper for THistoryMessageDeleted
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : THistoryMessageDeleted; overload; static;
    class function Deserialize(aJSON : String) : THistoryMessageDeleted; overload; static;
  end;
  
  THistorySerializer = class helper for THistory
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : THistory; overload; static;
    class function Deserialize(aJSON : String) : THistory; overload; static;
  end;
  
  TImapSettingsSerializer = class helper for TImapSettings
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TImapSettings; overload; static;
    class function Deserialize(aJSON : String) : TImapSettings; overload; static;
  end;
  
  TLabelColorSerializer = class helper for TLabelColor
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TLabelColor; overload; static;
    class function Deserialize(aJSON : String) : TLabelColor; overload; static;
  end;
  
  TLabelSerializer = class helper for TLabel
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TLabel; overload; static;
    class function Deserialize(aJSON : String) : TLabel; overload; static;
  end;
  
  TLanguageSettingsSerializer = class helper for TLanguageSettings
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TLanguageSettings; overload; static;
    class function Deserialize(aJSON : String) : TLanguageSettings; overload; static;
  end;
  
  TListCseIdentitiesResponseSerializer = class helper for TListCseIdentitiesResponse
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TListCseIdentitiesResponse; overload; static;
    class function Deserialize(aJSON : String) : TListCseIdentitiesResponse; overload; static;
  end;
  
  TListCseKeyPairsResponseSerializer = class helper for TListCseKeyPairsResponse
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TListCseKeyPairsResponse; overload; static;
    class function Deserialize(aJSON : String) : TListCseKeyPairsResponse; overload; static;
  end;
  
  TListDelegatesResponseSerializer = class helper for TListDelegatesResponse
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TListDelegatesResponse; overload; static;
    class function Deserialize(aJSON : String) : TListDelegatesResponse; overload; static;
  end;
  
  TListDraftsResponseSerializer = class helper for TListDraftsResponse
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TListDraftsResponse; overload; static;
    class function Deserialize(aJSON : String) : TListDraftsResponse; overload; static;
  end;
  
  TListFiltersResponseSerializer = class helper for TListFiltersResponse
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TListFiltersResponse; overload; static;
    class function Deserialize(aJSON : String) : TListFiltersResponse; overload; static;
  end;
  
  TListForwardingAddressesResponseSerializer = class helper for TListForwardingAddressesResponse
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TListForwardingAddressesResponse; overload; static;
    class function Deserialize(aJSON : String) : TListForwardingAddressesResponse; overload; static;
  end;
  
  TListHistoryResponseSerializer = class helper for TListHistoryResponse
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TListHistoryResponse; overload; static;
    class function Deserialize(aJSON : String) : TListHistoryResponse; overload; static;
  end;
  
  TListLabelsResponseSerializer = class helper for TListLabelsResponse
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TListLabelsResponse; overload; static;
    class function Deserialize(aJSON : String) : TListLabelsResponse; overload; static;
  end;
  
  TListMessagesResponseSerializer = class helper for TListMessagesResponse
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TListMessagesResponse; overload; static;
    class function Deserialize(aJSON : String) : TListMessagesResponse; overload; static;
  end;
  
  TSmtpMsaSerializer = class helper for TSmtpMsa
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TSmtpMsa; overload; static;
    class function Deserialize(aJSON : String) : TSmtpMsa; overload; static;
  end;
  
  TSendAsSerializer = class helper for TSendAs
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TSendAs; overload; static;
    class function Deserialize(aJSON : String) : TSendAs; overload; static;
  end;
  
  TListSendAsResponseSerializer = class helper for TListSendAsResponse
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TListSendAsResponse; overload; static;
    class function Deserialize(aJSON : String) : TListSendAsResponse; overload; static;
  end;
  
  TSmimeInfoSerializer = class helper for TSmimeInfo
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TSmimeInfo; overload; static;
    class function Deserialize(aJSON : String) : TSmimeInfo; overload; static;
  end;
  
  TListSmimeInfoResponseSerializer = class helper for TListSmimeInfoResponse
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TListSmimeInfoResponse; overload; static;
    class function Deserialize(aJSON : String) : TListSmimeInfoResponse; overload; static;
  end;
  
  TThread_Serializer = class helper for TThread_
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TThread_; overload; static;
    class function Deserialize(aJSON : String) : TThread_; overload; static;
  end;
  
  TListThreadsResponseSerializer = class helper for TListThreadsResponse
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TListThreadsResponse; overload; static;
    class function Deserialize(aJSON : String) : TListThreadsResponse; overload; static;
  end;
  
  TModifyMessageRequestSerializer = class helper for TModifyMessageRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TModifyMessageRequest; overload; static;
    class function Deserialize(aJSON : String) : TModifyMessageRequest; overload; static;
  end;
  
  TModifyThreadRequestSerializer = class helper for TModifyThreadRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TModifyThreadRequest; overload; static;
    class function Deserialize(aJSON : String) : TModifyThreadRequest; overload; static;
  end;
  
  TObliterateCseKeyPairRequestSerializer = class helper for TObliterateCseKeyPairRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TObliterateCseKeyPairRequest; overload; static;
    class function Deserialize(aJSON : String) : TObliterateCseKeyPairRequest; overload; static;
  end;
  
  TPopSettingsSerializer = class helper for TPopSettings
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TPopSettings; overload; static;
    class function Deserialize(aJSON : String) : TPopSettings; overload; static;
  end;
  
  TProfileSerializer = class helper for TProfile
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TProfile; overload; static;
    class function Deserialize(aJSON : String) : TProfile; overload; static;
  end;
  
  TVacationSettingsSerializer = class helper for TVacationSettings
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TVacationSettings; overload; static;
    class function Deserialize(aJSON : String) : TVacationSettings; overload; static;
  end;
  
  TWatchRequestSerializer = class helper for TWatchRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TWatchRequest; overload; static;
    class function Deserialize(aJSON : String) : TWatchRequest; overload; static;
  end;
  
  TWatchResponseSerializer = class helper for TWatchResponse
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TWatchResponse; overload; static;
    class function Deserialize(aJSON : String) : TWatchResponse; overload; static;
  end;
  
  TClassificationLabelFieldValueArraySerializer = type helper for TClassificationLabelFieldValueArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TClassificationLabelFieldValueArray; overload; static;
    class function Deserialize(aJSON : String) : TClassificationLabelFieldValueArray; overload; static;
  end;
  TClassificationLabelValueArraySerializer = type helper for TClassificationLabelValueArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TClassificationLabelValueArray; overload; static;
    class function Deserialize(aJSON : String) : TClassificationLabelValueArray; overload; static;
  end;
  TCseIdentityArraySerializer = type helper for TCseIdentityArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TCseIdentityArray; overload; static;
    class function Deserialize(aJSON : String) : TCseIdentityArray; overload; static;
  end;
  TCseKeyPairArraySerializer = type helper for TCseKeyPairArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TCseKeyPairArray; overload; static;
    class function Deserialize(aJSON : String) : TCseKeyPairArray; overload; static;
  end;
  TCsePrivateKeyMetadataArraySerializer = type helper for TCsePrivateKeyMetadataArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TCsePrivateKeyMetadataArray; overload; static;
    class function Deserialize(aJSON : String) : TCsePrivateKeyMetadataArray; overload; static;
  end;
  TDelegateArraySerializer = type helper for TDelegateArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TDelegateArray; overload; static;
    class function Deserialize(aJSON : String) : TDelegateArray; overload; static;
  end;
  TDraftArraySerializer = type helper for TDraftArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TDraftArray; overload; static;
    class function Deserialize(aJSON : String) : TDraftArray; overload; static;
  end;
  TFilterArraySerializer = type helper for TFilterArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TFilterArray; overload; static;
    class function Deserialize(aJSON : String) : TFilterArray; overload; static;
  end;
  TForwardingAddressArraySerializer = type helper for TForwardingAddressArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TForwardingAddressArray; overload; static;
    class function Deserialize(aJSON : String) : TForwardingAddressArray; overload; static;
  end;
  THistoryLabelAddedArraySerializer = type helper for THistoryLabelAddedArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : THistoryLabelAddedArray; overload; static;
    class function Deserialize(aJSON : String) : THistoryLabelAddedArray; overload; static;
  end;
  THistoryLabelRemovedArraySerializer = type helper for THistoryLabelRemovedArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : THistoryLabelRemovedArray; overload; static;
    class function Deserialize(aJSON : String) : THistoryLabelRemovedArray; overload; static;
  end;
  THistoryMessageAddedArraySerializer = type helper for THistoryMessageAddedArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : THistoryMessageAddedArray; overload; static;
    class function Deserialize(aJSON : String) : THistoryMessageAddedArray; overload; static;
  end;
  THistoryMessageDeletedArraySerializer = type helper for THistoryMessageDeletedArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : THistoryMessageDeletedArray; overload; static;
    class function Deserialize(aJSON : String) : THistoryMessageDeletedArray; overload; static;
  end;
  THistoryArraySerializer = type helper for THistoryArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : THistoryArray; overload; static;
    class function Deserialize(aJSON : String) : THistoryArray; overload; static;
  end;
  TLabelArraySerializer = type helper for TLabelArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TLabelArray; overload; static;
    class function Deserialize(aJSON : String) : TLabelArray; overload; static;
  end;
  TMessagePartHeaderArraySerializer = type helper for TMessagePartHeaderArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TMessagePartHeaderArray; overload; static;
    class function Deserialize(aJSON : String) : TMessagePartHeaderArray; overload; static;
  end;
  TMessagePartArraySerializer = type helper for TMessagePartArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TMessagePartArray; overload; static;
    class function Deserialize(aJSON : String) : TMessagePartArray; overload; static;
  end;
  TMessageArraySerializer = type helper for TMessageArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TMessageArray; overload; static;
    class function Deserialize(aJSON : String) : TMessageArray; overload; static;
  end;
  TSendAsArraySerializer = type helper for TSendAsArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TSendAsArray; overload; static;
    class function Deserialize(aJSON : String) : TSendAsArray; overload; static;
  end;
  TSmimeInfoArraySerializer = type helper for TSmimeInfoArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TSmimeInfoArray; overload; static;
    class function Deserialize(aJSON : String) : TSmimeInfoArray; overload; static;
  end;
  TThread_ArraySerializer = type helper for TThread_Array
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TThread_Array; overload; static;
    class function Deserialize(aJSON : String) : TThread_Array; overload; static;
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

function TAutoForwardingSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('disposition',disposition);
    Result.Add('emailAddress',emailAddress);
    Result.Add('enabled',enabled);
  except
    Result.Free;
    raise;
  end;
end;

function TAutoForwardingSerializer.Serialize : String;
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

class function TAutoForwardingSerializer.Deserialize(aJSON : TJSONObject) : TAutoForwarding;

begin
  Result := TAutoForwarding.Create;
  If (aJSON=Nil) then
    exit;
  Result.disposition:=aJSON.Get('disposition','');
  Result.emailAddress:=aJSON.Get('emailAddress','');
  Result.enabled:=aJSON.Get('enabled',False);
end;

class function TAutoForwardingSerializer.Deserialize(aJSON : String) : TAutoForwarding;

var
  lObj : TJSONObject;
begin
  Result := Default(TAutoForwarding);
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

function TBatchDeleteMessagesRequestSerializer.SerializeObject : TJSONObject;

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
  except
    Result.Free;
    raise;
  end;
end;

function TBatchDeleteMessagesRequestSerializer.Serialize : String;
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

class function TBatchDeleteMessagesRequestSerializer.Deserialize(aJSON : TJSONObject) : TBatchDeleteMessagesRequest;

var
  lArr : TJSONArray;
  i : Integer;
  lFids : TStringDynArray;
begin
  Result := TBatchDeleteMessagesRequest.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('ids',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFids,lArr.Count);
    For I:=0 to Length(lFids)-1 do
      lFids[i]:=lArr[i].Asstring;
    Result.ids:=lFids;
    end;
end;

class function TBatchDeleteMessagesRequestSerializer.Deserialize(aJSON : String) : TBatchDeleteMessagesRequest;

var
  lObj : TJSONObject;
begin
  Result := Default(TBatchDeleteMessagesRequest);
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

function TClassificationLabelFieldValueSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('fieldId',fieldId);
    Result.Add('selection',selection);
  except
    Result.Free;
    raise;
  end;
end;

function TClassificationLabelFieldValueSerializer.Serialize : String;
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

class function TClassificationLabelFieldValueSerializer.Deserialize(aJSON : TJSONObject) : TClassificationLabelFieldValue;

begin
  Result := TClassificationLabelFieldValue.Create;
  If (aJSON=Nil) then
    exit;
  Result.fieldId:=aJSON.Get('fieldId','');
  Result.selection:=aJSON.Get('selection','');
end;

class function TClassificationLabelFieldValueSerializer.Deserialize(aJSON : String) : TClassificationLabelFieldValue;

var
  lObj : TJSONObject;
begin
  Result := Default(TClassificationLabelFieldValue);
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

function TClassificationLabelValueSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('fields',Arr);
    For I:=0 to Length(fields)-1 do
      Arr.Add(fields[i].SerializeObject);
    Result.Add('labelId',labelId);
  except
    Result.Free;
    raise;
  end;
end;

function TClassificationLabelValueSerializer.Serialize : String;
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

class function TClassificationLabelValueSerializer.Deserialize(aJSON : TJSONObject) : TClassificationLabelValue;

var
  lArr : TJSONArray;
  i : Integer;
  lFfields : TClassificationLabelFieldValueArray;
begin
  Result := TClassificationLabelValue.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('fields',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFfields,lArr.Count);
    For I:=0 to Length(lFfields)-1 do
      lFfields[i]:=TClassificationLabelFieldValue.Deserialize(lArr[i] as TJSONObject);
    Result.fields:=lFfields;
    end;
  Result.labelId:=aJSON.Get('labelId','');
end;

class function TClassificationLabelValueSerializer.Deserialize(aJSON : String) : TClassificationLabelValue;

var
  lObj : TJSONObject;
begin
  Result := Default(TClassificationLabelValue);
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

function TBatchModifyMessagesRequestSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('addClassificationLabels',Arr);
    For I:=0 to Length(addClassificationLabels)-1 do
      Arr.Add(addClassificationLabels[i].SerializeObject);
    Arr:=TJSONArray.Create;
    Result.Add('addLabelIds',Arr);
    For I:=0 to Length(addLabelIds)-1 do
      Arr.Add(addLabelIds[i]);
    Arr:=TJSONArray.Create;
    Result.Add('ids',Arr);
    For I:=0 to Length(ids)-1 do
      Arr.Add(ids[i]);
    Arr:=TJSONArray.Create;
    Result.Add('removeClassificationLabelIds',Arr);
    For I:=0 to Length(removeClassificationLabelIds)-1 do
      Arr.Add(removeClassificationLabelIds[i]);
    Arr:=TJSONArray.Create;
    Result.Add('removeLabelIds',Arr);
    For I:=0 to Length(removeLabelIds)-1 do
      Arr.Add(removeLabelIds[i]);
  except
    Result.Free;
    raise;
  end;
end;

function TBatchModifyMessagesRequestSerializer.Serialize : String;
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

class function TBatchModifyMessagesRequestSerializer.Deserialize(aJSON : TJSONObject) : TBatchModifyMessagesRequest;

var
  lArr : TJSONArray;
  i : Integer;
  lFaddClassificationLabels : TClassificationLabelValueArray;
  lFaddLabelIds : TStringDynArray;
  lFids : TStringDynArray;
  lFremoveClassificationLabelIds : TStringDynArray;
  lFremoveLabelIds : TStringDynArray;
begin
  Result := TBatchModifyMessagesRequest.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('addClassificationLabels',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFaddClassificationLabels,lArr.Count);
    For I:=0 to Length(lFaddClassificationLabels)-1 do
      lFaddClassificationLabels[i]:=TClassificationLabelValue.Deserialize(lArr[i] as TJSONObject);
    Result.addClassificationLabels:=lFaddClassificationLabels;
    end;
  lArr:=aJSON.Get('addLabelIds',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFaddLabelIds,lArr.Count);
    For I:=0 to Length(lFaddLabelIds)-1 do
      lFaddLabelIds[i]:=lArr[i].Asstring;
    Result.addLabelIds:=lFaddLabelIds;
    end;
  lArr:=aJSON.Get('ids',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFids,lArr.Count);
    For I:=0 to Length(lFids)-1 do
      lFids[i]:=lArr[i].Asstring;
    Result.ids:=lFids;
    end;
  lArr:=aJSON.Get('removeClassificationLabelIds',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFremoveClassificationLabelIds,lArr.Count);
    For I:=0 to Length(lFremoveClassificationLabelIds)-1 do
      lFremoveClassificationLabelIds[i]:=lArr[i].Asstring;
    Result.removeClassificationLabelIds:=lFremoveClassificationLabelIds;
    end;
  lArr:=aJSON.Get('removeLabelIds',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFremoveLabelIds,lArr.Count);
    For I:=0 to Length(lFremoveLabelIds)-1 do
      lFremoveLabelIds[i]:=lArr[i].Asstring;
    Result.removeLabelIds:=lFremoveLabelIds;
    end;
end;

class function TBatchModifyMessagesRequestSerializer.Deserialize(aJSON : String) : TBatchModifyMessagesRequest;

var
  lObj : TJSONObject;
begin
  Result := Default(TBatchModifyMessagesRequest);
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

function TSignAndEncryptKeyPairsSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('encryptionKeyPairId',encryptionKeyPairId);
    Result.Add('signingKeyPairId',signingKeyPairId);
  except
    Result.Free;
    raise;
  end;
end;

function TSignAndEncryptKeyPairsSerializer.Serialize : String;
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

class function TSignAndEncryptKeyPairsSerializer.Deserialize(aJSON : TJSONObject) : TSignAndEncryptKeyPairs;

begin
  Result := TSignAndEncryptKeyPairs.Create;
  If (aJSON=Nil) then
    exit;
  Result.encryptionKeyPairId:=aJSON.Get('encryptionKeyPairId','');
  Result.signingKeyPairId:=aJSON.Get('signingKeyPairId','');
end;

class function TSignAndEncryptKeyPairsSerializer.Deserialize(aJSON : String) : TSignAndEncryptKeyPairs;

var
  lObj : TJSONObject;
begin
  Result := Default(TSignAndEncryptKeyPairs);
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

function TCseIdentitySerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('emailAddress',emailAddress);
    Result.Add('primaryKeyPairId',primaryKeyPairId);
    if Assigned(signAndEncryptKeyPairs) then
      Result.Add('signAndEncryptKeyPairs',signAndEncryptKeyPairs.SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TCseIdentitySerializer.Serialize : String;
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

class function TCseIdentitySerializer.Deserialize(aJSON : TJSONObject) : TCseIdentity;

begin
  Result := TCseIdentity.Create;
  If (aJSON=Nil) then
    exit;
  Result.emailAddress:=aJSON.Get('emailAddress','');
  Result.primaryKeyPairId:=aJSON.Get('primaryKeyPairId','');
  Result.signAndEncryptKeyPairs:=TSignAndEncryptKeyPairs.Deserialize(aJSON.Get('signAndEncryptKeyPairs',TJSONObject(Nil)));
end;

class function TCseIdentitySerializer.Deserialize(aJSON : String) : TCseIdentity;

var
  lObj : TJSONObject;
begin
  Result := Default(TCseIdentity);
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

function THardwareKeyMetadataSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('description',description);
  except
    Result.Free;
    raise;
  end;
end;

function THardwareKeyMetadataSerializer.Serialize : String;
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

class function THardwareKeyMetadataSerializer.Deserialize(aJSON : TJSONObject) : THardwareKeyMetadata;

begin
  Result := THardwareKeyMetadata.Create;
  If (aJSON=Nil) then
    exit;
  Result.description:=aJSON.Get('description','');
end;

class function THardwareKeyMetadataSerializer.Deserialize(aJSON : String) : THardwareKeyMetadata;

var
  lObj : TJSONObject;
begin
  Result := Default(THardwareKeyMetadata);
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

function TKaclsKeyMetadataSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('kaclsData',kaclsData);
    Result.Add('kaclsUri',kaclsUri);
  except
    Result.Free;
    raise;
  end;
end;

function TKaclsKeyMetadataSerializer.Serialize : String;
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

class function TKaclsKeyMetadataSerializer.Deserialize(aJSON : TJSONObject) : TKaclsKeyMetadata;

begin
  Result := TKaclsKeyMetadata.Create;
  If (aJSON=Nil) then
    exit;
  Result.kaclsData:=aJSON.Get('kaclsData','');
  Result.kaclsUri:=aJSON.Get('kaclsUri','');
end;

class function TKaclsKeyMetadataSerializer.Deserialize(aJSON : String) : TKaclsKeyMetadata;

var
  lObj : TJSONObject;
begin
  Result := Default(TKaclsKeyMetadata);
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

function TCsePrivateKeyMetadataSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    if Assigned(hardwareKeyMetadata) then
      Result.Add('hardwareKeyMetadata',hardwareKeyMetadata.SerializeObject);
    if Assigned(kaclsKeyMetadata) then
      Result.Add('kaclsKeyMetadata',kaclsKeyMetadata.SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TCsePrivateKeyMetadataSerializer.Serialize : String;
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

class function TCsePrivateKeyMetadataSerializer.Deserialize(aJSON : TJSONObject) : TCsePrivateKeyMetadata;

begin
  Result := TCsePrivateKeyMetadata.Create;
  If (aJSON=Nil) then
    exit;
  Result.hardwareKeyMetadata:=THardwareKeyMetadata.Deserialize(aJSON.Get('hardwareKeyMetadata',TJSONObject(Nil)));
  Result.kaclsKeyMetadata:=TKaclsKeyMetadata.Deserialize(aJSON.Get('kaclsKeyMetadata',TJSONObject(Nil)));
  Result.privateKeyMetadataId:=aJSON.Get('privateKeyMetadataId','');
end;

class function TCsePrivateKeyMetadataSerializer.Deserialize(aJSON : String) : TCsePrivateKeyMetadata;

var
  lObj : TJSONObject;
begin
  Result := Default(TCsePrivateKeyMetadata);
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

function TCseKeyPairSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('pkcs7',pkcs7);
    Arr:=TJSONArray.Create;
    Result.Add('privateKeyMetadata',Arr);
    For I:=0 to Length(privateKeyMetadata)-1 do
      Arr.Add(privateKeyMetadata[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TCseKeyPairSerializer.Serialize : String;
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

class function TCseKeyPairSerializer.Deserialize(aJSON : TJSONObject) : TCseKeyPair;

var
  lArr : TJSONArray;
  i : Integer;
  lFprivateKeyMetadata : TCsePrivateKeyMetadataArray;
  lFsubjectEmailAddresses : TStringDynArray;
begin
  Result := TCseKeyPair.Create;
  If (aJSON=Nil) then
    exit;
  Result.disableTime:=aJSON.Get('disableTime','');
  Result.enablementState:=aJSON.Get('enablementState','');
  Result.keyPairId:=aJSON.Get('keyPairId','');
  Result.pem:=aJSON.Get('pem','');
  Result.pkcs7:=aJSON.Get('pkcs7','');
  lArr:=aJSON.Get('privateKeyMetadata',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFprivateKeyMetadata,lArr.Count);
    For I:=0 to Length(lFprivateKeyMetadata)-1 do
      lFprivateKeyMetadata[i]:=TCsePrivateKeyMetadata.Deserialize(lArr[i] as TJSONObject);
    Result.privateKeyMetadata:=lFprivateKeyMetadata;
    end;
  lArr:=aJSON.Get('subjectEmailAddresses',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFsubjectEmailAddresses,lArr.Count);
    For I:=0 to Length(lFsubjectEmailAddresses)-1 do
      lFsubjectEmailAddresses[i]:=lArr[i].Asstring;
    Result.subjectEmailAddresses:=lFsubjectEmailAddresses;
    end;
end;

class function TCseKeyPairSerializer.Deserialize(aJSON : String) : TCseKeyPair;

var
  lObj : TJSONObject;
begin
  Result := Default(TCseKeyPair);
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

function TDelegateSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('delegateEmail',delegateEmail);
    Result.Add('verificationStatus',verificationStatus);
  except
    Result.Free;
    raise;
  end;
end;

function TDelegateSerializer.Serialize : String;
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

class function TDelegateSerializer.Deserialize(aJSON : TJSONObject) : TDelegate;

begin
  Result := TDelegate.Create;
  If (aJSON=Nil) then
    exit;
  Result.delegateEmail:=aJSON.Get('delegateEmail','');
  Result.verificationStatus:=aJSON.Get('verificationStatus','');
end;

class function TDelegateSerializer.Deserialize(aJSON : String) : TDelegate;

var
  lObj : TJSONObject;
begin
  Result := Default(TDelegate);
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

function TDisableCseKeyPairRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
  except
    Result.Free;
    raise;
  end;
end;

function TDisableCseKeyPairRequestSerializer.Serialize : String;
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

class function TDisableCseKeyPairRequestSerializer.Deserialize(aJSON : TJSONObject) : TDisableCseKeyPairRequest;

begin
  Result := TDisableCseKeyPairRequest.Create;
  If (aJSON=Nil) then
    exit;
end;

class function TDisableCseKeyPairRequestSerializer.Deserialize(aJSON : String) : TDisableCseKeyPairRequest;

var
  lObj : TJSONObject;
begin
  Result := Default(TDisableCseKeyPairRequest);
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

function TMessagePartBodySerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('attachmentId',attachmentId);
    Result.Add('data',data);
    Result.Add('size',size);
  except
    Result.Free;
    raise;
  end;
end;

function TMessagePartBodySerializer.Serialize : String;
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

class function TMessagePartBodySerializer.Deserialize(aJSON : TJSONObject) : TMessagePartBody;

begin
  Result := TMessagePartBody.Create;
  If (aJSON=Nil) then
    exit;
  Result.attachmentId:=aJSON.Get('attachmentId','');
  Result.data:=aJSON.Get('data','');
  Result.size:=aJSON.Get('size',0);
end;

class function TMessagePartBodySerializer.Deserialize(aJSON : String) : TMessagePartBody;

var
  lObj : TJSONObject;
begin
  Result := Default(TMessagePartBody);
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

function TMessagePartHeaderSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('name',name);
    Result.Add('value',value);
  except
    Result.Free;
    raise;
  end;
end;

function TMessagePartHeaderSerializer.Serialize : String;
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

class function TMessagePartHeaderSerializer.Deserialize(aJSON : TJSONObject) : TMessagePartHeader;

begin
  Result := TMessagePartHeader.Create;
  If (aJSON=Nil) then
    exit;
  Result.name:=aJSON.Get('name','');
  Result.value:=aJSON.Get('value','');
end;

class function TMessagePartHeaderSerializer.Deserialize(aJSON : String) : TMessagePartHeader;

var
  lObj : TJSONObject;
begin
  Result := Default(TMessagePartHeader);
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

function TMessagePartSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    if Assigned(body) then
      Result.Add('body',body.SerializeObject);
    Result.Add('filename',filename);
    Arr:=TJSONArray.Create;
    Result.Add('headers',Arr);
    For I:=0 to Length(headers)-1 do
      Arr.Add(headers[i].SerializeObject);
    Result.Add('mimeType',mimeType);
    Result.Add('partId',partId);
    Arr:=TJSONArray.Create;
    Result.Add('parts',Arr);
    For I:=0 to Length(parts)-1 do
      Arr.Add(parts[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TMessagePartSerializer.Serialize : String;
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

class function TMessagePartSerializer.Deserialize(aJSON : TJSONObject) : TMessagePart;

var
  lArr : TJSONArray;
  i : Integer;
  lFheaders : TMessagePartHeaderArray;
  lFparts : TMessagePartArray;
begin
  Result := TMessagePart.Create;
  If (aJSON=Nil) then
    exit;
  Result.body:=TMessagePartBody.Deserialize(aJSON.Get('body',TJSONObject(Nil)));
  Result.filename:=aJSON.Get('filename','');
  lArr:=aJSON.Get('headers',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFheaders,lArr.Count);
    For I:=0 to Length(lFheaders)-1 do
      lFheaders[i]:=TMessagePartHeader.Deserialize(lArr[i] as TJSONObject);
    Result.headers:=lFheaders;
    end;
  Result.mimeType:=aJSON.Get('mimeType','');
  Result.partId:=aJSON.Get('partId','');
  lArr:=aJSON.Get('parts',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFparts,lArr.Count);
    For I:=0 to Length(lFparts)-1 do
      lFparts[i]:=TMessagePart.Deserialize(lArr[i] as TJSONObject);
    Result.parts:=lFparts;
    end;
end;

class function TMessagePartSerializer.Deserialize(aJSON : String) : TMessagePart;

var
  lObj : TJSONObject;
begin
  Result := Default(TMessagePart);
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

function TMessageSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('classificationLabelValues',Arr);
    For I:=0 to Length(classificationLabelValues)-1 do
      Arr.Add(classificationLabelValues[i].SerializeObject);
    Result.Add('historyId',historyId);
    Result.Add('id',id);
    Result.Add('internalDate',internalDate);
    Arr:=TJSONArray.Create;
    Result.Add('labelIds',Arr);
    For I:=0 to Length(labelIds)-1 do
      Arr.Add(labelIds[i]);
    if Assigned(payload) then
      Result.Add('payload',payload.SerializeObject);
    Result.Add('raw',raw);
    Result.Add('sizeEstimate',sizeEstimate);
    Result.Add('snippet',snippet);
    Result.Add('threadId',threadId);
  except
    Result.Free;
    raise;
  end;
end;

function TMessageSerializer.Serialize : String;
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

class function TMessageSerializer.Deserialize(aJSON : TJSONObject) : TMessage;

var
  lArr : TJSONArray;
  i : Integer;
  lFclassificationLabelValues : TClassificationLabelValueArray;
  lFlabelIds : TStringDynArray;
begin
  Result := TMessage.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('classificationLabelValues',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFclassificationLabelValues,lArr.Count);
    For I:=0 to Length(lFclassificationLabelValues)-1 do
      lFclassificationLabelValues[i]:=TClassificationLabelValue.Deserialize(lArr[i] as TJSONObject);
    Result.classificationLabelValues:=lFclassificationLabelValues;
    end;
  Result.historyId:=aJSON.Get('historyId','');
  Result.id:=aJSON.Get('id','');
  Result.internalDate:=aJSON.Get('internalDate','');
  lArr:=aJSON.Get('labelIds',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFlabelIds,lArr.Count);
    For I:=0 to Length(lFlabelIds)-1 do
      lFlabelIds[i]:=lArr[i].Asstring;
    Result.labelIds:=lFlabelIds;
    end;
  Result.payload:=TMessagePart.Deserialize(aJSON.Get('payload',TJSONObject(Nil)));
  Result.raw:=aJSON.Get('raw','');
  Result.sizeEstimate:=aJSON.Get('sizeEstimate',0);
  Result.snippet:=aJSON.Get('snippet','');
  Result.threadId:=aJSON.Get('threadId','');
end;

class function TMessageSerializer.Deserialize(aJSON : String) : TMessage;

var
  lObj : TJSONObject;
begin
  Result := Default(TMessage);
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

function TDraftSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('id',id);
    if Assigned(message) then
      Result.Add('message',message.SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TDraftSerializer.Serialize : String;
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

class function TDraftSerializer.Deserialize(aJSON : TJSONObject) : TDraft;

begin
  Result := TDraft.Create;
  If (aJSON=Nil) then
    exit;
  Result.id:=aJSON.Get('id','');
  Result.message:=TMessage.Deserialize(aJSON.Get('message',TJSONObject(Nil)));
end;

class function TDraftSerializer.Deserialize(aJSON : String) : TDraft;

var
  lObj : TJSONObject;
begin
  Result := Default(TDraft);
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

function TEnableCseKeyPairRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
  except
    Result.Free;
    raise;
  end;
end;

function TEnableCseKeyPairRequestSerializer.Serialize : String;
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

class function TEnableCseKeyPairRequestSerializer.Deserialize(aJSON : TJSONObject) : TEnableCseKeyPairRequest;

begin
  Result := TEnableCseKeyPairRequest.Create;
  If (aJSON=Nil) then
    exit;
end;

class function TEnableCseKeyPairRequestSerializer.Deserialize(aJSON : String) : TEnableCseKeyPairRequest;

var
  lObj : TJSONObject;
begin
  Result := Default(TEnableCseKeyPairRequest);
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

function TFilterActionSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('addLabelIds',Arr);
    For I:=0 to Length(addLabelIds)-1 do
      Arr.Add(addLabelIds[i]);
    Result.Add('forward',forward);
    Arr:=TJSONArray.Create;
    Result.Add('removeLabelIds',Arr);
    For I:=0 to Length(removeLabelIds)-1 do
      Arr.Add(removeLabelIds[i]);
  except
    Result.Free;
    raise;
  end;
end;

function TFilterActionSerializer.Serialize : String;
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

class function TFilterActionSerializer.Deserialize(aJSON : TJSONObject) : TFilterAction;

var
  lArr : TJSONArray;
  i : Integer;
  lFaddLabelIds : TStringDynArray;
  lFremoveLabelIds : TStringDynArray;
begin
  Result := TFilterAction.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('addLabelIds',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFaddLabelIds,lArr.Count);
    For I:=0 to Length(lFaddLabelIds)-1 do
      lFaddLabelIds[i]:=lArr[i].Asstring;
    Result.addLabelIds:=lFaddLabelIds;
    end;
  Result.forward:=aJSON.Get('forward','');
  lArr:=aJSON.Get('removeLabelIds',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFremoveLabelIds,lArr.Count);
    For I:=0 to Length(lFremoveLabelIds)-1 do
      lFremoveLabelIds[i]:=lArr[i].Asstring;
    Result.removeLabelIds:=lFremoveLabelIds;
    end;
end;

class function TFilterActionSerializer.Deserialize(aJSON : String) : TFilterAction;

var
  lObj : TJSONObject;
begin
  Result := Default(TFilterAction);
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

function TFilterCriteriaSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('excludeChats',excludeChats);
    Result.Add('from',from);
    Result.Add('hasAttachment',hasAttachment);
    Result.Add('negatedQuery',negatedQuery);
    Result.Add('query',query);
    Result.Add('size',size);
    Result.Add('sizeComparison',sizeComparison);
    Result.Add('subject',subject);
    Result.Add('to',to_);
  except
    Result.Free;
    raise;
  end;
end;

function TFilterCriteriaSerializer.Serialize : String;
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

class function TFilterCriteriaSerializer.Deserialize(aJSON : TJSONObject) : TFilterCriteria;

begin
  Result := TFilterCriteria.Create;
  If (aJSON=Nil) then
    exit;
  Result.excludeChats:=aJSON.Get('excludeChats',False);
  Result.from:=aJSON.Get('from','');
  Result.hasAttachment:=aJSON.Get('hasAttachment',False);
  Result.negatedQuery:=aJSON.Get('negatedQuery','');
  Result.query:=aJSON.Get('query','');
  Result.size:=aJSON.Get('size',0);
  Result.sizeComparison:=aJSON.Get('sizeComparison','');
  Result.subject:=aJSON.Get('subject','');
  Result.to_:=aJSON.Get('to','');
end;

class function TFilterCriteriaSerializer.Deserialize(aJSON : String) : TFilterCriteria;

var
  lObj : TJSONObject;
begin
  Result := Default(TFilterCriteria);
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

function TFilterSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    if Assigned(action) then
      Result.Add('action',action.SerializeObject);
    if Assigned(criteria) then
      Result.Add('criteria',criteria.SerializeObject);
    Result.Add('id',id);
  except
    Result.Free;
    raise;
  end;
end;

function TFilterSerializer.Serialize : String;
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

class function TFilterSerializer.Deserialize(aJSON : TJSONObject) : TFilter;

begin
  Result := TFilter.Create;
  If (aJSON=Nil) then
    exit;
  Result.action:=TFilterAction.Deserialize(aJSON.Get('action',TJSONObject(Nil)));
  Result.criteria:=TFilterCriteria.Deserialize(aJSON.Get('criteria',TJSONObject(Nil)));
  Result.id:=aJSON.Get('id','');
end;

class function TFilterSerializer.Deserialize(aJSON : String) : TFilter;

var
  lObj : TJSONObject;
begin
  Result := Default(TFilter);
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

function TForwardingAddressSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('forwardingEmail',forwardingEmail);
    Result.Add('verificationStatus',verificationStatus);
  except
    Result.Free;
    raise;
  end;
end;

function TForwardingAddressSerializer.Serialize : String;
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

class function TForwardingAddressSerializer.Deserialize(aJSON : TJSONObject) : TForwardingAddress;

begin
  Result := TForwardingAddress.Create;
  If (aJSON=Nil) then
    exit;
  Result.forwardingEmail:=aJSON.Get('forwardingEmail','');
  Result.verificationStatus:=aJSON.Get('verificationStatus','');
end;

class function TForwardingAddressSerializer.Deserialize(aJSON : String) : TForwardingAddress;

var
  lObj : TJSONObject;
begin
  Result := Default(TForwardingAddress);
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

function THistoryLabelAddedSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('labelIds',Arr);
    For I:=0 to Length(labelIds)-1 do
      Arr.Add(labelIds[i]);
    if Assigned(message) then
      Result.Add('message',message.SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function THistoryLabelAddedSerializer.Serialize : String;
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

class function THistoryLabelAddedSerializer.Deserialize(aJSON : TJSONObject) : THistoryLabelAdded;

var
  lArr : TJSONArray;
  i : Integer;
  lFlabelIds : TStringDynArray;
begin
  Result := THistoryLabelAdded.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('labelIds',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFlabelIds,lArr.Count);
    For I:=0 to Length(lFlabelIds)-1 do
      lFlabelIds[i]:=lArr[i].Asstring;
    Result.labelIds:=lFlabelIds;
    end;
  Result.message:=TMessage.Deserialize(aJSON.Get('message',TJSONObject(Nil)));
end;

class function THistoryLabelAddedSerializer.Deserialize(aJSON : String) : THistoryLabelAdded;

var
  lObj : TJSONObject;
begin
  Result := Default(THistoryLabelAdded);
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

function THistoryLabelRemovedSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('labelIds',Arr);
    For I:=0 to Length(labelIds)-1 do
      Arr.Add(labelIds[i]);
    if Assigned(message) then
      Result.Add('message',message.SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function THistoryLabelRemovedSerializer.Serialize : String;
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

class function THistoryLabelRemovedSerializer.Deserialize(aJSON : TJSONObject) : THistoryLabelRemoved;

var
  lArr : TJSONArray;
  i : Integer;
  lFlabelIds : TStringDynArray;
begin
  Result := THistoryLabelRemoved.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('labelIds',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFlabelIds,lArr.Count);
    For I:=0 to Length(lFlabelIds)-1 do
      lFlabelIds[i]:=lArr[i].Asstring;
    Result.labelIds:=lFlabelIds;
    end;
  Result.message:=TMessage.Deserialize(aJSON.Get('message',TJSONObject(Nil)));
end;

class function THistoryLabelRemovedSerializer.Deserialize(aJSON : String) : THistoryLabelRemoved;

var
  lObj : TJSONObject;
begin
  Result := Default(THistoryLabelRemoved);
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

function THistoryMessageAddedSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    if Assigned(message) then
      Result.Add('message',message.SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function THistoryMessageAddedSerializer.Serialize : String;
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

class function THistoryMessageAddedSerializer.Deserialize(aJSON : TJSONObject) : THistoryMessageAdded;

begin
  Result := THistoryMessageAdded.Create;
  If (aJSON=Nil) then
    exit;
  Result.message:=TMessage.Deserialize(aJSON.Get('message',TJSONObject(Nil)));
end;

class function THistoryMessageAddedSerializer.Deserialize(aJSON : String) : THistoryMessageAdded;

var
  lObj : TJSONObject;
begin
  Result := Default(THistoryMessageAdded);
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

function THistoryMessageDeletedSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    if Assigned(message) then
      Result.Add('message',message.SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function THistoryMessageDeletedSerializer.Serialize : String;
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

class function THistoryMessageDeletedSerializer.Deserialize(aJSON : TJSONObject) : THistoryMessageDeleted;

begin
  Result := THistoryMessageDeleted.Create;
  If (aJSON=Nil) then
    exit;
  Result.message:=TMessage.Deserialize(aJSON.Get('message',TJSONObject(Nil)));
end;

class function THistoryMessageDeletedSerializer.Deserialize(aJSON : String) : THistoryMessageDeleted;

var
  lObj : TJSONObject;
begin
  Result := Default(THistoryMessageDeleted);
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

function THistorySerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('id',id);
    Arr:=TJSONArray.Create;
    Result.Add('labelsAdded',Arr);
    For I:=0 to Length(labelsAdded)-1 do
      Arr.Add(labelsAdded[i].SerializeObject);
    Arr:=TJSONArray.Create;
    Result.Add('labelsRemoved',Arr);
    For I:=0 to Length(labelsRemoved)-1 do
      Arr.Add(labelsRemoved[i].SerializeObject);
    Arr:=TJSONArray.Create;
    Result.Add('messages',Arr);
    For I:=0 to Length(messages)-1 do
      Arr.Add(messages[i].SerializeObject);
    Arr:=TJSONArray.Create;
    Result.Add('messagesAdded',Arr);
    For I:=0 to Length(messagesAdded)-1 do
      Arr.Add(messagesAdded[i].SerializeObject);
    Arr:=TJSONArray.Create;
    Result.Add('messagesDeleted',Arr);
    For I:=0 to Length(messagesDeleted)-1 do
      Arr.Add(messagesDeleted[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function THistorySerializer.Serialize : String;
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

class function THistorySerializer.Deserialize(aJSON : TJSONObject) : THistory;

var
  lArr : TJSONArray;
  i : Integer;
  lFlabelsAdded : THistoryLabelAddedArray;
  lFlabelsRemoved : THistoryLabelRemovedArray;
  lFmessages : TMessageArray;
  lFmessagesAdded : THistoryMessageAddedArray;
  lFmessagesDeleted : THistoryMessageDeletedArray;
begin
  Result := THistory.Create;
  If (aJSON=Nil) then
    exit;
  Result.id:=aJSON.Get('id','');
  lArr:=aJSON.Get('labelsAdded',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFlabelsAdded,lArr.Count);
    For I:=0 to Length(lFlabelsAdded)-1 do
      lFlabelsAdded[i]:=THistoryLabelAdded.Deserialize(lArr[i] as TJSONObject);
    Result.labelsAdded:=lFlabelsAdded;
    end;
  lArr:=aJSON.Get('labelsRemoved',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFlabelsRemoved,lArr.Count);
    For I:=0 to Length(lFlabelsRemoved)-1 do
      lFlabelsRemoved[i]:=THistoryLabelRemoved.Deserialize(lArr[i] as TJSONObject);
    Result.labelsRemoved:=lFlabelsRemoved;
    end;
  lArr:=aJSON.Get('messages',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFmessages,lArr.Count);
    For I:=0 to Length(lFmessages)-1 do
      lFmessages[i]:=TMessage.Deserialize(lArr[i] as TJSONObject);
    Result.messages:=lFmessages;
    end;
  lArr:=aJSON.Get('messagesAdded',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFmessagesAdded,lArr.Count);
    For I:=0 to Length(lFmessagesAdded)-1 do
      lFmessagesAdded[i]:=THistoryMessageAdded.Deserialize(lArr[i] as TJSONObject);
    Result.messagesAdded:=lFmessagesAdded;
    end;
  lArr:=aJSON.Get('messagesDeleted',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFmessagesDeleted,lArr.Count);
    For I:=0 to Length(lFmessagesDeleted)-1 do
      lFmessagesDeleted[i]:=THistoryMessageDeleted.Deserialize(lArr[i] as TJSONObject);
    Result.messagesDeleted:=lFmessagesDeleted;
    end;
end;

class function THistorySerializer.Deserialize(aJSON : String) : THistory;

var
  lObj : TJSONObject;
begin
  Result := Default(THistory);
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

function TImapSettingsSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('autoExpunge',autoExpunge);
    Result.Add('enabled',enabled);
    Result.Add('expungeBehavior',expungeBehavior);
    Result.Add('maxFolderSize',maxFolderSize);
  except
    Result.Free;
    raise;
  end;
end;

function TImapSettingsSerializer.Serialize : String;
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

class function TImapSettingsSerializer.Deserialize(aJSON : TJSONObject) : TImapSettings;

begin
  Result := TImapSettings.Create;
  If (aJSON=Nil) then
    exit;
  Result.autoExpunge:=aJSON.Get('autoExpunge',False);
  Result.enabled:=aJSON.Get('enabled',False);
  Result.expungeBehavior:=aJSON.Get('expungeBehavior','');
  Result.maxFolderSize:=aJSON.Get('maxFolderSize',0);
end;

class function TImapSettingsSerializer.Deserialize(aJSON : String) : TImapSettings;

var
  lObj : TJSONObject;
begin
  Result := Default(TImapSettings);
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

function TLabelColorSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('backgroundColor',backgroundColor);
    Result.Add('textColor',textColor);
  except
    Result.Free;
    raise;
  end;
end;

function TLabelColorSerializer.Serialize : String;
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

class function TLabelColorSerializer.Deserialize(aJSON : TJSONObject) : TLabelColor;

begin
  Result := TLabelColor.Create;
  If (aJSON=Nil) then
    exit;
  Result.backgroundColor:=aJSON.Get('backgroundColor','');
  Result.textColor:=aJSON.Get('textColor','');
end;

class function TLabelColorSerializer.Deserialize(aJSON : String) : TLabelColor;

var
  lObj : TJSONObject;
begin
  Result := Default(TLabelColor);
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
    if Assigned(color) then
      Result.Add('color',color.SerializeObject);
    Result.Add('id',id);
    Result.Add('labelListVisibility',labelListVisibility);
    Result.Add('messageListVisibility',messageListVisibility);
    Result.Add('messagesTotal',messagesTotal);
    Result.Add('messagesUnread',messagesUnread);
    Result.Add('name',name);
    Result.Add('threadsTotal',threadsTotal);
    Result.Add('threadsUnread',threadsUnread);
    Result.Add('type',type_);
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
  Result.color:=TLabelColor.Deserialize(aJSON.Get('color',TJSONObject(Nil)));
  Result.id:=aJSON.Get('id','');
  Result.labelListVisibility:=aJSON.Get('labelListVisibility','');
  Result.messageListVisibility:=aJSON.Get('messageListVisibility','');
  Result.messagesTotal:=aJSON.Get('messagesTotal',0);
  Result.messagesUnread:=aJSON.Get('messagesUnread',0);
  Result.name:=aJSON.Get('name','');
  Result.threadsTotal:=aJSON.Get('threadsTotal',0);
  Result.threadsUnread:=aJSON.Get('threadsUnread',0);
  Result.type_:=aJSON.Get('type','');
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

function TLanguageSettingsSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('displayLanguage',displayLanguage);
  except
    Result.Free;
    raise;
  end;
end;

function TLanguageSettingsSerializer.Serialize : String;
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

class function TLanguageSettingsSerializer.Deserialize(aJSON : TJSONObject) : TLanguageSettings;

begin
  Result := TLanguageSettings.Create;
  If (aJSON=Nil) then
    exit;
  Result.displayLanguage:=aJSON.Get('displayLanguage','');
end;

class function TLanguageSettingsSerializer.Deserialize(aJSON : String) : TLanguageSettings;

var
  lObj : TJSONObject;
begin
  Result := Default(TLanguageSettings);
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

function TListCseIdentitiesResponseSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('cseIdentities',Arr);
    For I:=0 to Length(cseIdentities)-1 do
      Arr.Add(cseIdentities[i].SerializeObject);
    Result.Add('nextPageToken',nextPageToken);
  except
    Result.Free;
    raise;
  end;
end;

function TListCseIdentitiesResponseSerializer.Serialize : String;
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

class function TListCseIdentitiesResponseSerializer.Deserialize(aJSON : TJSONObject) : TListCseIdentitiesResponse;

var
  lArr : TJSONArray;
  i : Integer;
  lFcseIdentities : TCseIdentityArray;
begin
  Result := TListCseIdentitiesResponse.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('cseIdentities',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFcseIdentities,lArr.Count);
    For I:=0 to Length(lFcseIdentities)-1 do
      lFcseIdentities[i]:=TCseIdentity.Deserialize(lArr[i] as TJSONObject);
    Result.cseIdentities:=lFcseIdentities;
    end;
  Result.nextPageToken:=aJSON.Get('nextPageToken','');
end;

class function TListCseIdentitiesResponseSerializer.Deserialize(aJSON : String) : TListCseIdentitiesResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TListCseIdentitiesResponse);
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

function TListCseKeyPairsResponseSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('cseKeyPairs',Arr);
    For I:=0 to Length(cseKeyPairs)-1 do
      Arr.Add(cseKeyPairs[i].SerializeObject);
    Result.Add('nextPageToken',nextPageToken);
  except
    Result.Free;
    raise;
  end;
end;

function TListCseKeyPairsResponseSerializer.Serialize : String;
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

class function TListCseKeyPairsResponseSerializer.Deserialize(aJSON : TJSONObject) : TListCseKeyPairsResponse;

var
  lArr : TJSONArray;
  i : Integer;
  lFcseKeyPairs : TCseKeyPairArray;
begin
  Result := TListCseKeyPairsResponse.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('cseKeyPairs',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFcseKeyPairs,lArr.Count);
    For I:=0 to Length(lFcseKeyPairs)-1 do
      lFcseKeyPairs[i]:=TCseKeyPair.Deserialize(lArr[i] as TJSONObject);
    Result.cseKeyPairs:=lFcseKeyPairs;
    end;
  Result.nextPageToken:=aJSON.Get('nextPageToken','');
end;

class function TListCseKeyPairsResponseSerializer.Deserialize(aJSON : String) : TListCseKeyPairsResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TListCseKeyPairsResponse);
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

function TListDelegatesResponseSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('delegates',Arr);
    For I:=0 to Length(delegates)-1 do
      Arr.Add(delegates[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TListDelegatesResponseSerializer.Serialize : String;
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

class function TListDelegatesResponseSerializer.Deserialize(aJSON : TJSONObject) : TListDelegatesResponse;

var
  lArr : TJSONArray;
  i : Integer;
  lFdelegates : TDelegateArray;
begin
  Result := TListDelegatesResponse.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('delegates',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFdelegates,lArr.Count);
    For I:=0 to Length(lFdelegates)-1 do
      lFdelegates[i]:=TDelegate.Deserialize(lArr[i] as TJSONObject);
    Result.delegates:=lFdelegates;
    end;
end;

class function TListDelegatesResponseSerializer.Deserialize(aJSON : String) : TListDelegatesResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TListDelegatesResponse);
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

function TListDraftsResponseSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('drafts',Arr);
    For I:=0 to Length(drafts)-1 do
      Arr.Add(drafts[i].SerializeObject);
    Result.Add('nextPageToken',nextPageToken);
    Result.Add('resultSizeEstimate',resultSizeEstimate);
  except
    Result.Free;
    raise;
  end;
end;

function TListDraftsResponseSerializer.Serialize : String;
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

class function TListDraftsResponseSerializer.Deserialize(aJSON : TJSONObject) : TListDraftsResponse;

var
  lArr : TJSONArray;
  i : Integer;
  lFdrafts : TDraftArray;
begin
  Result := TListDraftsResponse.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('drafts',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFdrafts,lArr.Count);
    For I:=0 to Length(lFdrafts)-1 do
      lFdrafts[i]:=TDraft.Deserialize(lArr[i] as TJSONObject);
    Result.drafts:=lFdrafts;
    end;
  Result.nextPageToken:=aJSON.Get('nextPageToken','');
  Result.resultSizeEstimate:=aJSON.Get('resultSizeEstimate',0);
end;

class function TListDraftsResponseSerializer.Deserialize(aJSON : String) : TListDraftsResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TListDraftsResponse);
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

function TListFiltersResponseSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('filter',Arr);
    For I:=0 to Length(filter)-1 do
      Arr.Add(filter[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TListFiltersResponseSerializer.Serialize : String;
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

class function TListFiltersResponseSerializer.Deserialize(aJSON : TJSONObject) : TListFiltersResponse;

var
  lArr : TJSONArray;
  i : Integer;
  lFfilter : TFilterArray;
begin
  Result := TListFiltersResponse.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('filter',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFfilter,lArr.Count);
    For I:=0 to Length(lFfilter)-1 do
      lFfilter[i]:=TFilter.Deserialize(lArr[i] as TJSONObject);
    Result.filter:=lFfilter;
    end;
end;

class function TListFiltersResponseSerializer.Deserialize(aJSON : String) : TListFiltersResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TListFiltersResponse);
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

function TListForwardingAddressesResponseSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('forwardingAddresses',Arr);
    For I:=0 to Length(forwardingAddresses)-1 do
      Arr.Add(forwardingAddresses[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TListForwardingAddressesResponseSerializer.Serialize : String;
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

class function TListForwardingAddressesResponseSerializer.Deserialize(aJSON : TJSONObject) : TListForwardingAddressesResponse;

var
  lArr : TJSONArray;
  i : Integer;
  lFforwardingAddresses : TForwardingAddressArray;
begin
  Result := TListForwardingAddressesResponse.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('forwardingAddresses',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFforwardingAddresses,lArr.Count);
    For I:=0 to Length(lFforwardingAddresses)-1 do
      lFforwardingAddresses[i]:=TForwardingAddress.Deserialize(lArr[i] as TJSONObject);
    Result.forwardingAddresses:=lFforwardingAddresses;
    end;
end;

class function TListForwardingAddressesResponseSerializer.Deserialize(aJSON : String) : TListForwardingAddressesResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TListForwardingAddressesResponse);
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

function TListHistoryResponseSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('history',Arr);
    For I:=0 to Length(history)-1 do
      Arr.Add(history[i].SerializeObject);
    Result.Add('historyId',historyId);
    Result.Add('nextPageToken',nextPageToken);
  except
    Result.Free;
    raise;
  end;
end;

function TListHistoryResponseSerializer.Serialize : String;
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

class function TListHistoryResponseSerializer.Deserialize(aJSON : TJSONObject) : TListHistoryResponse;

var
  lArr : TJSONArray;
  i : Integer;
  lFhistory : THistoryArray;
begin
  Result := TListHistoryResponse.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('history',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFhistory,lArr.Count);
    For I:=0 to Length(lFhistory)-1 do
      lFhistory[i]:=THistory.Deserialize(lArr[i] as TJSONObject);
    Result.history:=lFhistory;
    end;
  Result.historyId:=aJSON.Get('historyId','');
  Result.nextPageToken:=aJSON.Get('nextPageToken','');
end;

class function TListHistoryResponseSerializer.Deserialize(aJSON : String) : TListHistoryResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TListHistoryResponse);
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

function TListLabelsResponseSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('labels',Arr);
    For I:=0 to Length(labels)-1 do
      Arr.Add(labels[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TListLabelsResponseSerializer.Serialize : String;
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

class function TListLabelsResponseSerializer.Deserialize(aJSON : TJSONObject) : TListLabelsResponse;

var
  lArr : TJSONArray;
  i : Integer;
  lFlabels : TLabelArray;
begin
  Result := TListLabelsResponse.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('labels',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFlabels,lArr.Count);
    For I:=0 to Length(lFlabels)-1 do
      lFlabels[i]:=TLabel.Deserialize(lArr[i] as TJSONObject);
    Result.labels:=lFlabels;
    end;
end;

class function TListLabelsResponseSerializer.Deserialize(aJSON : String) : TListLabelsResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TListLabelsResponse);
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

function TListMessagesResponseSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('messages',Arr);
    For I:=0 to Length(messages)-1 do
      Arr.Add(messages[i].SerializeObject);
    Result.Add('nextPageToken',nextPageToken);
    Result.Add('resultSizeEstimate',resultSizeEstimate);
  except
    Result.Free;
    raise;
  end;
end;

function TListMessagesResponseSerializer.Serialize : String;
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

class function TListMessagesResponseSerializer.Deserialize(aJSON : TJSONObject) : TListMessagesResponse;

var
  lArr : TJSONArray;
  i : Integer;
  lFmessages : TMessageArray;
begin
  Result := TListMessagesResponse.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('messages',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFmessages,lArr.Count);
    For I:=0 to Length(lFmessages)-1 do
      lFmessages[i]:=TMessage.Deserialize(lArr[i] as TJSONObject);
    Result.messages:=lFmessages;
    end;
  Result.nextPageToken:=aJSON.Get('nextPageToken','');
  Result.resultSizeEstimate:=aJSON.Get('resultSizeEstimate',0);
end;

class function TListMessagesResponseSerializer.Deserialize(aJSON : String) : TListMessagesResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TListMessagesResponse);
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

function TSmtpMsaSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('host',host);
    Result.Add('password',password);
    Result.Add('port',port);
    Result.Add('securityMode',securityMode);
    Result.Add('username',username);
  except
    Result.Free;
    raise;
  end;
end;

function TSmtpMsaSerializer.Serialize : String;
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

class function TSmtpMsaSerializer.Deserialize(aJSON : TJSONObject) : TSmtpMsa;

begin
  Result := TSmtpMsa.Create;
  If (aJSON=Nil) then
    exit;
  Result.host:=aJSON.Get('host','');
  Result.password:=aJSON.Get('password','');
  Result.port:=aJSON.Get('port',0);
  Result.securityMode:=aJSON.Get('securityMode','');
  Result.username:=aJSON.Get('username','');
end;

class function TSmtpMsaSerializer.Deserialize(aJSON : String) : TSmtpMsa;

var
  lObj : TJSONObject;
begin
  Result := Default(TSmtpMsa);
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

function TSendAsSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('displayName',displayName);
    Result.Add('isDefault',isDefault);
    Result.Add('isPrimary',isPrimary);
    Result.Add('replyToAddress',replyToAddress);
    Result.Add('sendAsEmail',sendAsEmail);
    Result.Add('signature',signature);
    if Assigned(smtpMsa) then
      Result.Add('smtpMsa',smtpMsa.SerializeObject);
    Result.Add('treatAsAlias',treatAsAlias);
    Result.Add('verificationStatus',verificationStatus);
  except
    Result.Free;
    raise;
  end;
end;

function TSendAsSerializer.Serialize : String;
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

class function TSendAsSerializer.Deserialize(aJSON : TJSONObject) : TSendAs;

begin
  Result := TSendAs.Create;
  If (aJSON=Nil) then
    exit;
  Result.displayName:=aJSON.Get('displayName','');
  Result.isDefault:=aJSON.Get('isDefault',False);
  Result.isPrimary:=aJSON.Get('isPrimary',False);
  Result.replyToAddress:=aJSON.Get('replyToAddress','');
  Result.sendAsEmail:=aJSON.Get('sendAsEmail','');
  Result.signature:=aJSON.Get('signature','');
  Result.smtpMsa:=TSmtpMsa.Deserialize(aJSON.Get('smtpMsa',TJSONObject(Nil)));
  Result.treatAsAlias:=aJSON.Get('treatAsAlias',False);
  Result.verificationStatus:=aJSON.Get('verificationStatus','');
end;

class function TSendAsSerializer.Deserialize(aJSON : String) : TSendAs;

var
  lObj : TJSONObject;
begin
  Result := Default(TSendAs);
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

function TListSendAsResponseSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('sendAs',Arr);
    For I:=0 to Length(sendAs)-1 do
      Arr.Add(sendAs[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TListSendAsResponseSerializer.Serialize : String;
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

class function TListSendAsResponseSerializer.Deserialize(aJSON : TJSONObject) : TListSendAsResponse;

var
  lArr : TJSONArray;
  i : Integer;
  lFsendAs : TSendAsArray;
begin
  Result := TListSendAsResponse.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('sendAs',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFsendAs,lArr.Count);
    For I:=0 to Length(lFsendAs)-1 do
      lFsendAs[i]:=TSendAs.Deserialize(lArr[i] as TJSONObject);
    Result.sendAs:=lFsendAs;
    end;
end;

class function TListSendAsResponseSerializer.Deserialize(aJSON : String) : TListSendAsResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TListSendAsResponse);
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

function TSmimeInfoSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('encryptedKeyPassword',encryptedKeyPassword);
    Result.Add('expiration',expiration);
    Result.Add('id',id);
    Result.Add('isDefault',isDefault);
    Result.Add('issuerCn',issuerCn);
    Result.Add('pem',pem);
    Result.Add('pkcs12',pkcs12);
  except
    Result.Free;
    raise;
  end;
end;

function TSmimeInfoSerializer.Serialize : String;
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

class function TSmimeInfoSerializer.Deserialize(aJSON : TJSONObject) : TSmimeInfo;

begin
  Result := TSmimeInfo.Create;
  If (aJSON=Nil) then
    exit;
  Result.encryptedKeyPassword:=aJSON.Get('encryptedKeyPassword','');
  Result.expiration:=aJSON.Get('expiration','');
  Result.id:=aJSON.Get('id','');
  Result.isDefault:=aJSON.Get('isDefault',False);
  Result.issuerCn:=aJSON.Get('issuerCn','');
  Result.pem:=aJSON.Get('pem','');
  Result.pkcs12:=aJSON.Get('pkcs12','');
end;

class function TSmimeInfoSerializer.Deserialize(aJSON : String) : TSmimeInfo;

var
  lObj : TJSONObject;
begin
  Result := Default(TSmimeInfo);
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

function TListSmimeInfoResponseSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('smimeInfo',Arr);
    For I:=0 to Length(smimeInfo)-1 do
      Arr.Add(smimeInfo[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TListSmimeInfoResponseSerializer.Serialize : String;
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

class function TListSmimeInfoResponseSerializer.Deserialize(aJSON : TJSONObject) : TListSmimeInfoResponse;

var
  lArr : TJSONArray;
  i : Integer;
  lFsmimeInfo : TSmimeInfoArray;
begin
  Result := TListSmimeInfoResponse.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('smimeInfo',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFsmimeInfo,lArr.Count);
    For I:=0 to Length(lFsmimeInfo)-1 do
      lFsmimeInfo[i]:=TSmimeInfo.Deserialize(lArr[i] as TJSONObject);
    Result.smimeInfo:=lFsmimeInfo;
    end;
end;

class function TListSmimeInfoResponseSerializer.Deserialize(aJSON : String) : TListSmimeInfoResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TListSmimeInfoResponse);
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

function TThread_Serializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('historyId',historyId);
    Result.Add('id',id);
    Arr:=TJSONArray.Create;
    Result.Add('messages',Arr);
    For I:=0 to Length(messages)-1 do
      Arr.Add(messages[i].SerializeObject);
    Result.Add('snippet',snippet);
  except
    Result.Free;
    raise;
  end;
end;

function TThread_Serializer.Serialize : String;
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

class function TThread_Serializer.Deserialize(aJSON : TJSONObject) : TThread_;

var
  lArr : TJSONArray;
  i : Integer;
  lFmessages : TMessageArray;
begin
  Result := TThread_.Create;
  If (aJSON=Nil) then
    exit;
  Result.historyId:=aJSON.Get('historyId','');
  Result.id:=aJSON.Get('id','');
  lArr:=aJSON.Get('messages',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFmessages,lArr.Count);
    For I:=0 to Length(lFmessages)-1 do
      lFmessages[i]:=TMessage.Deserialize(lArr[i] as TJSONObject);
    Result.messages:=lFmessages;
    end;
  Result.snippet:=aJSON.Get('snippet','');
end;

class function TThread_Serializer.Deserialize(aJSON : String) : TThread_;

var
  lObj : TJSONObject;
begin
  Result := Default(TThread_);
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

function TListThreadsResponseSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('nextPageToken',nextPageToken);
    Result.Add('resultSizeEstimate',resultSizeEstimate);
    Arr:=TJSONArray.Create;
    Result.Add('threads',Arr);
    For I:=0 to Length(threads)-1 do
      Arr.Add(threads[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TListThreadsResponseSerializer.Serialize : String;
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

class function TListThreadsResponseSerializer.Deserialize(aJSON : TJSONObject) : TListThreadsResponse;

var
  lArr : TJSONArray;
  i : Integer;
  lFthreads : TThread_Array;
begin
  Result := TListThreadsResponse.Create;
  If (aJSON=Nil) then
    exit;
  Result.nextPageToken:=aJSON.Get('nextPageToken','');
  Result.resultSizeEstimate:=aJSON.Get('resultSizeEstimate',0);
  lArr:=aJSON.Get('threads',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFthreads,lArr.Count);
    For I:=0 to Length(lFthreads)-1 do
      lFthreads[i]:=TThread_.Deserialize(lArr[i] as TJSONObject);
    Result.threads:=lFthreads;
    end;
end;

class function TListThreadsResponseSerializer.Deserialize(aJSON : String) : TListThreadsResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TListThreadsResponse);
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

function TModifyMessageRequestSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('addClassificationLabels',Arr);
    For I:=0 to Length(addClassificationLabels)-1 do
      Arr.Add(addClassificationLabels[i].SerializeObject);
    Arr:=TJSONArray.Create;
    Result.Add('addLabelIds',Arr);
    For I:=0 to Length(addLabelIds)-1 do
      Arr.Add(addLabelIds[i]);
    Arr:=TJSONArray.Create;
    Result.Add('removeClassificationLabelIds',Arr);
    For I:=0 to Length(removeClassificationLabelIds)-1 do
      Arr.Add(removeClassificationLabelIds[i]);
    Arr:=TJSONArray.Create;
    Result.Add('removeLabelIds',Arr);
    For I:=0 to Length(removeLabelIds)-1 do
      Arr.Add(removeLabelIds[i]);
  except
    Result.Free;
    raise;
  end;
end;

function TModifyMessageRequestSerializer.Serialize : String;
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

class function TModifyMessageRequestSerializer.Deserialize(aJSON : TJSONObject) : TModifyMessageRequest;

var
  lArr : TJSONArray;
  i : Integer;
  lFaddClassificationLabels : TClassificationLabelValueArray;
  lFaddLabelIds : TStringDynArray;
  lFremoveClassificationLabelIds : TStringDynArray;
  lFremoveLabelIds : TStringDynArray;
begin
  Result := TModifyMessageRequest.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('addClassificationLabels',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFaddClassificationLabels,lArr.Count);
    For I:=0 to Length(lFaddClassificationLabels)-1 do
      lFaddClassificationLabels[i]:=TClassificationLabelValue.Deserialize(lArr[i] as TJSONObject);
    Result.addClassificationLabels:=lFaddClassificationLabels;
    end;
  lArr:=aJSON.Get('addLabelIds',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFaddLabelIds,lArr.Count);
    For I:=0 to Length(lFaddLabelIds)-1 do
      lFaddLabelIds[i]:=lArr[i].Asstring;
    Result.addLabelIds:=lFaddLabelIds;
    end;
  lArr:=aJSON.Get('removeClassificationLabelIds',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFremoveClassificationLabelIds,lArr.Count);
    For I:=0 to Length(lFremoveClassificationLabelIds)-1 do
      lFremoveClassificationLabelIds[i]:=lArr[i].Asstring;
    Result.removeClassificationLabelIds:=lFremoveClassificationLabelIds;
    end;
  lArr:=aJSON.Get('removeLabelIds',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFremoveLabelIds,lArr.Count);
    For I:=0 to Length(lFremoveLabelIds)-1 do
      lFremoveLabelIds[i]:=lArr[i].Asstring;
    Result.removeLabelIds:=lFremoveLabelIds;
    end;
end;

class function TModifyMessageRequestSerializer.Deserialize(aJSON : String) : TModifyMessageRequest;

var
  lObj : TJSONObject;
begin
  Result := Default(TModifyMessageRequest);
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

function TModifyThreadRequestSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('addLabelIds',Arr);
    For I:=0 to Length(addLabelIds)-1 do
      Arr.Add(addLabelIds[i]);
    Arr:=TJSONArray.Create;
    Result.Add('removeLabelIds',Arr);
    For I:=0 to Length(removeLabelIds)-1 do
      Arr.Add(removeLabelIds[i]);
  except
    Result.Free;
    raise;
  end;
end;

function TModifyThreadRequestSerializer.Serialize : String;
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

class function TModifyThreadRequestSerializer.Deserialize(aJSON : TJSONObject) : TModifyThreadRequest;

var
  lArr : TJSONArray;
  i : Integer;
  lFaddLabelIds : TStringDynArray;
  lFremoveLabelIds : TStringDynArray;
begin
  Result := TModifyThreadRequest.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('addLabelIds',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFaddLabelIds,lArr.Count);
    For I:=0 to Length(lFaddLabelIds)-1 do
      lFaddLabelIds[i]:=lArr[i].Asstring;
    Result.addLabelIds:=lFaddLabelIds;
    end;
  lArr:=aJSON.Get('removeLabelIds',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFremoveLabelIds,lArr.Count);
    For I:=0 to Length(lFremoveLabelIds)-1 do
      lFremoveLabelIds[i]:=lArr[i].Asstring;
    Result.removeLabelIds:=lFremoveLabelIds;
    end;
end;

class function TModifyThreadRequestSerializer.Deserialize(aJSON : String) : TModifyThreadRequest;

var
  lObj : TJSONObject;
begin
  Result := Default(TModifyThreadRequest);
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

function TObliterateCseKeyPairRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
  except
    Result.Free;
    raise;
  end;
end;

function TObliterateCseKeyPairRequestSerializer.Serialize : String;
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

class function TObliterateCseKeyPairRequestSerializer.Deserialize(aJSON : TJSONObject) : TObliterateCseKeyPairRequest;

begin
  Result := TObliterateCseKeyPairRequest.Create;
  If (aJSON=Nil) then
    exit;
end;

class function TObliterateCseKeyPairRequestSerializer.Deserialize(aJSON : String) : TObliterateCseKeyPairRequest;

var
  lObj : TJSONObject;
begin
  Result := Default(TObliterateCseKeyPairRequest);
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

function TPopSettingsSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('accessWindow',accessWindow);
    Result.Add('disposition',disposition);
  except
    Result.Free;
    raise;
  end;
end;

function TPopSettingsSerializer.Serialize : String;
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

class function TPopSettingsSerializer.Deserialize(aJSON : TJSONObject) : TPopSettings;

begin
  Result := TPopSettings.Create;
  If (aJSON=Nil) then
    exit;
  Result.accessWindow:=aJSON.Get('accessWindow','');
  Result.disposition:=aJSON.Get('disposition','');
end;

class function TPopSettingsSerializer.Deserialize(aJSON : String) : TPopSettings;

var
  lObj : TJSONObject;
begin
  Result := Default(TPopSettings);
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

function TProfileSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('emailAddress',emailAddress);
    Result.Add('historyId',historyId);
    Result.Add('messagesTotal',messagesTotal);
    Result.Add('threadsTotal',threadsTotal);
  except
    Result.Free;
    raise;
  end;
end;

function TProfileSerializer.Serialize : String;
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

class function TProfileSerializer.Deserialize(aJSON : TJSONObject) : TProfile;

begin
  Result := TProfile.Create;
  If (aJSON=Nil) then
    exit;
  Result.emailAddress:=aJSON.Get('emailAddress','');
  Result.historyId:=aJSON.Get('historyId','');
  Result.messagesTotal:=aJSON.Get('messagesTotal',0);
  Result.threadsTotal:=aJSON.Get('threadsTotal',0);
end;

class function TProfileSerializer.Deserialize(aJSON : String) : TProfile;

var
  lObj : TJSONObject;
begin
  Result := Default(TProfile);
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

function TVacationSettingsSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('enableAutoReply',enableAutoReply);
    Result.Add('endTime',endTime);
    Result.Add('responseBodyHtml',responseBodyHtml);
    Result.Add('responseBodyPlainText',responseBodyPlainText);
    Result.Add('responseSubject',responseSubject);
    Result.Add('restrictToContacts',restrictToContacts);
    Result.Add('restrictToDomain',restrictToDomain);
    Result.Add('startTime',startTime);
  except
    Result.Free;
    raise;
  end;
end;

function TVacationSettingsSerializer.Serialize : String;
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

class function TVacationSettingsSerializer.Deserialize(aJSON : TJSONObject) : TVacationSettings;

begin
  Result := TVacationSettings.Create;
  If (aJSON=Nil) then
    exit;
  Result.enableAutoReply:=aJSON.Get('enableAutoReply',False);
  Result.endTime:=aJSON.Get('endTime','');
  Result.responseBodyHtml:=aJSON.Get('responseBodyHtml','');
  Result.responseBodyPlainText:=aJSON.Get('responseBodyPlainText','');
  Result.responseSubject:=aJSON.Get('responseSubject','');
  Result.restrictToContacts:=aJSON.Get('restrictToContacts',False);
  Result.restrictToDomain:=aJSON.Get('restrictToDomain',False);
  Result.startTime:=aJSON.Get('startTime','');
end;

class function TVacationSettingsSerializer.Deserialize(aJSON : String) : TVacationSettings;

var
  lObj : TJSONObject;
begin
  Result := Default(TVacationSettings);
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

function TWatchRequestSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('labelFilterAction',labelFilterAction);
    Result.Add('labelFilterBehavior',labelFilterBehavior);
    Arr:=TJSONArray.Create;
    Result.Add('labelIds',Arr);
    For I:=0 to Length(labelIds)-1 do
      Arr.Add(labelIds[i]);
    Result.Add('topicName',topicName);
  except
    Result.Free;
    raise;
  end;
end;

function TWatchRequestSerializer.Serialize : String;
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

class function TWatchRequestSerializer.Deserialize(aJSON : TJSONObject) : TWatchRequest;

var
  lArr : TJSONArray;
  i : Integer;
  lFlabelIds : TStringDynArray;
begin
  Result := TWatchRequest.Create;
  If (aJSON=Nil) then
    exit;
  Result.labelFilterAction:=aJSON.Get('labelFilterAction','');
  Result.labelFilterBehavior:=aJSON.Get('labelFilterBehavior','');
  lArr:=aJSON.Get('labelIds',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(lFlabelIds,lArr.Count);
    For I:=0 to Length(lFlabelIds)-1 do
      lFlabelIds[i]:=lArr[i].Asstring;
    Result.labelIds:=lFlabelIds;
    end;
  Result.topicName:=aJSON.Get('topicName','');
end;

class function TWatchRequestSerializer.Deserialize(aJSON : String) : TWatchRequest;

var
  lObj : TJSONObject;
begin
  Result := Default(TWatchRequest);
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

function TWatchResponseSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('expiration',expiration);
    Result.Add('historyId',historyId);
  except
    Result.Free;
    raise;
  end;
end;

function TWatchResponseSerializer.Serialize : String;
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

class function TWatchResponseSerializer.Deserialize(aJSON : TJSONObject) : TWatchResponse;

begin
  Result := TWatchResponse.Create;
  If (aJSON=Nil) then
    exit;
  Result.expiration:=aJSON.Get('expiration','');
  Result.historyId:=aJSON.Get('historyId','');
end;

class function TWatchResponseSerializer.Deserialize(aJSON : String) : TWatchResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TWatchResponse);
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


function TClassificationLabelFieldValueArraySerializer.SerializeArray : TJSONArray;
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


function TClassificationLabelFieldValueArraySerializer.Serialize : String;
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

class function TClassificationLabelFieldValueArraySerializer.Deserialize(aJSON : TJSONArray) : TClassificationLabelFieldValueArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TClassificationLabelFieldValue.Deserialize(aJSON[i] as TJSONObject);
end;

class function TClassificationLabelFieldValueArraySerializer.Deserialize(aJSON : String) : TClassificationLabelFieldValueArray; 

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


function TClassificationLabelValueArraySerializer.SerializeArray : TJSONArray;
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


function TClassificationLabelValueArraySerializer.Serialize : String;
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

class function TClassificationLabelValueArraySerializer.Deserialize(aJSON : TJSONArray) : TClassificationLabelValueArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TClassificationLabelValue.Deserialize(aJSON[i] as TJSONObject);
end;

class function TClassificationLabelValueArraySerializer.Deserialize(aJSON : String) : TClassificationLabelValueArray; 

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


function TCseIdentityArraySerializer.SerializeArray : TJSONArray;
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


function TCseIdentityArraySerializer.Serialize : String;
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

class function TCseIdentityArraySerializer.Deserialize(aJSON : TJSONArray) : TCseIdentityArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TCseIdentity.Deserialize(aJSON[i] as TJSONObject);
end;

class function TCseIdentityArraySerializer.Deserialize(aJSON : String) : TCseIdentityArray; 

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


function TCseKeyPairArraySerializer.SerializeArray : TJSONArray;
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


function TCseKeyPairArraySerializer.Serialize : String;
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

class function TCseKeyPairArraySerializer.Deserialize(aJSON : TJSONArray) : TCseKeyPairArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TCseKeyPair.Deserialize(aJSON[i] as TJSONObject);
end;

class function TCseKeyPairArraySerializer.Deserialize(aJSON : String) : TCseKeyPairArray; 

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


function TCsePrivateKeyMetadataArraySerializer.SerializeArray : TJSONArray;
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


function TCsePrivateKeyMetadataArraySerializer.Serialize : String;
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

class function TCsePrivateKeyMetadataArraySerializer.Deserialize(aJSON : TJSONArray) : TCsePrivateKeyMetadataArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TCsePrivateKeyMetadata.Deserialize(aJSON[i] as TJSONObject);
end;

class function TCsePrivateKeyMetadataArraySerializer.Deserialize(aJSON : String) : TCsePrivateKeyMetadataArray; 

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


function TDelegateArraySerializer.SerializeArray : TJSONArray;
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


function TDelegateArraySerializer.Serialize : String;
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

class function TDelegateArraySerializer.Deserialize(aJSON : TJSONArray) : TDelegateArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TDelegate.Deserialize(aJSON[i] as TJSONObject);
end;

class function TDelegateArraySerializer.Deserialize(aJSON : String) : TDelegateArray; 

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


function TDraftArraySerializer.SerializeArray : TJSONArray;
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


function TDraftArraySerializer.Serialize : String;
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

class function TDraftArraySerializer.Deserialize(aJSON : TJSONArray) : TDraftArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TDraft.Deserialize(aJSON[i] as TJSONObject);
end;

class function TDraftArraySerializer.Deserialize(aJSON : String) : TDraftArray; 

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


function TFilterArraySerializer.SerializeArray : TJSONArray;
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


function TFilterArraySerializer.Serialize : String;
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

class function TFilterArraySerializer.Deserialize(aJSON : TJSONArray) : TFilterArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TFilter.Deserialize(aJSON[i] as TJSONObject);
end;

class function TFilterArraySerializer.Deserialize(aJSON : String) : TFilterArray; 

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


function TForwardingAddressArraySerializer.SerializeArray : TJSONArray;
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


function TForwardingAddressArraySerializer.Serialize : String;
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

class function TForwardingAddressArraySerializer.Deserialize(aJSON : TJSONArray) : TForwardingAddressArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TForwardingAddress.Deserialize(aJSON[i] as TJSONObject);
end;

class function TForwardingAddressArraySerializer.Deserialize(aJSON : String) : TForwardingAddressArray; 

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


function THistoryLabelAddedArraySerializer.SerializeArray : TJSONArray;
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


function THistoryLabelAddedArraySerializer.Serialize : String;
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

class function THistoryLabelAddedArraySerializer.Deserialize(aJSON : TJSONArray) : THistoryLabelAddedArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=THistoryLabelAdded.Deserialize(aJSON[i] as TJSONObject);
end;

class function THistoryLabelAddedArraySerializer.Deserialize(aJSON : String) : THistoryLabelAddedArray; 

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


function THistoryLabelRemovedArraySerializer.SerializeArray : TJSONArray;
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


function THistoryLabelRemovedArraySerializer.Serialize : String;
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

class function THistoryLabelRemovedArraySerializer.Deserialize(aJSON : TJSONArray) : THistoryLabelRemovedArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=THistoryLabelRemoved.Deserialize(aJSON[i] as TJSONObject);
end;

class function THistoryLabelRemovedArraySerializer.Deserialize(aJSON : String) : THistoryLabelRemovedArray; 

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


function THistoryMessageAddedArraySerializer.SerializeArray : TJSONArray;
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


function THistoryMessageAddedArraySerializer.Serialize : String;
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

class function THistoryMessageAddedArraySerializer.Deserialize(aJSON : TJSONArray) : THistoryMessageAddedArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=THistoryMessageAdded.Deserialize(aJSON[i] as TJSONObject);
end;

class function THistoryMessageAddedArraySerializer.Deserialize(aJSON : String) : THistoryMessageAddedArray; 

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


function THistoryMessageDeletedArraySerializer.SerializeArray : TJSONArray;
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


function THistoryMessageDeletedArraySerializer.Serialize : String;
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

class function THistoryMessageDeletedArraySerializer.Deserialize(aJSON : TJSONArray) : THistoryMessageDeletedArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=THistoryMessageDeleted.Deserialize(aJSON[i] as TJSONObject);
end;

class function THistoryMessageDeletedArraySerializer.Deserialize(aJSON : String) : THistoryMessageDeletedArray; 

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


function THistoryArraySerializer.SerializeArray : TJSONArray;
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


function THistoryArraySerializer.Serialize : String;
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

class function THistoryArraySerializer.Deserialize(aJSON : TJSONArray) : THistoryArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=THistory.Deserialize(aJSON[i] as TJSONObject);
end;

class function THistoryArraySerializer.Deserialize(aJSON : String) : THistoryArray; 

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


function TMessagePartHeaderArraySerializer.SerializeArray : TJSONArray;
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


function TMessagePartHeaderArraySerializer.Serialize : String;
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

class function TMessagePartHeaderArraySerializer.Deserialize(aJSON : TJSONArray) : TMessagePartHeaderArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TMessagePartHeader.Deserialize(aJSON[i] as TJSONObject);
end;

class function TMessagePartHeaderArraySerializer.Deserialize(aJSON : String) : TMessagePartHeaderArray; 

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


function TMessagePartArraySerializer.SerializeArray : TJSONArray;
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


function TMessagePartArraySerializer.Serialize : String;
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

class function TMessagePartArraySerializer.Deserialize(aJSON : TJSONArray) : TMessagePartArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TMessagePart.Deserialize(aJSON[i] as TJSONObject);
end;

class function TMessagePartArraySerializer.Deserialize(aJSON : String) : TMessagePartArray; 

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


function TMessageArraySerializer.SerializeArray : TJSONArray;
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


function TMessageArraySerializer.Serialize : String;
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

class function TMessageArraySerializer.Deserialize(aJSON : TJSONArray) : TMessageArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TMessage.Deserialize(aJSON[i] as TJSONObject);
end;

class function TMessageArraySerializer.Deserialize(aJSON : String) : TMessageArray; 

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


function TSendAsArraySerializer.SerializeArray : TJSONArray;
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


function TSendAsArraySerializer.Serialize : String;
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

class function TSendAsArraySerializer.Deserialize(aJSON : TJSONArray) : TSendAsArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TSendAs.Deserialize(aJSON[i] as TJSONObject);
end;

class function TSendAsArraySerializer.Deserialize(aJSON : String) : TSendAsArray; 

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


function TSmimeInfoArraySerializer.SerializeArray : TJSONArray;
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


function TSmimeInfoArraySerializer.Serialize : String;
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

class function TSmimeInfoArraySerializer.Deserialize(aJSON : TJSONArray) : TSmimeInfoArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TSmimeInfo.Deserialize(aJSON[i] as TJSONObject);
end;

class function TSmimeInfoArraySerializer.Deserialize(aJSON : String) : TSmimeInfoArray; 

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


function TThread_ArraySerializer.SerializeArray : TJSONArray;
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


function TThread_ArraySerializer.Serialize : String;
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

class function TThread_ArraySerializer.Deserialize(aJSON : TJSONArray) : TThread_Array; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TThread_.Deserialize(aJSON[i] as TJSONObject);
end;

class function TThread_ArraySerializer.Deserialize(aJSON : String) : TThread_Array; 

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
