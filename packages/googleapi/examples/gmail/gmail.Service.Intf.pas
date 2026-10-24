{ -----------------------------------------------------------------------
  Do not edit !
  
  This file was automatically generated on 2026-10-03 18:11.
  Used command-line parameters:
     -s gmail -C codegen.ini -o gmail -q
  Source OpenAPI document data:
    Title: Gmail API
    Version: v1
  -----------------------------------------------------------------------}
unit gmail.Service.Intf;

{$mode objfpc}
{$h+}
{$modeswitch advancedrecords}

interface

uses
   SysUtils, classes, fpopenapiclient, gmail.Dto;

Type
  // Complex response result types
  
  TDelegatesDeleteResponseKind = (
    DelegatesDeleterkSuccess,
    DelegatesDeleterkUnexpected
  );
  
  TDelegatesDeleteResult = record
    public
    FStatusCode: Integer;
    FStatusText: String;
    FContentType: String;
    FResponseKind: TDelegatesDeleteResponseKind;
    FRawContent: String;
    FRawStream: TStream;
    property StatusCode: Integer read FStatusCode;
    property StatusText: String read FStatusText;
    property ContentType: String read FContentType;
    property ResponseKind: TDelegatesDeleteResponseKind read FResponseKind;
    property RawContent: String read FRawContent;
    property RawStream: TStream read FRawStream;
    function Success: Boolean; inline;
    function IsClientError: Boolean; inline;
    function IsServerError: Boolean; inline;
    procedure Clear;
    function ExtractRawStream: TStream;
  end;
  
  TDraftsDeleteResponseKind = (
    DraftsDeleterkSuccess,
    DraftsDeleterkUnexpected
  );
  
  TDraftsDeleteResult = record
    public
    FStatusCode: Integer;
    FStatusText: String;
    FContentType: String;
    FResponseKind: TDraftsDeleteResponseKind;
    FRawContent: String;
    FRawStream: TStream;
    property StatusCode: Integer read FStatusCode;
    property StatusText: String read FStatusText;
    property ContentType: String read FContentType;
    property ResponseKind: TDraftsDeleteResponseKind read FResponseKind;
    property RawContent: String read FRawContent;
    property RawStream: TStream read FRawStream;
    function Success: Boolean; inline;
    function IsClientError: Boolean; inline;
    function IsServerError: Boolean; inline;
    procedure Clear;
    function ExtractRawStream: TStream;
  end;
  
  TFiltersDeleteResponseKind = (
    FiltersDeleterkSuccess,
    FiltersDeleterkUnexpected
  );
  
  TFiltersDeleteResult = record
    public
    FStatusCode: Integer;
    FStatusText: String;
    FContentType: String;
    FResponseKind: TFiltersDeleteResponseKind;
    FRawContent: String;
    FRawStream: TStream;
    property StatusCode: Integer read FStatusCode;
    property StatusText: String read FStatusText;
    property ContentType: String read FContentType;
    property ResponseKind: TFiltersDeleteResponseKind read FResponseKind;
    property RawContent: String read FRawContent;
    property RawStream: TStream read FRawStream;
    function Success: Boolean; inline;
    function IsClientError: Boolean; inline;
    function IsServerError: Boolean; inline;
    procedure Clear;
    function ExtractRawStream: TStream;
  end;
  
  TForwardingAddressesDeleteResponseKind = (
    ForwardingAddressesDeleterkSuccess,
    ForwardingAddressesDeleterkUnexpected
  );
  
  TForwardingAddressesDeleteResult = record
    public
    FStatusCode: Integer;
    FStatusText: String;
    FContentType: String;
    FResponseKind: TForwardingAddressesDeleteResponseKind;
    FRawContent: String;
    FRawStream: TStream;
    property StatusCode: Integer read FStatusCode;
    property StatusText: String read FStatusText;
    property ContentType: String read FContentType;
    property ResponseKind: TForwardingAddressesDeleteResponseKind read FResponseKind;
    property RawContent: String read FRawContent;
    property RawStream: TStream read FRawStream;
    function Success: Boolean; inline;
    function IsClientError: Boolean; inline;
    function IsServerError: Boolean; inline;
    procedure Clear;
    function ExtractRawStream: TStream;
  end;
  
  TIdentitiesDeleteResponseKind = (
    IdentitiesDeleterkSuccess,
    IdentitiesDeleterkUnexpected
  );
  
  TIdentitiesDeleteResult = record
    public
    FStatusCode: Integer;
    FStatusText: String;
    FContentType: String;
    FResponseKind: TIdentitiesDeleteResponseKind;
    FRawContent: String;
    FRawStream: TStream;
    property StatusCode: Integer read FStatusCode;
    property StatusText: String read FStatusText;
    property ContentType: String read FContentType;
    property ResponseKind: TIdentitiesDeleteResponseKind read FResponseKind;
    property RawContent: String read FRawContent;
    property RawStream: TStream read FRawStream;
    function Success: Boolean; inline;
    function IsClientError: Boolean; inline;
    function IsServerError: Boolean; inline;
    procedure Clear;
    function ExtractRawStream: TStream;
  end;
  
  TLabelsDeleteResponseKind = (
    LabelsDeleterkSuccess,
    LabelsDeleterkUnexpected
  );
  
  TLabelsDeleteResult = record
    public
    FStatusCode: Integer;
    FStatusText: String;
    FContentType: String;
    FResponseKind: TLabelsDeleteResponseKind;
    FRawContent: String;
    FRawStream: TStream;
    property StatusCode: Integer read FStatusCode;
    property StatusText: String read FStatusText;
    property ContentType: String read FContentType;
    property ResponseKind: TLabelsDeleteResponseKind read FResponseKind;
    property RawContent: String read FRawContent;
    property RawStream: TStream read FRawStream;
    function Success: Boolean; inline;
    function IsClientError: Boolean; inline;
    function IsServerError: Boolean; inline;
    procedure Clear;
    function ExtractRawStream: TStream;
  end;
  
  TMessagesDeleteResponseKind = (
    MessagesDeleterkSuccess,
    MessagesDeleterkUnexpected
  );
  
  TMessagesDeleteResult = record
    public
    FStatusCode: Integer;
    FStatusText: String;
    FContentType: String;
    FResponseKind: TMessagesDeleteResponseKind;
    FRawContent: String;
    FRawStream: TStream;
    property StatusCode: Integer read FStatusCode;
    property StatusText: String read FStatusText;
    property ContentType: String read FContentType;
    property ResponseKind: TMessagesDeleteResponseKind read FResponseKind;
    property RawContent: String read FRawContent;
    property RawStream: TStream read FRawStream;
    function Success: Boolean; inline;
    function IsClientError: Boolean; inline;
    function IsServerError: Boolean; inline;
    procedure Clear;
    function ExtractRawStream: TStream;
  end;
  
  TSendAsDeleteResponseKind = (
    SendAsDeleterkSuccess,
    SendAsDeleterkUnexpected
  );
  
  TSendAsDeleteResult = record
    public
    FStatusCode: Integer;
    FStatusText: String;
    FContentType: String;
    FResponseKind: TSendAsDeleteResponseKind;
    FRawContent: String;
    FRawStream: TStream;
    property StatusCode: Integer read FStatusCode;
    property StatusText: String read FStatusText;
    property ContentType: String read FContentType;
    property ResponseKind: TSendAsDeleteResponseKind read FResponseKind;
    property RawContent: String read FRawContent;
    property RawStream: TStream read FRawStream;
    function Success: Boolean; inline;
    function IsClientError: Boolean; inline;
    function IsServerError: Boolean; inline;
    procedure Clear;
    function ExtractRawStream: TStream;
  end;
  
  TSmimeInfoDeleteResponseKind = (
    SmimeInfoDeleterkSuccess,
    SmimeInfoDeleterkUnexpected
  );
  
  TSmimeInfoDeleteResult = record
    public
    FStatusCode: Integer;
    FStatusText: String;
    FContentType: String;
    FResponseKind: TSmimeInfoDeleteResponseKind;
    FRawContent: String;
    FRawStream: TStream;
    property StatusCode: Integer read FStatusCode;
    property StatusText: String read FStatusText;
    property ContentType: String read FContentType;
    property ResponseKind: TSmimeInfoDeleteResponseKind read FResponseKind;
    property RawContent: String read FRawContent;
    property RawStream: TStream read FRawStream;
    function Success: Boolean; inline;
    function IsClientError: Boolean; inline;
    function IsServerError: Boolean; inline;
    procedure Clear;
    function ExtractRawStream: TStream;
  end;
  
  TThreadsDeleteResponseKind = (
    ThreadsDeleterkSuccess,
    ThreadsDeleterkUnexpected
  );
  
  TThreadsDeleteResult = record
    public
    FStatusCode: Integer;
    FStatusText: String;
    FContentType: String;
    FResponseKind: TThreadsDeleteResponseKind;
    FRawContent: String;
    FRawStream: TStream;
    property StatusCode: Integer read FStatusCode;
    property StatusText: String read FStatusText;
    property ContentType: String read FContentType;
    property ResponseKind: TThreadsDeleteResponseKind read FResponseKind;
    property RawContent: String read FRawContent;
    property RawStream: TStream read FRawStream;
    function Success: Boolean; inline;
    function IsClientError: Boolean; inline;
    function IsServerError: Boolean; inline;
    procedure Clear;
    function ExtractRawStream: TStream;
  end;
  
  // Service result types
  TAutoForwardingServiceResult = specialize TServiceResult<TAutoForwarding>;
  TCseIdentityServiceResult = specialize TServiceResult<TCseIdentity>;
  TCseKeyPairServiceResult = specialize TServiceResult<TCseKeyPair>;
  TDelegateServiceResult = specialize TServiceResult<TDelegate>;
  TDraftServiceResult = specialize TServiceResult<TDraft>;
  TFilterServiceResult = specialize TServiceResult<TFilter>;
  TForwardingAddressServiceResult = specialize TServiceResult<TForwardingAddress>;
  TImapSettingsServiceResult = specialize TServiceResult<TImapSettings>;
  TLabelServiceResult = specialize TServiceResult<TLabel>;
  TLanguageSettingsServiceResult = specialize TServiceResult<TLanguageSettings>;
  TListCseIdentitiesResponseServiceResult = specialize TServiceResult<TListCseIdentitiesResponse>;
  TListCseKeyPairsResponseServiceResult = specialize TServiceResult<TListCseKeyPairsResponse>;
  TListDelegatesResponseServiceResult = specialize TServiceResult<TListDelegatesResponse>;
  TListDraftsResponseServiceResult = specialize TServiceResult<TListDraftsResponse>;
  TListFiltersResponseServiceResult = specialize TServiceResult<TListFiltersResponse>;
  TListForwardingAddressesResponseServiceResult = specialize TServiceResult<TListForwardingAddressesResponse>;
  TListHistoryResponseServiceResult = specialize TServiceResult<TListHistoryResponse>;
  TListLabelsResponseServiceResult = specialize TServiceResult<TListLabelsResponse>;
  TListMessagesResponseServiceResult = specialize TServiceResult<TListMessagesResponse>;
  TListSendAsResponseServiceResult = specialize TServiceResult<TListSendAsResponse>;
  TListSmimeInfoResponseServiceResult = specialize TServiceResult<TListSmimeInfoResponse>;
  TListThreadsResponseServiceResult = specialize TServiceResult<TListThreadsResponse>;
  TMessagePartBodyServiceResult = specialize TServiceResult<TMessagePartBody>;
  TMessageServiceResult = specialize TServiceResult<TMessage>;
  TPopSettingsServiceResult = specialize TServiceResult<TPopSettings>;
  TProfileServiceResult = specialize TServiceResult<TProfile>;
  TSendAsServiceResult = specialize TServiceResult<TSendAs>;
  TSmimeInfoServiceResult = specialize TServiceResult<TSmimeInfo>;
  TThread_ServiceResult = specialize TServiceResult<TThread_>;
  TVacationSettingsServiceResult = specialize TServiceResult<TVacationSettings>;
  TWatchResponseServiceResult = specialize TServiceResult<TWatchResponse>;
  
  // Service IAttachments
  
  IAttachments = interface  ['{6CD8ABC4-B06C-4152-A599-2070B1639860}']
    Function Get(aId : string; aMessageId : string; aUserId : string = 'me') : TMessagePartBodyServiceResult;
  end;
  
  // Service IDelegates
  
  IDelegates = interface  ['{0CAD9E16-765F-40C8-B288-68EAB81CFBB6}']
    Function Create_(aRequest : TDelegate; aUserId : string = 'me') : TDelegateServiceResult;
    Function Delete(aDelegateEmail : string; aUserId : string = 'me') : TDelegatesDeleteResult;
    Function Get(aDelegateEmail : string; aUserId : string = 'me') : TDelegateServiceResult;
    Function List(aUserId : string = 'me') : TListDelegatesResponseServiceResult;
  end;
  
  // Service IDrafts
  
  IDrafts = interface  ['{33BAFC1B-FEC4-4427-998E-3E889BECA854}']
    Function Create_(aRequest : TDraft; aUserId : string = 'me') : TDraftServiceResult;
    Function Delete(aId : string; aUserId : string = 'me') : TDraftsDeleteResult;
    Function Get(aId : string; aFormat : string = 'full'; aUserId : string = 'me') : TDraftServiceResult;
    Function List(aPageToken : string; aQ : string; aIncludeSpamTrash : boolean = false; aMaxResults : integer = 100; aUserId : string = 'me') : TListDraftsResponseServiceResult;
    Function Send(aRequest : TDraft; aUserId : string = 'me') : TMessageServiceResult;
    Function Update(aId : string; aRequest : TDraft; aUserId : string = 'me') : TDraftServiceResult;
  end;
  
  // Service IFilters
  
  IFilters = interface  ['{124DC417-661D-41E8-B79C-E02CEFF0D7EA}']
    Function Create_(aRequest : TFilter; aUserId : string = 'me') : TFilterServiceResult;
    Function Delete(aId : string; aUserId : string = 'me') : TFiltersDeleteResult;
    Function Get(aId : string; aUserId : string = 'me') : TFilterServiceResult;
    Function List(aUserId : string = 'me') : TListFiltersResponseServiceResult;
  end;
  
  // Service IForwardingAddresses
  
  IForwardingAddresses = interface  ['{4F107A2C-A157-4BC7-AB45-1E9C76241D9D}']
    Function Create_(aRequest : TForwardingAddress; aUserId : string = 'me') : TForwardingAddressServiceResult;
    Function Delete(aForwardingEmail : string; aUserId : string = 'me') : TForwardingAddressesDeleteResult;
    Function Get(aForwardingEmail : string; aUserId : string = 'me') : TForwardingAddressServiceResult;
    Function List(aUserId : string = 'me') : TListForwardingAddressesResponseServiceResult;
  end;
  
  // Service IHistory
  
  IHistory = interface  ['{D7B602FC-31C9-42F0-9239-4BE9D5C2D05E}']
    Function List(aHistoryTypes : string; aLabelId : string; aPageToken : string; aStartHistoryId : string; aMaxResults : integer = 100; aUserId : string = 'me') : TListHistoryResponseServiceResult;
  end;
  
  // Service IIdentities
  
  IIdentities = interface  ['{A699AB20-93ED-4BCF-BB33-5CF81428E81A}']
    Function Create_(aRequest : TCseIdentity; aUserId : string = 'me') : TCseIdentityServiceResult;
    Function Delete(aCseEmailAddress : string; aUserId : string = 'me') : TIdentitiesDeleteResult;
    Function Get(aCseEmailAddress : string; aUserId : string = 'me') : TCseIdentityServiceResult;
    Function List(aPageToken : string; aPageSize : integer = 20; aUserId : string = 'me') : TListCseIdentitiesResponseServiceResult;
    Function Patch(aEmailAddress : string; aRequest : TCseIdentity; aUserId : string = 'me') : TCseIdentityServiceResult;
  end;
  
  // Service IKeypairs
  
  IKeypairs = interface  ['{103BC8BC-4FD8-4C1D-A69E-7526511FAEDA}']
    Function Create_(aRequest : TCseKeyPair; aChainValidation : string = 'all'; aUserId : string = 'me') : TCseKeyPairServiceResult;
    Function Disable(aKeyPairId : string; aRequest : TDisableCseKeyPairRequest; aUserId : string = 'me') : TCseKeyPairServiceResult;
    Function Enable(aKeyPairId : string; aRequest : TEnableCseKeyPairRequest; aUserId : string = 'me') : TCseKeyPairServiceResult;
    Function Get(aKeyPairId : string; aUserId : string = 'me') : TCseKeyPairServiceResult;
    Function List(aPageToken : string; aPageSize : integer = 20; aUserId : string = 'me') : TListCseKeyPairsResponseServiceResult;
  end;
  
  // Service ILabels
  
  ILabels = interface  ['{852468D8-B0C8-4B8E-8D37-EF5DB6DB5CDA}']
    Function Create_(aRequest : TLabel; aUserId : string = 'me') : TLabelServiceResult;
    Function Delete(aId : string; aUserId : string = 'me') : TLabelsDeleteResult;
    Function Get(aId : string; aUserId : string = 'me') : TLabelServiceResult;
    Function List(aUserId : string = 'me') : TListLabelsResponseServiceResult;
    Function Patch(aId : string; aRequest : TLabel; aUserId : string = 'me') : TLabelServiceResult;
    Function Update(aId : string; aRequest : TLabel; aUserId : string = 'me') : TLabelServiceResult;
  end;
  
  // Service IMessages
  
  IMessages = interface  ['{EA725396-CC5F-4B9F-B2C5-BA30992C8DA7}']
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
  
  ISendAs = interface  ['{BAF434E1-4211-49BB-BD79-5D5586BADE36}']
    Function Create_(aRequest : TSendAs; aUserId : string = 'me') : TSendAsServiceResult;
    Function Delete(aSendAsEmail : string; aUserId : string = 'me') : TSendAsDeleteResult;
    Function Get(aSendAsEmail : string; aUserId : string = 'me') : TSendAsServiceResult;
    Function List(aUserId : string = 'me') : TListSendAsResponseServiceResult;
    Function Patch(aSendAsEmail : string; aRequest : TSendAs; aUserId : string = 'me') : TSendAsServiceResult;
    Function Update(aSendAsEmail : string; aRequest : TSendAs; aUserId : string = 'me') : TSendAsServiceResult;
  end;
  
  // Service ISettings
  
  ISettings = interface  ['{7ED22F2C-F179-4F17-BC0D-E11B525B0C63}']
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
  
  ISmimeInfo = interface  ['{0536B44B-9740-4E90-A3ED-294DEC194AD4}']
    Function Delete(aId : string; aSendAsEmail : string; aUserId : string = 'me') : TSmimeInfoDeleteResult;
    Function Get(aId : string; aSendAsEmail : string; aUserId : string = 'me') : TSmimeInfoServiceResult;
    Function Insert(aSendAsEmail : string; aRequest : TSmimeInfo; aUserId : string = 'me') : TSmimeInfoServiceResult;
    Function List(aSendAsEmail : string; aUserId : string = 'me') : TListSmimeInfoResponseServiceResult;
  end;
  
  // Service IThreads
  
  IThreads = interface  ['{3691C728-B4D1-4FFD-BB19-0BA0FFCDC04E}']
    Function Delete(aId : string; aUserId : string = 'me') : TThreadsDeleteResult;
    Function Get(aId : string; aMetadataHeaders : string; aFormat : string = 'full'; aUserId : string = 'me') : TThread_ServiceResult;
    Function List(aLabelIds : string; aPageToken : string; aQ : string; aIncludeSpamTrash : boolean = false; aMaxResults : integer = 100; aUserId : string = 'me') : TListThreadsResponseServiceResult;
    Function Modify(aId : string; aRequest : TModifyThreadRequest; aUserId : string = 'me') : TThread_ServiceResult;
    Function Trash(aId : string; aUserId : string = 'me') : TThread_ServiceResult;
    Function Untrash(aId : string; aUserId : string = 'me') : TThread_ServiceResult;
  end;
  
  // Service IUsers
  
  IUsers = interface  ['{79AD9158-C8C9-4783-BCFA-2524BD24A123}']
    Function GetProfile(aUserId : string = 'me') : TProfileServiceResult;
    Function Watch(aRequest : TWatchRequest; aUserId : string = 'me') : TWatchResponseServiceResult;
  end;
  

implementation

function TDelegatesDeleteResult.Success: Boolean;
begin
  Result:=(FStatusCode >= 200) and (FStatusCode < 300);
end;

function TDelegatesDeleteResult.IsClientError: Boolean;
begin
  Result:=(FStatusCode >= 400) and (FStatusCode < 500);
end;

function TDelegatesDeleteResult.IsServerError: Boolean;
begin
  Result:=(FStatusCode >= 500) and (FStatusCode < 600);
end;

procedure TDelegatesDeleteResult.Clear;
begin
  FStatusCode:=0;
  FStatusText:='';
  FContentType:='';
  FResponseKind:=Default(TDelegatesDeleteResponseKind);
  FRawContent:='';
  FreeAndNil(FRawStream);
end;

function TDelegatesDeleteResult.ExtractRawStream: TStream;
begin
  Result:=FRawStream;
  FRawStream:=nil;
end;

function TDraftsDeleteResult.Success: Boolean;
begin
  Result:=(FStatusCode >= 200) and (FStatusCode < 300);
end;

function TDraftsDeleteResult.IsClientError: Boolean;
begin
  Result:=(FStatusCode >= 400) and (FStatusCode < 500);
end;

function TDraftsDeleteResult.IsServerError: Boolean;
begin
  Result:=(FStatusCode >= 500) and (FStatusCode < 600);
end;

procedure TDraftsDeleteResult.Clear;
begin
  FStatusCode:=0;
  FStatusText:='';
  FContentType:='';
  FResponseKind:=Default(TDraftsDeleteResponseKind);
  FRawContent:='';
  FreeAndNil(FRawStream);
end;

function TDraftsDeleteResult.ExtractRawStream: TStream;
begin
  Result:=FRawStream;
  FRawStream:=nil;
end;

function TFiltersDeleteResult.Success: Boolean;
begin
  Result:=(FStatusCode >= 200) and (FStatusCode < 300);
end;

function TFiltersDeleteResult.IsClientError: Boolean;
begin
  Result:=(FStatusCode >= 400) and (FStatusCode < 500);
end;

function TFiltersDeleteResult.IsServerError: Boolean;
begin
  Result:=(FStatusCode >= 500) and (FStatusCode < 600);
end;

procedure TFiltersDeleteResult.Clear;
begin
  FStatusCode:=0;
  FStatusText:='';
  FContentType:='';
  FResponseKind:=Default(TFiltersDeleteResponseKind);
  FRawContent:='';
  FreeAndNil(FRawStream);
end;

function TFiltersDeleteResult.ExtractRawStream: TStream;
begin
  Result:=FRawStream;
  FRawStream:=nil;
end;

function TForwardingAddressesDeleteResult.Success: Boolean;
begin
  Result:=(FStatusCode >= 200) and (FStatusCode < 300);
end;

function TForwardingAddressesDeleteResult.IsClientError: Boolean;
begin
  Result:=(FStatusCode >= 400) and (FStatusCode < 500);
end;

function TForwardingAddressesDeleteResult.IsServerError: Boolean;
begin
  Result:=(FStatusCode >= 500) and (FStatusCode < 600);
end;

procedure TForwardingAddressesDeleteResult.Clear;
begin
  FStatusCode:=0;
  FStatusText:='';
  FContentType:='';
  FResponseKind:=Default(TForwardingAddressesDeleteResponseKind);
  FRawContent:='';
  FreeAndNil(FRawStream);
end;

function TForwardingAddressesDeleteResult.ExtractRawStream: TStream;
begin
  Result:=FRawStream;
  FRawStream:=nil;
end;

function TIdentitiesDeleteResult.Success: Boolean;
begin
  Result:=(FStatusCode >= 200) and (FStatusCode < 300);
end;

function TIdentitiesDeleteResult.IsClientError: Boolean;
begin
  Result:=(FStatusCode >= 400) and (FStatusCode < 500);
end;

function TIdentitiesDeleteResult.IsServerError: Boolean;
begin
  Result:=(FStatusCode >= 500) and (FStatusCode < 600);
end;

procedure TIdentitiesDeleteResult.Clear;
begin
  FStatusCode:=0;
  FStatusText:='';
  FContentType:='';
  FResponseKind:=Default(TIdentitiesDeleteResponseKind);
  FRawContent:='';
  FreeAndNil(FRawStream);
end;

function TIdentitiesDeleteResult.ExtractRawStream: TStream;
begin
  Result:=FRawStream;
  FRawStream:=nil;
end;

function TLabelsDeleteResult.Success: Boolean;
begin
  Result:=(FStatusCode >= 200) and (FStatusCode < 300);
end;

function TLabelsDeleteResult.IsClientError: Boolean;
begin
  Result:=(FStatusCode >= 400) and (FStatusCode < 500);
end;

function TLabelsDeleteResult.IsServerError: Boolean;
begin
  Result:=(FStatusCode >= 500) and (FStatusCode < 600);
end;

procedure TLabelsDeleteResult.Clear;
begin
  FStatusCode:=0;
  FStatusText:='';
  FContentType:='';
  FResponseKind:=Default(TLabelsDeleteResponseKind);
  FRawContent:='';
  FreeAndNil(FRawStream);
end;

function TLabelsDeleteResult.ExtractRawStream: TStream;
begin
  Result:=FRawStream;
  FRawStream:=nil;
end;

function TMessagesDeleteResult.Success: Boolean;
begin
  Result:=(FStatusCode >= 200) and (FStatusCode < 300);
end;

function TMessagesDeleteResult.IsClientError: Boolean;
begin
  Result:=(FStatusCode >= 400) and (FStatusCode < 500);
end;

function TMessagesDeleteResult.IsServerError: Boolean;
begin
  Result:=(FStatusCode >= 500) and (FStatusCode < 600);
end;

procedure TMessagesDeleteResult.Clear;
begin
  FStatusCode:=0;
  FStatusText:='';
  FContentType:='';
  FResponseKind:=Default(TMessagesDeleteResponseKind);
  FRawContent:='';
  FreeAndNil(FRawStream);
end;

function TMessagesDeleteResult.ExtractRawStream: TStream;
begin
  Result:=FRawStream;
  FRawStream:=nil;
end;

function TSendAsDeleteResult.Success: Boolean;
begin
  Result:=(FStatusCode >= 200) and (FStatusCode < 300);
end;

function TSendAsDeleteResult.IsClientError: Boolean;
begin
  Result:=(FStatusCode >= 400) and (FStatusCode < 500);
end;

function TSendAsDeleteResult.IsServerError: Boolean;
begin
  Result:=(FStatusCode >= 500) and (FStatusCode < 600);
end;

procedure TSendAsDeleteResult.Clear;
begin
  FStatusCode:=0;
  FStatusText:='';
  FContentType:='';
  FResponseKind:=Default(TSendAsDeleteResponseKind);
  FRawContent:='';
  FreeAndNil(FRawStream);
end;

function TSendAsDeleteResult.ExtractRawStream: TStream;
begin
  Result:=FRawStream;
  FRawStream:=nil;
end;

function TSmimeInfoDeleteResult.Success: Boolean;
begin
  Result:=(FStatusCode >= 200) and (FStatusCode < 300);
end;

function TSmimeInfoDeleteResult.IsClientError: Boolean;
begin
  Result:=(FStatusCode >= 400) and (FStatusCode < 500);
end;

function TSmimeInfoDeleteResult.IsServerError: Boolean;
begin
  Result:=(FStatusCode >= 500) and (FStatusCode < 600);
end;

procedure TSmimeInfoDeleteResult.Clear;
begin
  FStatusCode:=0;
  FStatusText:='';
  FContentType:='';
  FResponseKind:=Default(TSmimeInfoDeleteResponseKind);
  FRawContent:='';
  FreeAndNil(FRawStream);
end;

function TSmimeInfoDeleteResult.ExtractRawStream: TStream;
begin
  Result:=FRawStream;
  FRawStream:=nil;
end;

function TThreadsDeleteResult.Success: Boolean;
begin
  Result:=(FStatusCode >= 200) and (FStatusCode < 300);
end;

function TThreadsDeleteResult.IsClientError: Boolean;
begin
  Result:=(FStatusCode >= 400) and (FStatusCode < 500);
end;

function TThreadsDeleteResult.IsServerError: Boolean;
begin
  Result:=(FStatusCode >= 500) and (FStatusCode < 600);
end;

procedure TThreadsDeleteResult.Clear;
begin
  FStatusCode:=0;
  FStatusText:='';
  FContentType:='';
  FResponseKind:=Default(TThreadsDeleteResponseKind);
  FRawContent:='';
  FreeAndNil(FRawStream);
end;

function TThreadsDeleteResult.ExtractRawStream: TStream;
begin
  Result:=FRawStream;
  FRawStream:=nil;
end;

end.
