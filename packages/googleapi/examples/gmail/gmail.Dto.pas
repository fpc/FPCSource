{ -----------------------------------------------------------------------
  Do not edit !
  
  This file was automatically generated on 2026-10-03 10:20.
  Used command-line parameters:
     -s gmail -C codegen.ini -o gmail -q
  Source OpenAPI document data:
    Title: Gmail API
    Version: v1
  -----------------------------------------------------------------------}
unit gmail.Dto;

{$mode objfpc}
{$h+}


interface

uses types;

Type

  TAutoForwarding = class;
  TBatchDeleteMessagesRequest = class;
  TClassificationLabelFieldValue = class;
  TClassificationLabelValue = class;
  TBatchModifyMessagesRequest = class;
  TSignAndEncryptKeyPairs = class;
  TCseIdentity = class;
  THardwareKeyMetadata = class;
  TKaclsKeyMetadata = class;
  TCsePrivateKeyMetadata = class;
  TCseKeyPair = class;
  TDelegate = class;
  TDisableCseKeyPairRequest = class;
  TMessagePartBody = class;
  TMessagePartHeader = class;
  TMessagePart = class;
  TMessage = class;
  TDraft = class;
  TEnableCseKeyPairRequest = class;
  TFilterAction = class;
  TFilterCriteria = class;
  TFilter = class;
  TForwardingAddress = class;
  THistoryLabelAdded = class;
  THistoryLabelRemoved = class;
  THistoryMessageAdded = class;
  THistoryMessageDeleted = class;
  THistory = class;
  TImapSettings = class;
  TLabelColor = class;
  TLabel = class;
  TLanguageSettings = class;
  TListCseIdentitiesResponse = class;
  TListCseKeyPairsResponse = class;
  TListDelegatesResponse = class;
  TListDraftsResponse = class;
  TListFiltersResponse = class;
  TListForwardingAddressesResponse = class;
  TListHistoryResponse = class;
  TListLabelsResponse = class;
  TListMessagesResponse = class;
  TSmtpMsa = class;
  TSendAs = class;
  TListSendAsResponse = class;
  TSmimeInfo = class;
  TListSmimeInfoResponse = class;
  TThread_ = class;
  TListThreadsResponse = class;
  TModifyMessageRequest = class;
  TModifyThreadRequest = class;
  TObliterateCseKeyPairRequest = class;
  TPopSettings = class;
  TProfile = class;
  TVacationSettings = class;
  TWatchRequest = class;
  TWatchResponse = class;
  TClassificationLabelFieldValueArray = Array of TClassificationLabelFieldValue;
  TClassificationLabelValueArray = Array of TClassificationLabelValue;
  TCseIdentityArray = Array of TCseIdentity;
  TCseKeyPairArray = Array of TCseKeyPair;
  TCsePrivateKeyMetadataArray = Array of TCsePrivateKeyMetadata;
  TDelegateArray = Array of TDelegate;
  TDraftArray = Array of TDraft;
  TFilterArray = Array of TFilter;
  TForwardingAddressArray = Array of TForwardingAddress;
  THistoryLabelAddedArray = Array of THistoryLabelAdded;
  THistoryLabelRemovedArray = Array of THistoryLabelRemoved;
  THistoryMessageAddedArray = Array of THistoryMessageAdded;
  THistoryMessageDeletedArray = Array of THistoryMessageDeleted;
  THistoryArray = Array of THistory;
  TLabelArray = Array of TLabel;
  TMessagePartHeaderArray = Array of TMessagePartHeader;
  TMessagePartArray = Array of TMessagePart;
  TMessageArray = Array of TMessage;
  TSendAsArray = Array of TSendAs;
  TSmimeInfoArray = Array of TSmimeInfo;
  TThread_Array = Array of TThread_;
  
  TAutoForwarding = Class(TObject)
  private
    Fdisposition : string;
    FemailAddress : string;
    Fenabled : boolean;
  public
    property disposition : string read Fdisposition write Fdisposition;
    property emailAddress : string read FemailAddress write FemailAddress;
    property enabled : boolean read Fenabled write Fenabled;
  end;
  
  TBatchDeleteMessagesRequest = Class(TObject)
  private
    Fids : TStringDynArray;
  public
    property ids : TStringDynArray read Fids write Fids;
  end;
  
  TClassificationLabelFieldValue = Class(TObject)
  private
    FfieldId : string;
    Fselection : string;
  public
    property fieldId : string read FfieldId write FfieldId;
    property selection : string read Fselection write Fselection;
  end;
  
  TClassificationLabelValue = Class(TObject)
  private
    Ffields : TClassificationLabelFieldValueArray;
    FlabelId : string;
  public
    property fields : TClassificationLabelFieldValueArray read Ffields write Ffields;
    property labelId : string read FlabelId write FlabelId;
  end;
  
  TBatchModifyMessagesRequest = Class(TObject)
  private
    FaddClassificationLabels : TClassificationLabelValueArray;
    FaddLabelIds : TStringDynArray;
    Fids : TStringDynArray;
    FremoveClassificationLabelIds : TStringDynArray;
    FremoveLabelIds : TStringDynArray;
  public
    property addClassificationLabels : TClassificationLabelValueArray read FaddClassificationLabels write FaddClassificationLabels;
    property addLabelIds : TStringDynArray read FaddLabelIds write FaddLabelIds;
    property ids : TStringDynArray read Fids write Fids;
    property removeClassificationLabelIds : TStringDynArray read FremoveClassificationLabelIds write FremoveClassificationLabelIds;
    property removeLabelIds : TStringDynArray read FremoveLabelIds write FremoveLabelIds;
  end;
  
  TSignAndEncryptKeyPairs = Class(TObject)
  private
    FencryptionKeyPairId : string;
    FsigningKeyPairId : string;
  public
    property encryptionKeyPairId : string read FencryptionKeyPairId write FencryptionKeyPairId;
    property signingKeyPairId : string read FsigningKeyPairId write FsigningKeyPairId;
  end;
  
  TCseIdentity = Class(TObject)
  private
    FemailAddress : string;
    FprimaryKeyPairId : string;
    FsignAndEncryptKeyPairs : TSignAndEncryptKeyPairs;
  public
    constructor CreateWithMembers;
    property emailAddress : string read FemailAddress write FemailAddress;
    property primaryKeyPairId : string read FprimaryKeyPairId write FprimaryKeyPairId;
    property signAndEncryptKeyPairs : TSignAndEncryptKeyPairs read FsignAndEncryptKeyPairs write FsignAndEncryptKeyPairs;
  end;
  
  THardwareKeyMetadata = Class(TObject)
  private
    Fdescription : string;
  public
    property description : string read Fdescription write Fdescription;
  end;
  
  TKaclsKeyMetadata = Class(TObject)
  private
    FkaclsData : string;
    FkaclsUri : string;
  public
    property kaclsData : string read FkaclsData write FkaclsData;
    property kaclsUri : string read FkaclsUri write FkaclsUri;
  end;
  
  TCsePrivateKeyMetadata = Class(TObject)
  private
    FhardwareKeyMetadata : THardwareKeyMetadata;
    FkaclsKeyMetadata : TKaclsKeyMetadata;
    FprivateKeyMetadataId : string;
  public
    constructor CreateWithMembers;
    property hardwareKeyMetadata : THardwareKeyMetadata read FhardwareKeyMetadata write FhardwareKeyMetadata;
    property kaclsKeyMetadata : TKaclsKeyMetadata read FkaclsKeyMetadata write FkaclsKeyMetadata;
    property privateKeyMetadataId : string read FprivateKeyMetadataId write FprivateKeyMetadataId;
  end;
  
  TCseKeyPair = Class(TObject)
  private
    FdisableTime : string;
    FenablementState : string;
    FkeyPairId : string;
    Fpem : string;
    Fpkcs7 : string;
    FprivateKeyMetadata : TCsePrivateKeyMetadataArray;
    FsubjectEmailAddresses : TStringDynArray;
  public
    property disableTime : string read FdisableTime write FdisableTime;
    property enablementState : string read FenablementState write FenablementState;
    property keyPairId : string read FkeyPairId write FkeyPairId;
    property pem : string read Fpem write Fpem;
    property pkcs7 : string read Fpkcs7 write Fpkcs7;
    property privateKeyMetadata : TCsePrivateKeyMetadataArray read FprivateKeyMetadata write FprivateKeyMetadata;
    property subjectEmailAddresses : TStringDynArray read FsubjectEmailAddresses write FsubjectEmailAddresses;
  end;
  
  TDelegate = Class(TObject)
  private
    FdelegateEmail : string;
    FverificationStatus : string;
  public
    property delegateEmail : string read FdelegateEmail write FdelegateEmail;
    property verificationStatus : string read FverificationStatus write FverificationStatus;
  end;
  
  TDisableCseKeyPairRequest = Class(TObject)
  private
  public
  end;
  
  TMessagePartBody = Class(TObject)
  private
    FattachmentId : string;
    Fdata : string;
    Fsize : integer;
  public
    property attachmentId : string read FattachmentId write FattachmentId;
    property data : string read Fdata write Fdata;
    property size : integer read Fsize write Fsize;
  end;
  
  TMessagePartHeader = Class(TObject)
  private
    Fname : string;
    Fvalue : string;
  public
    property name : string read Fname write Fname;
    property value : string read Fvalue write Fvalue;
  end;
  
  TMessagePart = Class(TObject)
  private
    Fbody : TMessagePartBody;
    Ffilename : string;
    Fheaders : TMessagePartHeaderArray;
    FmimeType : string;
    FpartId : string;
    Fparts : TMessagePartArray;
  public
    constructor CreateWithMembers;
    property body : TMessagePartBody read Fbody write Fbody;
    property filename : string read Ffilename write Ffilename;
    property headers : TMessagePartHeaderArray read Fheaders write Fheaders;
    property mimeType : string read FmimeType write FmimeType;
    property partId : string read FpartId write FpartId;
    property parts : TMessagePartArray read Fparts write Fparts;
  end;
  
  TMessage = Class(TObject)
  private
    FclassificationLabelValues : TClassificationLabelValueArray;
    FhistoryId : string;
    Fid : string;
    FinternalDate : string;
    FlabelIds : TStringDynArray;
    Fpayload : TMessagePart;
    Fraw : string;
    FsizeEstimate : integer;
    Fsnippet : string;
    FthreadId : string;
  public
    constructor CreateWithMembers;
    property classificationLabelValues : TClassificationLabelValueArray read FclassificationLabelValues write FclassificationLabelValues;
    property historyId : string read FhistoryId write FhistoryId;
    property id : string read Fid write Fid;
    property internalDate : string read FinternalDate write FinternalDate;
    property labelIds : TStringDynArray read FlabelIds write FlabelIds;
    property payload : TMessagePart read Fpayload write Fpayload;
    property raw : string read Fraw write Fraw;
    property sizeEstimate : integer read FsizeEstimate write FsizeEstimate;
    property snippet : string read Fsnippet write Fsnippet;
    property threadId : string read FthreadId write FthreadId;
  end;
  
  TDraft = Class(TObject)
  private
    Fid : string;
    Fmessage : TMessage;
  public
    constructor CreateWithMembers;
    property id : string read Fid write Fid;
    property message : TMessage read Fmessage write Fmessage;
  end;
  
  TEnableCseKeyPairRequest = Class(TObject)
  private
  public
  end;
  
  TFilterAction = Class(TObject)
  private
    FaddLabelIds : TStringDynArray;
    Fforward : string;
    FremoveLabelIds : TStringDynArray;
  public
    property addLabelIds : TStringDynArray read FaddLabelIds write FaddLabelIds;
    property forward : string read Fforward write Fforward;
    property removeLabelIds : TStringDynArray read FremoveLabelIds write FremoveLabelIds;
  end;
  
  TFilterCriteria = Class(TObject)
  private
    FexcludeChats : boolean;
    Ffrom : string;
    FhasAttachment : boolean;
    FnegatedQuery : string;
    Fquery : string;
    Fsize : integer;
    FsizeComparison : string;
    Fsubject : string;
    Fto_ : string;
  public
    property excludeChats : boolean read FexcludeChats write FexcludeChats;
    property from : string read Ffrom write Ffrom;
    property hasAttachment : boolean read FhasAttachment write FhasAttachment;
    property negatedQuery : string read FnegatedQuery write FnegatedQuery;
    property query : string read Fquery write Fquery;
    property size : integer read Fsize write Fsize;
    property sizeComparison : string read FsizeComparison write FsizeComparison;
    property subject : string read Fsubject write Fsubject;
    property to_ : string read Fto_ write Fto_;
  end;
  
  TFilter = Class(TObject)
  private
    Faction : TFilterAction;
    Fcriteria : TFilterCriteria;
    Fid : string;
  public
    constructor CreateWithMembers;
    property action : TFilterAction read Faction write Faction;
    property criteria : TFilterCriteria read Fcriteria write Fcriteria;
    property id : string read Fid write Fid;
  end;
  
  TForwardingAddress = Class(TObject)
  private
    FforwardingEmail : string;
    FverificationStatus : string;
  public
    property forwardingEmail : string read FforwardingEmail write FforwardingEmail;
    property verificationStatus : string read FverificationStatus write FverificationStatus;
  end;
  
  THistoryLabelAdded = Class(TObject)
  private
    FlabelIds : TStringDynArray;
    Fmessage : TMessage;
  public
    constructor CreateWithMembers;
    property labelIds : TStringDynArray read FlabelIds write FlabelIds;
    property message : TMessage read Fmessage write Fmessage;
  end;
  
  THistoryLabelRemoved = Class(TObject)
  private
    FlabelIds : TStringDynArray;
    Fmessage : TMessage;
  public
    constructor CreateWithMembers;
    property labelIds : TStringDynArray read FlabelIds write FlabelIds;
    property message : TMessage read Fmessage write Fmessage;
  end;
  
  THistoryMessageAdded = Class(TObject)
  private
    Fmessage : TMessage;
  public
    constructor CreateWithMembers;
    property message : TMessage read Fmessage write Fmessage;
  end;
  
  THistoryMessageDeleted = Class(TObject)
  private
    Fmessage : TMessage;
  public
    constructor CreateWithMembers;
    property message : TMessage read Fmessage write Fmessage;
  end;
  
  THistory = Class(TObject)
  private
    Fid : string;
    FlabelsAdded : THistoryLabelAddedArray;
    FlabelsRemoved : THistoryLabelRemovedArray;
    Fmessages : TMessageArray;
    FmessagesAdded : THistoryMessageAddedArray;
    FmessagesDeleted : THistoryMessageDeletedArray;
  public
    property id : string read Fid write Fid;
    property labelsAdded : THistoryLabelAddedArray read FlabelsAdded write FlabelsAdded;
    property labelsRemoved : THistoryLabelRemovedArray read FlabelsRemoved write FlabelsRemoved;
    property messages : TMessageArray read Fmessages write Fmessages;
    property messagesAdded : THistoryMessageAddedArray read FmessagesAdded write FmessagesAdded;
    property messagesDeleted : THistoryMessageDeletedArray read FmessagesDeleted write FmessagesDeleted;
  end;
  
  TImapSettings = Class(TObject)
  private
    FautoExpunge : boolean;
    Fenabled : boolean;
    FexpungeBehavior : string;
    FmaxFolderSize : integer;
  public
    property autoExpunge : boolean read FautoExpunge write FautoExpunge;
    property enabled : boolean read Fenabled write Fenabled;
    property expungeBehavior : string read FexpungeBehavior write FexpungeBehavior;
    property maxFolderSize : integer read FmaxFolderSize write FmaxFolderSize;
  end;
  
  TLabelColor = Class(TObject)
  private
    FbackgroundColor : string;
    FtextColor : string;
  public
    property backgroundColor : string read FbackgroundColor write FbackgroundColor;
    property textColor : string read FtextColor write FtextColor;
  end;
  
  TLabel = Class(TObject)
  private
    Fcolor : TLabelColor;
    Fid : string;
    FlabelListVisibility : string;
    FmessageListVisibility : string;
    FmessagesTotal : integer;
    FmessagesUnread : integer;
    Fname : string;
    FthreadsTotal : integer;
    FthreadsUnread : integer;
    Ftype_ : string;
  public
    constructor CreateWithMembers;
    property color : TLabelColor read Fcolor write Fcolor;
    property id : string read Fid write Fid;
    property labelListVisibility : string read FlabelListVisibility write FlabelListVisibility;
    property messageListVisibility : string read FmessageListVisibility write FmessageListVisibility;
    property messagesTotal : integer read FmessagesTotal write FmessagesTotal;
    property messagesUnread : integer read FmessagesUnread write FmessagesUnread;
    property name : string read Fname write Fname;
    property threadsTotal : integer read FthreadsTotal write FthreadsTotal;
    property threadsUnread : integer read FthreadsUnread write FthreadsUnread;
    property type_ : string read Ftype_ write Ftype_;
  end;
  
  TLanguageSettings = Class(TObject)
  private
    FdisplayLanguage : string;
  public
    property displayLanguage : string read FdisplayLanguage write FdisplayLanguage;
  end;
  
  TListCseIdentitiesResponse = Class(TObject)
  private
    FcseIdentities : TCseIdentityArray;
    FnextPageToken : string;
  public
    property cseIdentities : TCseIdentityArray read FcseIdentities write FcseIdentities;
    property nextPageToken : string read FnextPageToken write FnextPageToken;
  end;
  
  TListCseKeyPairsResponse = Class(TObject)
  private
    FcseKeyPairs : TCseKeyPairArray;
    FnextPageToken : string;
  public
    property cseKeyPairs : TCseKeyPairArray read FcseKeyPairs write FcseKeyPairs;
    property nextPageToken : string read FnextPageToken write FnextPageToken;
  end;
  
  TListDelegatesResponse = Class(TObject)
  private
    Fdelegates : TDelegateArray;
  public
    property delegates : TDelegateArray read Fdelegates write Fdelegates;
  end;
  
  TListDraftsResponse = Class(TObject)
  private
    Fdrafts : TDraftArray;
    FnextPageToken : string;
    FresultSizeEstimate : Cardinal;
  public
    property drafts : TDraftArray read Fdrafts write Fdrafts;
    property nextPageToken : string read FnextPageToken write FnextPageToken;
    property resultSizeEstimate : Cardinal read FresultSizeEstimate write FresultSizeEstimate;
  end;
  
  TListFiltersResponse = Class(TObject)
  private
    Ffilter : TFilterArray;
  public
    property filter : TFilterArray read Ffilter write Ffilter;
  end;
  
  TListForwardingAddressesResponse = Class(TObject)
  private
    FforwardingAddresses : TForwardingAddressArray;
  public
    property forwardingAddresses : TForwardingAddressArray read FforwardingAddresses write FforwardingAddresses;
  end;
  
  TListHistoryResponse = Class(TObject)
  private
    Fhistory : THistoryArray;
    FhistoryId : string;
    FnextPageToken : string;
  public
    property history : THistoryArray read Fhistory write Fhistory;
    property historyId : string read FhistoryId write FhistoryId;
    property nextPageToken : string read FnextPageToken write FnextPageToken;
  end;
  
  TListLabelsResponse = Class(TObject)
  private
    Flabels : TLabelArray;
  public
    property labels : TLabelArray read Flabels write Flabels;
  end;
  
  TListMessagesResponse = Class(TObject)
  private
    Fmessages : TMessageArray;
    FnextPageToken : string;
    FresultSizeEstimate : Cardinal;
  public
    property messages : TMessageArray read Fmessages write Fmessages;
    property nextPageToken : string read FnextPageToken write FnextPageToken;
    property resultSizeEstimate : Cardinal read FresultSizeEstimate write FresultSizeEstimate;
  end;
  
  TSmtpMsa = Class(TObject)
  private
    Fhost : string;
    Fpassword : string;
    Fport : integer;
    FsecurityMode : string;
    Fusername : string;
  public
    property host : string read Fhost write Fhost;
    property password : string read Fpassword write Fpassword;
    property port : integer read Fport write Fport;
    property securityMode : string read FsecurityMode write FsecurityMode;
    property username : string read Fusername write Fusername;
  end;
  
  TSendAs = Class(TObject)
  private
    FdisplayName : string;
    FisDefault : boolean;
    FisPrimary : boolean;
    FreplyToAddress : string;
    FsendAsEmail : string;
    Fsignature : string;
    FsmtpMsa : TSmtpMsa;
    FtreatAsAlias : boolean;
    FverificationStatus : string;
  public
    constructor CreateWithMembers;
    property displayName : string read FdisplayName write FdisplayName;
    property isDefault : boolean read FisDefault write FisDefault;
    property isPrimary : boolean read FisPrimary write FisPrimary;
    property replyToAddress : string read FreplyToAddress write FreplyToAddress;
    property sendAsEmail : string read FsendAsEmail write FsendAsEmail;
    property signature : string read Fsignature write Fsignature;
    property smtpMsa : TSmtpMsa read FsmtpMsa write FsmtpMsa;
    property treatAsAlias : boolean read FtreatAsAlias write FtreatAsAlias;
    property verificationStatus : string read FverificationStatus write FverificationStatus;
  end;
  
  TListSendAsResponse = Class(TObject)
  private
    FsendAs : TSendAsArray;
  public
    property sendAs : TSendAsArray read FsendAs write FsendAs;
  end;
  
  TSmimeInfo = Class(TObject)
  private
    FencryptedKeyPassword : string;
    Fexpiration : string;
    Fid : string;
    FisDefault : boolean;
    FissuerCn : string;
    Fpem : string;
    Fpkcs12 : string;
  public
    property encryptedKeyPassword : string read FencryptedKeyPassword write FencryptedKeyPassword;
    property expiration : string read Fexpiration write Fexpiration;
    property id : string read Fid write Fid;
    property isDefault : boolean read FisDefault write FisDefault;
    property issuerCn : string read FissuerCn write FissuerCn;
    property pem : string read Fpem write Fpem;
    property pkcs12 : string read Fpkcs12 write Fpkcs12;
  end;
  
  TListSmimeInfoResponse = Class(TObject)
  private
    FsmimeInfo : TSmimeInfoArray;
  public
    property smimeInfo : TSmimeInfoArray read FsmimeInfo write FsmimeInfo;
  end;
  
  TThread_ = Class(TObject)
  private
    FhistoryId : string;
    Fid : string;
    Fmessages : TMessageArray;
    Fsnippet : string;
  public
    property historyId : string read FhistoryId write FhistoryId;
    property id : string read Fid write Fid;
    property messages : TMessageArray read Fmessages write Fmessages;
    property snippet : string read Fsnippet write Fsnippet;
  end;
  
  TListThreadsResponse = Class(TObject)
  private
    FnextPageToken : string;
    FresultSizeEstimate : Cardinal;
    Fthreads : TThread_Array;
  public
    property nextPageToken : string read FnextPageToken write FnextPageToken;
    property resultSizeEstimate : Cardinal read FresultSizeEstimate write FresultSizeEstimate;
    property threads : TThread_Array read Fthreads write Fthreads;
  end;
  
  TModifyMessageRequest = Class(TObject)
  private
    FaddClassificationLabels : TClassificationLabelValueArray;
    FaddLabelIds : TStringDynArray;
    FremoveClassificationLabelIds : TStringDynArray;
    FremoveLabelIds : TStringDynArray;
  public
    property addClassificationLabels : TClassificationLabelValueArray read FaddClassificationLabels write FaddClassificationLabels;
    property addLabelIds : TStringDynArray read FaddLabelIds write FaddLabelIds;
    property removeClassificationLabelIds : TStringDynArray read FremoveClassificationLabelIds write FremoveClassificationLabelIds;
    property removeLabelIds : TStringDynArray read FremoveLabelIds write FremoveLabelIds;
  end;
  
  TModifyThreadRequest = Class(TObject)
  private
    FaddLabelIds : TStringDynArray;
    FremoveLabelIds : TStringDynArray;
  public
    property addLabelIds : TStringDynArray read FaddLabelIds write FaddLabelIds;
    property removeLabelIds : TStringDynArray read FremoveLabelIds write FremoveLabelIds;
  end;
  
  TObliterateCseKeyPairRequest = Class(TObject)
  private
  public
  end;
  
  TPopSettings = Class(TObject)
  private
    FaccessWindow : string;
    Fdisposition : string;
  public
    property accessWindow : string read FaccessWindow write FaccessWindow;
    property disposition : string read Fdisposition write Fdisposition;
  end;
  
  TProfile = Class(TObject)
  private
    FemailAddress : string;
    FhistoryId : string;
    FmessagesTotal : integer;
    FthreadsTotal : integer;
  public
    property emailAddress : string read FemailAddress write FemailAddress;
    property historyId : string read FhistoryId write FhistoryId;
    property messagesTotal : integer read FmessagesTotal write FmessagesTotal;
    property threadsTotal : integer read FthreadsTotal write FthreadsTotal;
  end;
  
  TVacationSettings = Class(TObject)
  private
    FenableAutoReply : boolean;
    FendTime : string;
    FresponseBodyHtml : string;
    FresponseBodyPlainText : string;
    FresponseSubject : string;
    FrestrictToContacts : boolean;
    FrestrictToDomain : boolean;
    FstartTime : string;
  public
    property enableAutoReply : boolean read FenableAutoReply write FenableAutoReply;
    property endTime : string read FendTime write FendTime;
    property responseBodyHtml : string read FresponseBodyHtml write FresponseBodyHtml;
    property responseBodyPlainText : string read FresponseBodyPlainText write FresponseBodyPlainText;
    property responseSubject : string read FresponseSubject write FresponseSubject;
    property restrictToContacts : boolean read FrestrictToContacts write FrestrictToContacts;
    property restrictToDomain : boolean read FrestrictToDomain write FrestrictToDomain;
    property startTime : string read FstartTime write FstartTime;
  end;
  
  TWatchRequest = Class(TObject)
  private
    FlabelFilterAction : string;
    FlabelFilterBehavior : string;
    FlabelIds : TStringDynArray;
    FtopicName : string;
  public
    property labelFilterAction : string read FlabelFilterAction write FlabelFilterAction;
    property labelFilterBehavior : string read FlabelFilterBehavior write FlabelFilterBehavior;
    property labelIds : TStringDynArray read FlabelIds write FlabelIds;
    property topicName : string read FtopicName write FtopicName;
  end;
  
  TWatchResponse = Class(TObject)
  private
    Fexpiration : string;
    FhistoryId : string;
  public
    property expiration : string read Fexpiration write Fexpiration;
    property historyId : string read FhistoryId write FhistoryId;
  end;
  
implementation

constructor TCseIdentity.CreateWithMembers;

begin
  FsignAndEncryptKeyPairs := TSignAndEncryptKeyPairs.Create;
end;

constructor TCsePrivateKeyMetadata.CreateWithMembers;

begin
  FhardwareKeyMetadata := THardwareKeyMetadata.Create;
  FkaclsKeyMetadata := TKaclsKeyMetadata.Create;
end;

constructor TMessagePart.CreateWithMembers;

begin
  Fbody := TMessagePartBody.Create;
end;

constructor TMessage.CreateWithMembers;

begin
  Fpayload := TMessagePart.CreateWithMembers;
end;

constructor TDraft.CreateWithMembers;

begin
  Fmessage := TMessage.CreateWithMembers;
end;

constructor TFilter.CreateWithMembers;

begin
  Faction := TFilterAction.Create;
  Fcriteria := TFilterCriteria.Create;
end;

constructor THistoryLabelAdded.CreateWithMembers;

begin
  Fmessage := TMessage.CreateWithMembers;
end;

constructor THistoryLabelRemoved.CreateWithMembers;

begin
  Fmessage := TMessage.CreateWithMembers;
end;

constructor THistoryMessageAdded.CreateWithMembers;

begin
  Fmessage := TMessage.CreateWithMembers;
end;

constructor THistoryMessageDeleted.CreateWithMembers;

begin
  Fmessage := TMessage.CreateWithMembers;
end;

constructor TLabel.CreateWithMembers;

begin
  Fcolor := TLabelColor.Create;
end;

constructor TSendAs.CreateWithMembers;

begin
  FsmtpMsa := TSmtpMsa.Create;
end;

end.
