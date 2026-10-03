{ -----------------------------------------------------------------------
  Do not edit !
  
  This file was automatically generated on 2026-10-03 18:11.
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
    procedure Setfields(const aValue : TClassificationLabelFieldValueArray);
  public
    destructor Destroy; override;
    property fields : TClassificationLabelFieldValueArray read Ffields write Setfields;
    property labelId : string read FlabelId write FlabelId;
  end;
  
  TBatchModifyMessagesRequest = Class(TObject)
  private
    FaddClassificationLabels : TClassificationLabelValueArray;
    FaddLabelIds : TStringDynArray;
    Fids : TStringDynArray;
    FremoveClassificationLabelIds : TStringDynArray;
    FremoveLabelIds : TStringDynArray;
    procedure SetaddClassificationLabels(const aValue : TClassificationLabelValueArray);
  public
    destructor Destroy; override;
    property addClassificationLabels : TClassificationLabelValueArray read FaddClassificationLabels write SetaddClassificationLabels;
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
    procedure SetsignAndEncryptKeyPairs(const aValue : TSignAndEncryptKeyPairs);
  public
    constructor CreateWithMembers;
    destructor Destroy; override;
    property emailAddress : string read FemailAddress write FemailAddress;
    property primaryKeyPairId : string read FprimaryKeyPairId write FprimaryKeyPairId;
    property signAndEncryptKeyPairs : TSignAndEncryptKeyPairs read FsignAndEncryptKeyPairs write SetsignAndEncryptKeyPairs;
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
    procedure SethardwareKeyMetadata(const aValue : THardwareKeyMetadata);
    procedure SetkaclsKeyMetadata(const aValue : TKaclsKeyMetadata);
  public
    constructor CreateWithMembers;
    destructor Destroy; override;
    property hardwareKeyMetadata : THardwareKeyMetadata read FhardwareKeyMetadata write SethardwareKeyMetadata;
    property kaclsKeyMetadata : TKaclsKeyMetadata read FkaclsKeyMetadata write SetkaclsKeyMetadata;
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
    procedure SetprivateKeyMetadata(const aValue : TCsePrivateKeyMetadataArray);
  public
    destructor Destroy; override;
    property disableTime : string read FdisableTime write FdisableTime;
    property enablementState : string read FenablementState write FenablementState;
    property keyPairId : string read FkeyPairId write FkeyPairId;
    property pem : string read Fpem write Fpem;
    property pkcs7 : string read Fpkcs7 write Fpkcs7;
    property privateKeyMetadata : TCsePrivateKeyMetadataArray read FprivateKeyMetadata write SetprivateKeyMetadata;
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
    procedure Setbody(const aValue : TMessagePartBody);
    procedure Setheaders(const aValue : TMessagePartHeaderArray);
    procedure Setparts(const aValue : TMessagePartArray);
  public
    constructor CreateWithMembers;
    destructor Destroy; override;
    property body : TMessagePartBody read Fbody write Setbody;
    property filename : string read Ffilename write Ffilename;
    property headers : TMessagePartHeaderArray read Fheaders write Setheaders;
    property mimeType : string read FmimeType write FmimeType;
    property partId : string read FpartId write FpartId;
    property parts : TMessagePartArray read Fparts write Setparts;
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
    procedure SetclassificationLabelValues(const aValue : TClassificationLabelValueArray);
    procedure Setpayload(const aValue : TMessagePart);
  public
    constructor CreateWithMembers;
    destructor Destroy; override;
    property classificationLabelValues : TClassificationLabelValueArray read FclassificationLabelValues write SetclassificationLabelValues;
    property historyId : string read FhistoryId write FhistoryId;
    property id : string read Fid write Fid;
    property internalDate : string read FinternalDate write FinternalDate;
    property labelIds : TStringDynArray read FlabelIds write FlabelIds;
    property payload : TMessagePart read Fpayload write Setpayload;
    property raw : string read Fraw write Fraw;
    property sizeEstimate : integer read FsizeEstimate write FsizeEstimate;
    property snippet : string read Fsnippet write Fsnippet;
    property threadId : string read FthreadId write FthreadId;
  end;
  
  TDraft = Class(TObject)
  private
    Fid : string;
    Fmessage : TMessage;
    procedure Setmessage(const aValue : TMessage);
  public
    constructor CreateWithMembers;
    destructor Destroy; override;
    property id : string read Fid write Fid;
    property message : TMessage read Fmessage write Setmessage;
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
    procedure Setaction(const aValue : TFilterAction);
    procedure Setcriteria(const aValue : TFilterCriteria);
  public
    constructor CreateWithMembers;
    destructor Destroy; override;
    property action : TFilterAction read Faction write Setaction;
    property criteria : TFilterCriteria read Fcriteria write Setcriteria;
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
    procedure Setmessage(const aValue : TMessage);
  public
    constructor CreateWithMembers;
    destructor Destroy; override;
    property labelIds : TStringDynArray read FlabelIds write FlabelIds;
    property message : TMessage read Fmessage write Setmessage;
  end;
  
  THistoryLabelRemoved = Class(TObject)
  private
    FlabelIds : TStringDynArray;
    Fmessage : TMessage;
    procedure Setmessage(const aValue : TMessage);
  public
    constructor CreateWithMembers;
    destructor Destroy; override;
    property labelIds : TStringDynArray read FlabelIds write FlabelIds;
    property message : TMessage read Fmessage write Setmessage;
  end;
  
  THistoryMessageAdded = Class(TObject)
  private
    Fmessage : TMessage;
    procedure Setmessage(const aValue : TMessage);
  public
    constructor CreateWithMembers;
    destructor Destroy; override;
    property message : TMessage read Fmessage write Setmessage;
  end;
  
  THistoryMessageDeleted = Class(TObject)
  private
    Fmessage : TMessage;
    procedure Setmessage(const aValue : TMessage);
  public
    constructor CreateWithMembers;
    destructor Destroy; override;
    property message : TMessage read Fmessage write Setmessage;
  end;
  
  THistory = Class(TObject)
  private
    Fid : string;
    FlabelsAdded : THistoryLabelAddedArray;
    FlabelsRemoved : THistoryLabelRemovedArray;
    Fmessages : TMessageArray;
    FmessagesAdded : THistoryMessageAddedArray;
    FmessagesDeleted : THistoryMessageDeletedArray;
    procedure SetlabelsAdded(const aValue : THistoryLabelAddedArray);
    procedure SetlabelsRemoved(const aValue : THistoryLabelRemovedArray);
    procedure Setmessages(const aValue : TMessageArray);
    procedure SetmessagesAdded(const aValue : THistoryMessageAddedArray);
    procedure SetmessagesDeleted(const aValue : THistoryMessageDeletedArray);
  public
    destructor Destroy; override;
    property id : string read Fid write Fid;
    property labelsAdded : THistoryLabelAddedArray read FlabelsAdded write SetlabelsAdded;
    property labelsRemoved : THistoryLabelRemovedArray read FlabelsRemoved write SetlabelsRemoved;
    property messages : TMessageArray read Fmessages write Setmessages;
    property messagesAdded : THistoryMessageAddedArray read FmessagesAdded write SetmessagesAdded;
    property messagesDeleted : THistoryMessageDeletedArray read FmessagesDeleted write SetmessagesDeleted;
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
    procedure Setcolor(const aValue : TLabelColor);
  public
    constructor CreateWithMembers;
    destructor Destroy; override;
    property color : TLabelColor read Fcolor write Setcolor;
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
    procedure SetcseIdentities(const aValue : TCseIdentityArray);
  public
    destructor Destroy; override;
    property cseIdentities : TCseIdentityArray read FcseIdentities write SetcseIdentities;
    property nextPageToken : string read FnextPageToken write FnextPageToken;
  end;
  
  TListCseKeyPairsResponse = Class(TObject)
  private
    FcseKeyPairs : TCseKeyPairArray;
    FnextPageToken : string;
    procedure SetcseKeyPairs(const aValue : TCseKeyPairArray);
  public
    destructor Destroy; override;
    property cseKeyPairs : TCseKeyPairArray read FcseKeyPairs write SetcseKeyPairs;
    property nextPageToken : string read FnextPageToken write FnextPageToken;
  end;
  
  TListDelegatesResponse = Class(TObject)
  private
    Fdelegates : TDelegateArray;
    procedure Setdelegates(const aValue : TDelegateArray);
  public
    destructor Destroy; override;
    property delegates : TDelegateArray read Fdelegates write Setdelegates;
  end;
  
  TListDraftsResponse = Class(TObject)
  private
    Fdrafts : TDraftArray;
    FnextPageToken : string;
    FresultSizeEstimate : Cardinal;
    procedure Setdrafts(const aValue : TDraftArray);
  public
    destructor Destroy; override;
    property drafts : TDraftArray read Fdrafts write Setdrafts;
    property nextPageToken : string read FnextPageToken write FnextPageToken;
    property resultSizeEstimate : Cardinal read FresultSizeEstimate write FresultSizeEstimate;
  end;
  
  TListFiltersResponse = Class(TObject)
  private
    Ffilter : TFilterArray;
    procedure Setfilter(const aValue : TFilterArray);
  public
    destructor Destroy; override;
    property filter : TFilterArray read Ffilter write Setfilter;
  end;
  
  TListForwardingAddressesResponse = Class(TObject)
  private
    FforwardingAddresses : TForwardingAddressArray;
    procedure SetforwardingAddresses(const aValue : TForwardingAddressArray);
  public
    destructor Destroy; override;
    property forwardingAddresses : TForwardingAddressArray read FforwardingAddresses write SetforwardingAddresses;
  end;
  
  TListHistoryResponse = Class(TObject)
  private
    Fhistory : THistoryArray;
    FhistoryId : string;
    FnextPageToken : string;
    procedure Sethistory(const aValue : THistoryArray);
  public
    destructor Destroy; override;
    property history : THistoryArray read Fhistory write Sethistory;
    property historyId : string read FhistoryId write FhistoryId;
    property nextPageToken : string read FnextPageToken write FnextPageToken;
  end;
  
  TListLabelsResponse = Class(TObject)
  private
    Flabels : TLabelArray;
    procedure Setlabels(const aValue : TLabelArray);
  public
    destructor Destroy; override;
    property labels : TLabelArray read Flabels write Setlabels;
  end;
  
  TListMessagesResponse = Class(TObject)
  private
    Fmessages : TMessageArray;
    FnextPageToken : string;
    FresultSizeEstimate : Cardinal;
    procedure Setmessages(const aValue : TMessageArray);
  public
    destructor Destroy; override;
    property messages : TMessageArray read Fmessages write Setmessages;
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
    procedure SetsmtpMsa(const aValue : TSmtpMsa);
  public
    constructor CreateWithMembers;
    destructor Destroy; override;
    property displayName : string read FdisplayName write FdisplayName;
    property isDefault : boolean read FisDefault write FisDefault;
    property isPrimary : boolean read FisPrimary write FisPrimary;
    property replyToAddress : string read FreplyToAddress write FreplyToAddress;
    property sendAsEmail : string read FsendAsEmail write FsendAsEmail;
    property signature : string read Fsignature write Fsignature;
    property smtpMsa : TSmtpMsa read FsmtpMsa write SetsmtpMsa;
    property treatAsAlias : boolean read FtreatAsAlias write FtreatAsAlias;
    property verificationStatus : string read FverificationStatus write FverificationStatus;
  end;
  
  TListSendAsResponse = Class(TObject)
  private
    FsendAs : TSendAsArray;
    procedure SetsendAs(const aValue : TSendAsArray);
  public
    destructor Destroy; override;
    property sendAs : TSendAsArray read FsendAs write SetsendAs;
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
    procedure SetsmimeInfo(const aValue : TSmimeInfoArray);
  public
    destructor Destroy; override;
    property smimeInfo : TSmimeInfoArray read FsmimeInfo write SetsmimeInfo;
  end;
  
  TThread_ = Class(TObject)
  private
    FhistoryId : string;
    Fid : string;
    Fmessages : TMessageArray;
    Fsnippet : string;
    procedure Setmessages(const aValue : TMessageArray);
  public
    destructor Destroy; override;
    property historyId : string read FhistoryId write FhistoryId;
    property id : string read Fid write Fid;
    property messages : TMessageArray read Fmessages write Setmessages;
    property snippet : string read Fsnippet write Fsnippet;
  end;
  
  TListThreadsResponse = Class(TObject)
  private
    FnextPageToken : string;
    FresultSizeEstimate : Cardinal;
    Fthreads : TThread_Array;
    procedure Setthreads(const aValue : TThread_Array);
  public
    destructor Destroy; override;
    property nextPageToken : string read FnextPageToken write FnextPageToken;
    property resultSizeEstimate : Cardinal read FresultSizeEstimate write FresultSizeEstimate;
    property threads : TThread_Array read Fthreads write Setthreads;
  end;
  
  TModifyMessageRequest = Class(TObject)
  private
    FaddClassificationLabels : TClassificationLabelValueArray;
    FaddLabelIds : TStringDynArray;
    FremoveClassificationLabelIds : TStringDynArray;
    FremoveLabelIds : TStringDynArray;
    procedure SetaddClassificationLabels(const aValue : TClassificationLabelValueArray);
  public
    destructor Destroy; override;
    property addClassificationLabels : TClassificationLabelValueArray read FaddClassificationLabels write SetaddClassificationLabels;
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

destructor TClassificationLabelValue.Destroy;

var
  lI : Integer;

begin
  for lI:=0 to Length(Ffields)-1 do
    Ffields[lI].Free;
  inherited Destroy;
end;

procedure TClassificationLabelValue.Setfields(const aValue : TClassificationLabelFieldValueArray);

var
  lI, lJ : Integer;
  lKeep : Boolean;

begin
  // Free the objects that are not in the new array
  for lI:=0 to Length(Ffields)-1 do
    begin
    lKeep:=False;
    for lJ:=0 to Length(aValue)-1 do
      if aValue[lJ]=Ffields[lI] then
        lKeep:=True;
    if not lKeep then
      Ffields[lI].Free;
    end;
  Ffields:=aValue;
end;

destructor TBatchModifyMessagesRequest.Destroy;

var
  lI : Integer;

begin
  for lI:=0 to Length(FaddClassificationLabels)-1 do
    FaddClassificationLabels[lI].Free;
  inherited Destroy;
end;

procedure TBatchModifyMessagesRequest.SetaddClassificationLabels(const aValue : TClassificationLabelValueArray);

var
  lI, lJ : Integer;
  lKeep : Boolean;

begin
  // Free the objects that are not in the new array
  for lI:=0 to Length(FaddClassificationLabels)-1 do
    begin
    lKeep:=False;
    for lJ:=0 to Length(aValue)-1 do
      if aValue[lJ]=FaddClassificationLabels[lI] then
        lKeep:=True;
    if not lKeep then
      FaddClassificationLabels[lI].Free;
    end;
  FaddClassificationLabels:=aValue;
end;

constructor TCseIdentity.CreateWithMembers;

begin
  FsignAndEncryptKeyPairs := TSignAndEncryptKeyPairs.Create;
end;

destructor TCseIdentity.Destroy;

begin
  FsignAndEncryptKeyPairs.Free;
  inherited Destroy;
end;

procedure TCseIdentity.SetsignAndEncryptKeyPairs(const aValue : TSignAndEncryptKeyPairs);

begin
  if FsignAndEncryptKeyPairs<>aValue then
    FsignAndEncryptKeyPairs.Free;
  FsignAndEncryptKeyPairs:=aValue;
end;

constructor TCsePrivateKeyMetadata.CreateWithMembers;

begin
  FhardwareKeyMetadata := THardwareKeyMetadata.Create;
  FkaclsKeyMetadata := TKaclsKeyMetadata.Create;
end;

destructor TCsePrivateKeyMetadata.Destroy;

begin
  FhardwareKeyMetadata.Free;
  FkaclsKeyMetadata.Free;
  inherited Destroy;
end;

procedure TCsePrivateKeyMetadata.SethardwareKeyMetadata(const aValue : THardwareKeyMetadata);

begin
  if FhardwareKeyMetadata<>aValue then
    FhardwareKeyMetadata.Free;
  FhardwareKeyMetadata:=aValue;
end;

procedure TCsePrivateKeyMetadata.SetkaclsKeyMetadata(const aValue : TKaclsKeyMetadata);

begin
  if FkaclsKeyMetadata<>aValue then
    FkaclsKeyMetadata.Free;
  FkaclsKeyMetadata:=aValue;
end;

destructor TCseKeyPair.Destroy;

var
  lI : Integer;

begin
  for lI:=0 to Length(FprivateKeyMetadata)-1 do
    FprivateKeyMetadata[lI].Free;
  inherited Destroy;
end;

procedure TCseKeyPair.SetprivateKeyMetadata(const aValue : TCsePrivateKeyMetadataArray);

var
  lI, lJ : Integer;
  lKeep : Boolean;

begin
  // Free the objects that are not in the new array
  for lI:=0 to Length(FprivateKeyMetadata)-1 do
    begin
    lKeep:=False;
    for lJ:=0 to Length(aValue)-1 do
      if aValue[lJ]=FprivateKeyMetadata[lI] then
        lKeep:=True;
    if not lKeep then
      FprivateKeyMetadata[lI].Free;
    end;
  FprivateKeyMetadata:=aValue;
end;

constructor TMessagePart.CreateWithMembers;

begin
  Fbody := TMessagePartBody.Create;
end;

destructor TMessagePart.Destroy;

var
  lI : Integer;

begin
  Fbody.Free;
  for lI:=0 to Length(Fheaders)-1 do
    Fheaders[lI].Free;
  for lI:=0 to Length(Fparts)-1 do
    Fparts[lI].Free;
  inherited Destroy;
end;

procedure TMessagePart.Setbody(const aValue : TMessagePartBody);

begin
  if Fbody<>aValue then
    Fbody.Free;
  Fbody:=aValue;
end;

procedure TMessagePart.Setheaders(const aValue : TMessagePartHeaderArray);

var
  lI, lJ : Integer;
  lKeep : Boolean;

begin
  // Free the objects that are not in the new array
  for lI:=0 to Length(Fheaders)-1 do
    begin
    lKeep:=False;
    for lJ:=0 to Length(aValue)-1 do
      if aValue[lJ]=Fheaders[lI] then
        lKeep:=True;
    if not lKeep then
      Fheaders[lI].Free;
    end;
  Fheaders:=aValue;
end;

procedure TMessagePart.Setparts(const aValue : TMessagePartArray);

var
  lI, lJ : Integer;
  lKeep : Boolean;

begin
  // Free the objects that are not in the new array
  for lI:=0 to Length(Fparts)-1 do
    begin
    lKeep:=False;
    for lJ:=0 to Length(aValue)-1 do
      if aValue[lJ]=Fparts[lI] then
        lKeep:=True;
    if not lKeep then
      Fparts[lI].Free;
    end;
  Fparts:=aValue;
end;

constructor TMessage.CreateWithMembers;

begin
  Fpayload := TMessagePart.CreateWithMembers;
end;

destructor TMessage.Destroy;

var
  lI : Integer;

begin
  for lI:=0 to Length(FclassificationLabelValues)-1 do
    FclassificationLabelValues[lI].Free;
  Fpayload.Free;
  inherited Destroy;
end;

procedure TMessage.SetclassificationLabelValues(const aValue : TClassificationLabelValueArray);

var
  lI, lJ : Integer;
  lKeep : Boolean;

begin
  // Free the objects that are not in the new array
  for lI:=0 to Length(FclassificationLabelValues)-1 do
    begin
    lKeep:=False;
    for lJ:=0 to Length(aValue)-1 do
      if aValue[lJ]=FclassificationLabelValues[lI] then
        lKeep:=True;
    if not lKeep then
      FclassificationLabelValues[lI].Free;
    end;
  FclassificationLabelValues:=aValue;
end;

procedure TMessage.Setpayload(const aValue : TMessagePart);

begin
  if Fpayload<>aValue then
    Fpayload.Free;
  Fpayload:=aValue;
end;

constructor TDraft.CreateWithMembers;

begin
  Fmessage := TMessage.CreateWithMembers;
end;

destructor TDraft.Destroy;

begin
  Fmessage.Free;
  inherited Destroy;
end;

procedure TDraft.Setmessage(const aValue : TMessage);

begin
  if Fmessage<>aValue then
    Fmessage.Free;
  Fmessage:=aValue;
end;

constructor TFilter.CreateWithMembers;

begin
  Faction := TFilterAction.Create;
  Fcriteria := TFilterCriteria.Create;
end;

destructor TFilter.Destroy;

begin
  Faction.Free;
  Fcriteria.Free;
  inherited Destroy;
end;

procedure TFilter.Setaction(const aValue : TFilterAction);

begin
  if Faction<>aValue then
    Faction.Free;
  Faction:=aValue;
end;

procedure TFilter.Setcriteria(const aValue : TFilterCriteria);

begin
  if Fcriteria<>aValue then
    Fcriteria.Free;
  Fcriteria:=aValue;
end;

constructor THistoryLabelAdded.CreateWithMembers;

begin
  Fmessage := TMessage.CreateWithMembers;
end;

destructor THistoryLabelAdded.Destroy;

begin
  Fmessage.Free;
  inherited Destroy;
end;

procedure THistoryLabelAdded.Setmessage(const aValue : TMessage);

begin
  if Fmessage<>aValue then
    Fmessage.Free;
  Fmessage:=aValue;
end;

constructor THistoryLabelRemoved.CreateWithMembers;

begin
  Fmessage := TMessage.CreateWithMembers;
end;

destructor THistoryLabelRemoved.Destroy;

begin
  Fmessage.Free;
  inherited Destroy;
end;

procedure THistoryLabelRemoved.Setmessage(const aValue : TMessage);

begin
  if Fmessage<>aValue then
    Fmessage.Free;
  Fmessage:=aValue;
end;

constructor THistoryMessageAdded.CreateWithMembers;

begin
  Fmessage := TMessage.CreateWithMembers;
end;

destructor THistoryMessageAdded.Destroy;

begin
  Fmessage.Free;
  inherited Destroy;
end;

procedure THistoryMessageAdded.Setmessage(const aValue : TMessage);

begin
  if Fmessage<>aValue then
    Fmessage.Free;
  Fmessage:=aValue;
end;

constructor THistoryMessageDeleted.CreateWithMembers;

begin
  Fmessage := TMessage.CreateWithMembers;
end;

destructor THistoryMessageDeleted.Destroy;

begin
  Fmessage.Free;
  inherited Destroy;
end;

procedure THistoryMessageDeleted.Setmessage(const aValue : TMessage);

begin
  if Fmessage<>aValue then
    Fmessage.Free;
  Fmessage:=aValue;
end;

destructor THistory.Destroy;

var
  lI : Integer;

begin
  for lI:=0 to Length(FlabelsAdded)-1 do
    FlabelsAdded[lI].Free;
  for lI:=0 to Length(FlabelsRemoved)-1 do
    FlabelsRemoved[lI].Free;
  for lI:=0 to Length(Fmessages)-1 do
    Fmessages[lI].Free;
  for lI:=0 to Length(FmessagesAdded)-1 do
    FmessagesAdded[lI].Free;
  for lI:=0 to Length(FmessagesDeleted)-1 do
    FmessagesDeleted[lI].Free;
  inherited Destroy;
end;

procedure THistory.SetlabelsAdded(const aValue : THistoryLabelAddedArray);

var
  lI, lJ : Integer;
  lKeep : Boolean;

begin
  // Free the objects that are not in the new array
  for lI:=0 to Length(FlabelsAdded)-1 do
    begin
    lKeep:=False;
    for lJ:=0 to Length(aValue)-1 do
      if aValue[lJ]=FlabelsAdded[lI] then
        lKeep:=True;
    if not lKeep then
      FlabelsAdded[lI].Free;
    end;
  FlabelsAdded:=aValue;
end;

procedure THistory.SetlabelsRemoved(const aValue : THistoryLabelRemovedArray);

var
  lI, lJ : Integer;
  lKeep : Boolean;

begin
  // Free the objects that are not in the new array
  for lI:=0 to Length(FlabelsRemoved)-1 do
    begin
    lKeep:=False;
    for lJ:=0 to Length(aValue)-1 do
      if aValue[lJ]=FlabelsRemoved[lI] then
        lKeep:=True;
    if not lKeep then
      FlabelsRemoved[lI].Free;
    end;
  FlabelsRemoved:=aValue;
end;

procedure THistory.Setmessages(const aValue : TMessageArray);

var
  lI, lJ : Integer;
  lKeep : Boolean;

begin
  // Free the objects that are not in the new array
  for lI:=0 to Length(Fmessages)-1 do
    begin
    lKeep:=False;
    for lJ:=0 to Length(aValue)-1 do
      if aValue[lJ]=Fmessages[lI] then
        lKeep:=True;
    if not lKeep then
      Fmessages[lI].Free;
    end;
  Fmessages:=aValue;
end;

procedure THistory.SetmessagesAdded(const aValue : THistoryMessageAddedArray);

var
  lI, lJ : Integer;
  lKeep : Boolean;

begin
  // Free the objects that are not in the new array
  for lI:=0 to Length(FmessagesAdded)-1 do
    begin
    lKeep:=False;
    for lJ:=0 to Length(aValue)-1 do
      if aValue[lJ]=FmessagesAdded[lI] then
        lKeep:=True;
    if not lKeep then
      FmessagesAdded[lI].Free;
    end;
  FmessagesAdded:=aValue;
end;

procedure THistory.SetmessagesDeleted(const aValue : THistoryMessageDeletedArray);

var
  lI, lJ : Integer;
  lKeep : Boolean;

begin
  // Free the objects that are not in the new array
  for lI:=0 to Length(FmessagesDeleted)-1 do
    begin
    lKeep:=False;
    for lJ:=0 to Length(aValue)-1 do
      if aValue[lJ]=FmessagesDeleted[lI] then
        lKeep:=True;
    if not lKeep then
      FmessagesDeleted[lI].Free;
    end;
  FmessagesDeleted:=aValue;
end;

constructor TLabel.CreateWithMembers;

begin
  Fcolor := TLabelColor.Create;
end;

destructor TLabel.Destroy;

begin
  Fcolor.Free;
  inherited Destroy;
end;

procedure TLabel.Setcolor(const aValue : TLabelColor);

begin
  if Fcolor<>aValue then
    Fcolor.Free;
  Fcolor:=aValue;
end;

destructor TListCseIdentitiesResponse.Destroy;

var
  lI : Integer;

begin
  for lI:=0 to Length(FcseIdentities)-1 do
    FcseIdentities[lI].Free;
  inherited Destroy;
end;

procedure TListCseIdentitiesResponse.SetcseIdentities(const aValue : TCseIdentityArray);

var
  lI, lJ : Integer;
  lKeep : Boolean;

begin
  // Free the objects that are not in the new array
  for lI:=0 to Length(FcseIdentities)-1 do
    begin
    lKeep:=False;
    for lJ:=0 to Length(aValue)-1 do
      if aValue[lJ]=FcseIdentities[lI] then
        lKeep:=True;
    if not lKeep then
      FcseIdentities[lI].Free;
    end;
  FcseIdentities:=aValue;
end;

destructor TListCseKeyPairsResponse.Destroy;

var
  lI : Integer;

begin
  for lI:=0 to Length(FcseKeyPairs)-1 do
    FcseKeyPairs[lI].Free;
  inherited Destroy;
end;

procedure TListCseKeyPairsResponse.SetcseKeyPairs(const aValue : TCseKeyPairArray);

var
  lI, lJ : Integer;
  lKeep : Boolean;

begin
  // Free the objects that are not in the new array
  for lI:=0 to Length(FcseKeyPairs)-1 do
    begin
    lKeep:=False;
    for lJ:=0 to Length(aValue)-1 do
      if aValue[lJ]=FcseKeyPairs[lI] then
        lKeep:=True;
    if not lKeep then
      FcseKeyPairs[lI].Free;
    end;
  FcseKeyPairs:=aValue;
end;

destructor TListDelegatesResponse.Destroy;

var
  lI : Integer;

begin
  for lI:=0 to Length(Fdelegates)-1 do
    Fdelegates[lI].Free;
  inherited Destroy;
end;

procedure TListDelegatesResponse.Setdelegates(const aValue : TDelegateArray);

var
  lI, lJ : Integer;
  lKeep : Boolean;

begin
  // Free the objects that are not in the new array
  for lI:=0 to Length(Fdelegates)-1 do
    begin
    lKeep:=False;
    for lJ:=0 to Length(aValue)-1 do
      if aValue[lJ]=Fdelegates[lI] then
        lKeep:=True;
    if not lKeep then
      Fdelegates[lI].Free;
    end;
  Fdelegates:=aValue;
end;

destructor TListDraftsResponse.Destroy;

var
  lI : Integer;

begin
  for lI:=0 to Length(Fdrafts)-1 do
    Fdrafts[lI].Free;
  inherited Destroy;
end;

procedure TListDraftsResponse.Setdrafts(const aValue : TDraftArray);

var
  lI, lJ : Integer;
  lKeep : Boolean;

begin
  // Free the objects that are not in the new array
  for lI:=0 to Length(Fdrafts)-1 do
    begin
    lKeep:=False;
    for lJ:=0 to Length(aValue)-1 do
      if aValue[lJ]=Fdrafts[lI] then
        lKeep:=True;
    if not lKeep then
      Fdrafts[lI].Free;
    end;
  Fdrafts:=aValue;
end;

destructor TListFiltersResponse.Destroy;

var
  lI : Integer;

begin
  for lI:=0 to Length(Ffilter)-1 do
    Ffilter[lI].Free;
  inherited Destroy;
end;

procedure TListFiltersResponse.Setfilter(const aValue : TFilterArray);

var
  lI, lJ : Integer;
  lKeep : Boolean;

begin
  // Free the objects that are not in the new array
  for lI:=0 to Length(Ffilter)-1 do
    begin
    lKeep:=False;
    for lJ:=0 to Length(aValue)-1 do
      if aValue[lJ]=Ffilter[lI] then
        lKeep:=True;
    if not lKeep then
      Ffilter[lI].Free;
    end;
  Ffilter:=aValue;
end;

destructor TListForwardingAddressesResponse.Destroy;

var
  lI : Integer;

begin
  for lI:=0 to Length(FforwardingAddresses)-1 do
    FforwardingAddresses[lI].Free;
  inherited Destroy;
end;

procedure TListForwardingAddressesResponse.SetforwardingAddresses(const aValue : TForwardingAddressArray);

var
  lI, lJ : Integer;
  lKeep : Boolean;

begin
  // Free the objects that are not in the new array
  for lI:=0 to Length(FforwardingAddresses)-1 do
    begin
    lKeep:=False;
    for lJ:=0 to Length(aValue)-1 do
      if aValue[lJ]=FforwardingAddresses[lI] then
        lKeep:=True;
    if not lKeep then
      FforwardingAddresses[lI].Free;
    end;
  FforwardingAddresses:=aValue;
end;

destructor TListHistoryResponse.Destroy;

var
  lI : Integer;

begin
  for lI:=0 to Length(Fhistory)-1 do
    Fhistory[lI].Free;
  inherited Destroy;
end;

procedure TListHistoryResponse.Sethistory(const aValue : THistoryArray);

var
  lI, lJ : Integer;
  lKeep : Boolean;

begin
  // Free the objects that are not in the new array
  for lI:=0 to Length(Fhistory)-1 do
    begin
    lKeep:=False;
    for lJ:=0 to Length(aValue)-1 do
      if aValue[lJ]=Fhistory[lI] then
        lKeep:=True;
    if not lKeep then
      Fhistory[lI].Free;
    end;
  Fhistory:=aValue;
end;

destructor TListLabelsResponse.Destroy;

var
  lI : Integer;

begin
  for lI:=0 to Length(Flabels)-1 do
    Flabels[lI].Free;
  inherited Destroy;
end;

procedure TListLabelsResponse.Setlabels(const aValue : TLabelArray);

var
  lI, lJ : Integer;
  lKeep : Boolean;

begin
  // Free the objects that are not in the new array
  for lI:=0 to Length(Flabels)-1 do
    begin
    lKeep:=False;
    for lJ:=0 to Length(aValue)-1 do
      if aValue[lJ]=Flabels[lI] then
        lKeep:=True;
    if not lKeep then
      Flabels[lI].Free;
    end;
  Flabels:=aValue;
end;

destructor TListMessagesResponse.Destroy;

var
  lI : Integer;

begin
  for lI:=0 to Length(Fmessages)-1 do
    Fmessages[lI].Free;
  inherited Destroy;
end;

procedure TListMessagesResponse.Setmessages(const aValue : TMessageArray);

var
  lI, lJ : Integer;
  lKeep : Boolean;

begin
  // Free the objects that are not in the new array
  for lI:=0 to Length(Fmessages)-1 do
    begin
    lKeep:=False;
    for lJ:=0 to Length(aValue)-1 do
      if aValue[lJ]=Fmessages[lI] then
        lKeep:=True;
    if not lKeep then
      Fmessages[lI].Free;
    end;
  Fmessages:=aValue;
end;

constructor TSendAs.CreateWithMembers;

begin
  FsmtpMsa := TSmtpMsa.Create;
end;

destructor TSendAs.Destroy;

begin
  FsmtpMsa.Free;
  inherited Destroy;
end;

procedure TSendAs.SetsmtpMsa(const aValue : TSmtpMsa);

begin
  if FsmtpMsa<>aValue then
    FsmtpMsa.Free;
  FsmtpMsa:=aValue;
end;

destructor TListSendAsResponse.Destroy;

var
  lI : Integer;

begin
  for lI:=0 to Length(FsendAs)-1 do
    FsendAs[lI].Free;
  inherited Destroy;
end;

procedure TListSendAsResponse.SetsendAs(const aValue : TSendAsArray);

var
  lI, lJ : Integer;
  lKeep : Boolean;

begin
  // Free the objects that are not in the new array
  for lI:=0 to Length(FsendAs)-1 do
    begin
    lKeep:=False;
    for lJ:=0 to Length(aValue)-1 do
      if aValue[lJ]=FsendAs[lI] then
        lKeep:=True;
    if not lKeep then
      FsendAs[lI].Free;
    end;
  FsendAs:=aValue;
end;

destructor TListSmimeInfoResponse.Destroy;

var
  lI : Integer;

begin
  for lI:=0 to Length(FsmimeInfo)-1 do
    FsmimeInfo[lI].Free;
  inherited Destroy;
end;

procedure TListSmimeInfoResponse.SetsmimeInfo(const aValue : TSmimeInfoArray);

var
  lI, lJ : Integer;
  lKeep : Boolean;

begin
  // Free the objects that are not in the new array
  for lI:=0 to Length(FsmimeInfo)-1 do
    begin
    lKeep:=False;
    for lJ:=0 to Length(aValue)-1 do
      if aValue[lJ]=FsmimeInfo[lI] then
        lKeep:=True;
    if not lKeep then
      FsmimeInfo[lI].Free;
    end;
  FsmimeInfo:=aValue;
end;

destructor TThread_.Destroy;

var
  lI : Integer;

begin
  for lI:=0 to Length(Fmessages)-1 do
    Fmessages[lI].Free;
  inherited Destroy;
end;

procedure TThread_.Setmessages(const aValue : TMessageArray);

var
  lI, lJ : Integer;
  lKeep : Boolean;

begin
  // Free the objects that are not in the new array
  for lI:=0 to Length(Fmessages)-1 do
    begin
    lKeep:=False;
    for lJ:=0 to Length(aValue)-1 do
      if aValue[lJ]=Fmessages[lI] then
        lKeep:=True;
    if not lKeep then
      Fmessages[lI].Free;
    end;
  Fmessages:=aValue;
end;

destructor TListThreadsResponse.Destroy;

var
  lI : Integer;

begin
  for lI:=0 to Length(Fthreads)-1 do
    Fthreads[lI].Free;
  inherited Destroy;
end;

procedure TListThreadsResponse.Setthreads(const aValue : TThread_Array);

var
  lI, lJ : Integer;
  lKeep : Boolean;

begin
  // Free the objects that are not in the new array
  for lI:=0 to Length(Fthreads)-1 do
    begin
    lKeep:=False;
    for lJ:=0 to Length(aValue)-1 do
      if aValue[lJ]=Fthreads[lI] then
        lKeep:=True;
    if not lKeep then
      Fthreads[lI].Free;
    end;
  Fthreads:=aValue;
end;

destructor TModifyMessageRequest.Destroy;

var
  lI : Integer;

begin
  for lI:=0 to Length(FaddClassificationLabels)-1 do
    FaddClassificationLabels[lI].Free;
  inherited Destroy;
end;

procedure TModifyMessageRequest.SetaddClassificationLabels(const aValue : TClassificationLabelValueArray);

var
  lI, lJ : Integer;
  lKeep : Boolean;

begin
  // Free the objects that are not in the new array
  for lI:=0 to Length(FaddClassificationLabels)-1 do
    begin
    lKeep:=False;
    for lJ:=0 to Length(aValue)-1 do
      if aValue[lJ]=FaddClassificationLabels[lI] then
        lKeep:=True;
    if not lKeep then
      FaddClassificationLabels[lI].Free;
    end;
  FaddClassificationLabels:=aValue;
end;

end.
