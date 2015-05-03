unit googlemirror;
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
  TAccount = class;
  TAccountArray = Array of TAccount;
  TAccountauthTokens = class;
  TAccountauthTokensArray = Array of TAccountauthTokens;
  TAccountfeatures = class;
  TAccountfeaturesArray = Array of TAccountfeatures;
  TAccountuserData = class;
  TAccountuserDataArray = Array of TAccountuserData;
  TAttachment = class;
  TAttachmentArray = Array of TAttachment;
  TAttachmentsListResponse = class;
  TAttachmentsListResponseArray = Array of TAttachmentsListResponse;
  TAttachmentsListResponseitems = class;
  TAttachmentsListResponseitemsArray = Array of TAttachmentsListResponseitems;
  TAuthToken = class;
  TAuthTokenArray = Array of TAuthToken;
  TCommand = class;
  TCommandArray = Array of TCommand;
  TContact = class;
  TContactArray = Array of TContact;
  TContactacceptCommands = class;
  TContactacceptCommandsArray = Array of TContactacceptCommands;
  TContactacceptTypes = class;
  TContactacceptTypesArray = Array of TContactacceptTypes;
  TContactimageUrls = class;
  TContactimageUrlsArray = Array of TContactimageUrls;
  TContactsharingFeatures = class;
  TContactsharingFeaturesArray = Array of TContactsharingFeatures;
  TContactsListResponse = class;
  TContactsListResponseArray = Array of TContactsListResponse;
  TContactsListResponseitems = class;
  TContactsListResponseitemsArray = Array of TContactsListResponseitems;
  TLocation = class;
  TLocationArray = Array of TLocation;
  TLocationsListResponse = class;
  TLocationsListResponseArray = Array of TLocationsListResponse;
  TLocationsListResponseitems = class;
  TLocationsListResponseitemsArray = Array of TLocationsListResponseitems;
  TMenuItem = class;
  TMenuItemArray = Array of TMenuItem;
  TMenuItemvalues = class;
  TMenuItemvaluesArray = Array of TMenuItemvalues;
  TMenuValue = class;
  TMenuValueArray = Array of TMenuValue;
  TNotification = class;
  TNotificationArray = Array of TNotification;
  TNotificationuserActions = class;
  TNotificationuserActionsArray = Array of TNotificationuserActions;
  TNotificationConfig = class;
  TNotificationConfigArray = Array of TNotificationConfig;
  TSetting = class;
  TSettingArray = Array of TSetting;
  TSubscription = class;
  TSubscriptionArray = Array of TSubscription;
  TSubscriptionoperation = class;
  TSubscriptionoperationArray = Array of TSubscriptionoperation;
  TSubscriptionsListResponse = class;
  TSubscriptionsListResponseArray = Array of TSubscriptionsListResponse;
  TSubscriptionsListResponseitems = class;
  TSubscriptionsListResponseitemsArray = Array of TSubscriptionsListResponseitems;
  TTimelineItem = class;
  TTimelineItemArray = Array of TTimelineItem;
  TTimelineItemattachments = class;
  TTimelineItemattachmentsArray = Array of TTimelineItemattachments;
  TTimelineItemmenuItems = class;
  TTimelineItemmenuItemsArray = Array of TTimelineItemmenuItems;
  TTimelineItemrecipients = class;
  TTimelineItemrecipientsArray = Array of TTimelineItemrecipients;
  TTimelineListResponse = class;
  TTimelineListResponseArray = Array of TTimelineListResponse;
  TTimelineListResponseitems = class;
  TTimelineListResponseitemsArray = Array of TTimelineListResponseitems;
  TUserAction = class;
  TUserActionArray = Array of TUserAction;
  TUserData = class;
  TUserDataArray = Array of TUserData;
  
  { --------------------------------------------------------------------
    TAccount
    --------------------------------------------------------------------}
  
  TAccount = Class(TGoogleBaseObject)
  Private
    FauthTokens : TAccountauthTokens;
    Ffeatures : TAccountfeatures;
    Fpassword : string;
    FuserData : TAccountuserData;
  Protected
    //Property setters
    Procedure SetauthTokens(AIndex : Integer; AValue : TAccountauthTokens); virtual;
    Procedure Setfeatures(AIndex : Integer; AValue : TAccountfeatures); virtual;
    Procedure Setpassword(AIndex : Integer; AValue : string); virtual;
    Procedure SetuserData(AIndex : Integer; AValue : TAccountuserData); virtual;
  Public
  Published
    Property authTokens : TAccountauthTokens Index 0 Read FauthTokens Write SetauthTokens;
    Property features : TAccountfeatures Index 8 Read Ffeatures Write Setfeatures;
    Property password : string Index 16 Read Fpassword Write Setpassword;
    Property userData : TAccountuserData Index 24 Read FuserData Write SetuserData;
  end;
  TAccountClass = Class of TAccount;
  
  { --------------------------------------------------------------------
    TAccountauthTokens
    --------------------------------------------------------------------}
  
  TAccountauthTokens = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountauthTokensClass = Class of TAccountauthTokens;
  
  { --------------------------------------------------------------------
    TAccountfeatures
    --------------------------------------------------------------------}
  
  TAccountfeatures = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountfeaturesClass = Class of TAccountfeatures;
  
  { --------------------------------------------------------------------
    TAccountuserData
    --------------------------------------------------------------------}
  
  TAccountuserData = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountuserDataClass = Class of TAccountuserData;
  
  { --------------------------------------------------------------------
    TAttachment
    --------------------------------------------------------------------}
  
  TAttachment = Class(TGoogleBaseObject)
  Private
    FcontentType : string;
    FcontentUrl : string;
    Fid : string;
    FisProcessingContent : boolean;
  Protected
    //Property setters
    Procedure SetcontentType(AIndex : Integer; AValue : string); virtual;
    Procedure SetcontentUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetisProcessingContent(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property contentType : string Index 0 Read FcontentType Write SetcontentType;
    Property contentUrl : string Index 8 Read FcontentUrl Write SetcontentUrl;
    Property id : string Index 16 Read Fid Write Setid;
    Property isProcessingContent : boolean Index 24 Read FisProcessingContent Write SetisProcessingContent;
  end;
  TAttachmentClass = Class of TAttachment;
  
  { --------------------------------------------------------------------
    TAttachmentsListResponse
    --------------------------------------------------------------------}
  
  TAttachmentsListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TAttachmentsListResponseitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TAttachmentsListResponseitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TAttachmentsListResponseitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TAttachmentsListResponseClass = Class of TAttachmentsListResponse;
  
  { --------------------------------------------------------------------
    TAttachmentsListResponseitems
    --------------------------------------------------------------------}
  
  TAttachmentsListResponseitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAttachmentsListResponseitemsClass = Class of TAttachmentsListResponseitems;
  
  { --------------------------------------------------------------------
    TAuthToken
    --------------------------------------------------------------------}
  
  TAuthToken = Class(TGoogleBaseObject)
  Private
    FauthToken : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetauthToken(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property authToken : string Index 0 Read FauthToken Write SetauthToken;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TAuthTokenClass = Class of TAuthToken;
  
  { --------------------------------------------------------------------
    TCommand
    --------------------------------------------------------------------}
  
  TCommand = Class(TGoogleBaseObject)
  Private
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property _type : string Index 0 Read F_type Write Set_type;
  end;
  TCommandClass = Class of TCommand;
  
  { --------------------------------------------------------------------
    TContact
    --------------------------------------------------------------------}
  
  TContact = Class(TGoogleBaseObject)
  Private
    FacceptCommands : TContactacceptCommands;
    FacceptTypes : TContactacceptTypes;
    FdisplayName : string;
    Fid : string;
    FimageUrls : TContactimageUrls;
    Fkind : string;
    FphoneNumber : string;
    Fpriority : integer;
    FsharingFeatures : TContactsharingFeatures;
    Fsource : string;
    FspeakableName : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetacceptCommands(AIndex : Integer; AValue : TContactacceptCommands); virtual;
    Procedure SetacceptTypes(AIndex : Integer; AValue : TContactacceptTypes); virtual;
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetimageUrls(AIndex : Integer; AValue : TContactimageUrls); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetphoneNumber(AIndex : Integer; AValue : string); virtual;
    Procedure Setpriority(AIndex : Integer; AValue : integer); virtual;
    Procedure SetsharingFeatures(AIndex : Integer; AValue : TContactsharingFeatures); virtual;
    Procedure Setsource(AIndex : Integer; AValue : string); virtual;
    Procedure SetspeakableName(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property acceptCommands : TContactacceptCommands Index 0 Read FacceptCommands Write SetacceptCommands;
    Property acceptTypes : TContactacceptTypes Index 8 Read FacceptTypes Write SetacceptTypes;
    Property displayName : string Index 16 Read FdisplayName Write SetdisplayName;
    Property id : string Index 24 Read Fid Write Setid;
    Property imageUrls : TContactimageUrls Index 32 Read FimageUrls Write SetimageUrls;
    Property kind : string Index 40 Read Fkind Write Setkind;
    Property phoneNumber : string Index 48 Read FphoneNumber Write SetphoneNumber;
    Property priority : integer Index 56 Read Fpriority Write Setpriority;
    Property sharingFeatures : TContactsharingFeatures Index 64 Read FsharingFeatures Write SetsharingFeatures;
    Property source : string Index 72 Read Fsource Write Setsource;
    Property speakableName : string Index 80 Read FspeakableName Write SetspeakableName;
    Property _type : string Index 88 Read F_type Write Set_type;
  end;
  TContactClass = Class of TContact;
  
  { --------------------------------------------------------------------
    TContactacceptCommands
    --------------------------------------------------------------------}
  
  TContactacceptCommands = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TContactacceptCommandsClass = Class of TContactacceptCommands;
  
  { --------------------------------------------------------------------
    TContactacceptTypes
    --------------------------------------------------------------------}
  
  TContactacceptTypes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TContactacceptTypesClass = Class of TContactacceptTypes;
  
  { --------------------------------------------------------------------
    TContactimageUrls
    --------------------------------------------------------------------}
  
  TContactimageUrls = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TContactimageUrlsClass = Class of TContactimageUrls;
  
  { --------------------------------------------------------------------
    TContactsharingFeatures
    --------------------------------------------------------------------}
  
  TContactsharingFeatures = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TContactsharingFeaturesClass = Class of TContactsharingFeatures;
  
  { --------------------------------------------------------------------
    TContactsListResponse
    --------------------------------------------------------------------}
  
  TContactsListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TContactsListResponseitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TContactsListResponseitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TContactsListResponseitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TContactsListResponseClass = Class of TContactsListResponse;
  
  { --------------------------------------------------------------------
    TContactsListResponseitems
    --------------------------------------------------------------------}
  
  TContactsListResponseitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TContactsListResponseitemsClass = Class of TContactsListResponseitems;
  
  { --------------------------------------------------------------------
    TLocation
    --------------------------------------------------------------------}
  
  TLocation = Class(TGoogleBaseObject)
  Private
    Faccuracy : double;
    Faddress : string;
    FdisplayName : string;
    Fid : string;
    Fkind : string;
    Flatitude : double;
    Flongitude : double;
    Ftimestamp : TDatetime;
  Protected
    //Property setters
    Procedure Setaccuracy(AIndex : Integer; AValue : double); virtual;
    Procedure Setaddress(AIndex : Integer; AValue : string); virtual;
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlatitude(AIndex : Integer; AValue : double); virtual;
    Procedure Setlongitude(AIndex : Integer; AValue : double); virtual;
    Procedure Settimestamp(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property accuracy : double Index 0 Read Faccuracy Write Setaccuracy;
    Property address : string Index 8 Read Faddress Write Setaddress;
    Property displayName : string Index 16 Read FdisplayName Write SetdisplayName;
    Property id : string Index 24 Read Fid Write Setid;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property latitude : double Index 40 Read Flatitude Write Setlatitude;
    Property longitude : double Index 48 Read Flongitude Write Setlongitude;
    Property timestamp : TDatetime Index 56 Read Ftimestamp Write Settimestamp;
  end;
  TLocationClass = Class of TLocation;
  
  { --------------------------------------------------------------------
    TLocationsListResponse
    --------------------------------------------------------------------}
  
  TLocationsListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TLocationsListResponseitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TLocationsListResponseitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TLocationsListResponseitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TLocationsListResponseClass = Class of TLocationsListResponse;
  
  { --------------------------------------------------------------------
    TLocationsListResponseitems
    --------------------------------------------------------------------}
  
  TLocationsListResponseitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLocationsListResponseitemsClass = Class of TLocationsListResponseitems;
  
  { --------------------------------------------------------------------
    TMenuItem
    --------------------------------------------------------------------}
  
  TMenuItem = Class(TGoogleBaseObject)
  Private
    Faction : string;
    Fcontextual_command : string;
    Fid : string;
    Fpayload : string;
    FremoveWhenSelected : boolean;
    Fvalues : TMenuItemvalues;
  Protected
    //Property setters
    Procedure Setaction(AIndex : Integer; AValue : string); virtual;
    Procedure Setcontextual_command(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setpayload(AIndex : Integer; AValue : string); virtual;
    Procedure SetremoveWhenSelected(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setvalues(AIndex : Integer; AValue : TMenuItemvalues); virtual;
  Public
  Published
    Property action : string Index 0 Read Faction Write Setaction;
    Property contextual_command : string Index 8 Read Fcontextual_command Write Setcontextual_command;
    Property id : string Index 16 Read Fid Write Setid;
    Property payload : string Index 24 Read Fpayload Write Setpayload;
    Property removeWhenSelected : boolean Index 32 Read FremoveWhenSelected Write SetremoveWhenSelected;
    Property values : TMenuItemvalues Index 40 Read Fvalues Write Setvalues;
  end;
  TMenuItemClass = Class of TMenuItem;
  
  { --------------------------------------------------------------------
    TMenuItemvalues
    --------------------------------------------------------------------}
  
  TMenuItemvalues = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMenuItemvaluesClass = Class of TMenuItemvalues;
  
  { --------------------------------------------------------------------
    TMenuValue
    --------------------------------------------------------------------}
  
  TMenuValue = Class(TGoogleBaseObject)
  Private
    FdisplayName : string;
    FiconUrl : string;
    Fstate : string;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure SeticonUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Setstate(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property displayName : string Index 0 Read FdisplayName Write SetdisplayName;
    Property iconUrl : string Index 8 Read FiconUrl Write SeticonUrl;
    Property state : string Index 16 Read Fstate Write Setstate;
  end;
  TMenuValueClass = Class of TMenuValue;
  
  { --------------------------------------------------------------------
    TNotification
    --------------------------------------------------------------------}
  
  TNotification = Class(TGoogleBaseObject)
  Private
    Fcollection : string;
    FitemId : string;
    Foperation : string;
    FuserActions : TNotificationuserActions;
    FuserToken : string;
    FverifyToken : string;
  Protected
    //Property setters
    Procedure Setcollection(AIndex : Integer; AValue : string); virtual;
    Procedure SetitemId(AIndex : Integer; AValue : string); virtual;
    Procedure Setoperation(AIndex : Integer; AValue : string); virtual;
    Procedure SetuserActions(AIndex : Integer; AValue : TNotificationuserActions); virtual;
    Procedure SetuserToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetverifyToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property collection : string Index 0 Read Fcollection Write Setcollection;
    Property itemId : string Index 8 Read FitemId Write SetitemId;
    Property operation : string Index 16 Read Foperation Write Setoperation;
    Property userActions : TNotificationuserActions Index 24 Read FuserActions Write SetuserActions;
    Property userToken : string Index 32 Read FuserToken Write SetuserToken;
    Property verifyToken : string Index 40 Read FverifyToken Write SetverifyToken;
  end;
  TNotificationClass = Class of TNotification;
  
  { --------------------------------------------------------------------
    TNotificationuserActions
    --------------------------------------------------------------------}
  
  TNotificationuserActions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TNotificationuserActionsClass = Class of TNotificationuserActions;
  
  { --------------------------------------------------------------------
    TNotificationConfig
    --------------------------------------------------------------------}
  
  TNotificationConfig = Class(TGoogleBaseObject)
  Private
    FdeliveryTime : TDatetime;
    Flevel : string;
  Protected
    //Property setters
    Procedure SetdeliveryTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setlevel(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property deliveryTime : TDatetime Index 0 Read FdeliveryTime Write SetdeliveryTime;
    Property level : string Index 8 Read Flevel Write Setlevel;
  end;
  TNotificationConfigClass = Class of TNotificationConfig;
  
  { --------------------------------------------------------------------
    TSetting
    --------------------------------------------------------------------}
  
  TSetting = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fkind : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property value : string Index 16 Read Fvalue Write Setvalue;
  end;
  TSettingClass = Class of TSetting;
  
  { --------------------------------------------------------------------
    TSubscription
    --------------------------------------------------------------------}
  
  TSubscription = Class(TGoogleBaseObject)
  Private
    FcallbackUrl : string;
    Fcollection : string;
    Fid : string;
    Fkind : string;
    Fnotification : TNotification;
    Foperation : TSubscriptionoperation;
    Fupdated : TDatetime;
    FuserToken : string;
    FverifyToken : string;
  Protected
    //Property setters
    Procedure SetcallbackUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Setcollection(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setnotification(AIndex : Integer; AValue : TNotification); virtual;
    Procedure Setoperation(AIndex : Integer; AValue : TSubscriptionoperation); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetuserToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetverifyToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property callbackUrl : string Index 0 Read FcallbackUrl Write SetcallbackUrl;
    Property collection : string Index 8 Read Fcollection Write Setcollection;
    Property id : string Index 16 Read Fid Write Setid;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property notification : TNotification Index 32 Read Fnotification Write Setnotification;
    Property operation : TSubscriptionoperation Index 40 Read Foperation Write Setoperation;
    Property updated : TDatetime Index 48 Read Fupdated Write Setupdated;
    Property userToken : string Index 56 Read FuserToken Write SetuserToken;
    Property verifyToken : string Index 64 Read FverifyToken Write SetverifyToken;
  end;
  TSubscriptionClass = Class of TSubscription;
  
  { --------------------------------------------------------------------
    TSubscriptionoperation
    --------------------------------------------------------------------}
  
  TSubscriptionoperation = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSubscriptionoperationClass = Class of TSubscriptionoperation;
  
  { --------------------------------------------------------------------
    TSubscriptionsListResponse
    --------------------------------------------------------------------}
  
  TSubscriptionsListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TSubscriptionsListResponseitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TSubscriptionsListResponseitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TSubscriptionsListResponseitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TSubscriptionsListResponseClass = Class of TSubscriptionsListResponse;
  
  { --------------------------------------------------------------------
    TSubscriptionsListResponseitems
    --------------------------------------------------------------------}
  
  TSubscriptionsListResponseitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSubscriptionsListResponseitemsClass = Class of TSubscriptionsListResponseitems;
  
  { --------------------------------------------------------------------
    TTimelineItem
    --------------------------------------------------------------------}
  
  TTimelineItem = Class(TGoogleBaseObject)
  Private
    Fattachments : TTimelineItemattachments;
    FbundleId : string;
    FcanonicalUrl : string;
    Fcreated : TDatetime;
    Fcreator : TContact;
    FdisplayTime : TDatetime;
    Fetag : string;
    Fhtml : string;
    Fid : string;
    FinReplyTo : string;
    FisBundleCover : boolean;
    FisDeleted : boolean;
    FisPinned : boolean;
    Fkind : string;
    Flocation : TLocation;
    FmenuItems : TTimelineItemmenuItems;
    Fnotification : TNotificationConfig;
    FpinScore : integer;
    Frecipients : TTimelineItemrecipients;
    FselfLink : string;
    FsourceItemId : string;
    FspeakableText : string;
    FspeakableType : string;
    Ftext : string;
    Ftitle : string;
    Fupdated : TDatetime;
  Protected
    //Property setters
    Procedure Setattachments(AIndex : Integer; AValue : TTimelineItemattachments); virtual;
    Procedure SetbundleId(AIndex : Integer; AValue : string); virtual;
    Procedure SetcanonicalUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setcreator(AIndex : Integer; AValue : TContact); virtual;
    Procedure SetdisplayTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Sethtml(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetinReplyTo(AIndex : Integer; AValue : string); virtual;
    Procedure SetisBundleCover(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetisDeleted(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetisPinned(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : TLocation); virtual;
    Procedure SetmenuItems(AIndex : Integer; AValue : TTimelineItemmenuItems); virtual;
    Procedure Setnotification(AIndex : Integer; AValue : TNotificationConfig); virtual;
    Procedure SetpinScore(AIndex : Integer; AValue : integer); virtual;
    Procedure Setrecipients(AIndex : Integer; AValue : TTimelineItemrecipients); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetsourceItemId(AIndex : Integer; AValue : string); virtual;
    Procedure SetspeakableText(AIndex : Integer; AValue : string); virtual;
    Procedure SetspeakableType(AIndex : Integer; AValue : string); virtual;
    Procedure Settext(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property attachments : TTimelineItemattachments Index 0 Read Fattachments Write Setattachments;
    Property bundleId : string Index 8 Read FbundleId Write SetbundleId;
    Property canonicalUrl : string Index 16 Read FcanonicalUrl Write SetcanonicalUrl;
    Property created : TDatetime Index 24 Read Fcreated Write Setcreated;
    Property creator : TContact Index 32 Read Fcreator Write Setcreator;
    Property displayTime : TDatetime Index 40 Read FdisplayTime Write SetdisplayTime;
    Property etag : string Index 48 Read Fetag Write Setetag;
    Property html : string Index 56 Read Fhtml Write Sethtml;
    Property id : string Index 64 Read Fid Write Setid;
    Property inReplyTo : string Index 72 Read FinReplyTo Write SetinReplyTo;
    Property isBundleCover : boolean Index 80 Read FisBundleCover Write SetisBundleCover;
    Property isDeleted : boolean Index 88 Read FisDeleted Write SetisDeleted;
    Property isPinned : boolean Index 96 Read FisPinned Write SetisPinned;
    Property kind : string Index 104 Read Fkind Write Setkind;
    Property location : TLocation Index 112 Read Flocation Write Setlocation;
    Property menuItems : TTimelineItemmenuItems Index 120 Read FmenuItems Write SetmenuItems;
    Property notification : TNotificationConfig Index 128 Read Fnotification Write Setnotification;
    Property pinScore : integer Index 136 Read FpinScore Write SetpinScore;
    Property recipients : TTimelineItemrecipients Index 144 Read Frecipients Write Setrecipients;
    Property selfLink : string Index 152 Read FselfLink Write SetselfLink;
    Property sourceItemId : string Index 160 Read FsourceItemId Write SetsourceItemId;
    Property speakableText : string Index 168 Read FspeakableText Write SetspeakableText;
    Property speakableType : string Index 176 Read FspeakableType Write SetspeakableType;
    Property text : string Index 184 Read Ftext Write Settext;
    Property title : string Index 192 Read Ftitle Write Settitle;
    Property updated : TDatetime Index 200 Read Fupdated Write Setupdated;
  end;
  TTimelineItemClass = Class of TTimelineItem;
  
  { --------------------------------------------------------------------
    TTimelineItemattachments
    --------------------------------------------------------------------}
  
  TTimelineItemattachments = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTimelineItemattachmentsClass = Class of TTimelineItemattachments;
  
  { --------------------------------------------------------------------
    TTimelineItemmenuItems
    --------------------------------------------------------------------}
  
  TTimelineItemmenuItems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTimelineItemmenuItemsClass = Class of TTimelineItemmenuItems;
  
  { --------------------------------------------------------------------
    TTimelineItemrecipients
    --------------------------------------------------------------------}
  
  TTimelineItemrecipients = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTimelineItemrecipientsClass = Class of TTimelineItemrecipients;
  
  { --------------------------------------------------------------------
    TTimelineListResponse
    --------------------------------------------------------------------}
  
  TTimelineListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TTimelineListResponseitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TTimelineListResponseitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TTimelineListResponseitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TTimelineListResponseClass = Class of TTimelineListResponse;
  
  { --------------------------------------------------------------------
    TTimelineListResponseitems
    --------------------------------------------------------------------}
  
  TTimelineListResponseitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTimelineListResponseitemsClass = Class of TTimelineListResponseitems;
  
  { --------------------------------------------------------------------
    TUserAction
    --------------------------------------------------------------------}
  
  TUserAction = Class(TGoogleBaseObject)
  Private
    Fpayload : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setpayload(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property payload : string Index 0 Read Fpayload Write Setpayload;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TUserActionClass = Class of TUserAction;
  
  { --------------------------------------------------------------------
    TUserData
    --------------------------------------------------------------------}
  
  TUserData = Class(TGoogleBaseObject)
  Private
    Fkey : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property key : string Index 0 Read Fkey Write Setkey;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TUserDataClass = Class of TUserData;
  
  { --------------------------------------------------------------------
    TAccountsResource
    --------------------------------------------------------------------}
  
  TAccountsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Insert(accountName: string; accountType: string; userToken: string; aAccount : TAccount) : TAccount;
  end;
  
  
  { --------------------------------------------------------------------
    TContactsResource
    --------------------------------------------------------------------}
  
  TContactsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(id: string);
    Function Get(id: string) : TContact;
    Function Insert(aContact : TContact) : TContact;
    Function List : TContactsListResponse;
    Function Patch(id: string; aContact : TContact) : TContact;
    Function Update(id: string; aContact : TContact) : TContact;
  end;
  
  
  { --------------------------------------------------------------------
    TLocationsResource
    --------------------------------------------------------------------}
  
  TLocationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string) : TLocation;
    Function List : TLocationsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TSettingsResource
    --------------------------------------------------------------------}
  
  TSettingsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string) : TSetting;
  end;
  
  
  { --------------------------------------------------------------------
    TSubscriptionsResource
    --------------------------------------------------------------------}
  
  TSubscriptionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(id: string);
    Function Insert(aSubscription : TSubscription) : TSubscription;
    Function List : TSubscriptionsListResponse;
    Function Update(id: string; aSubscription : TSubscription) : TSubscription;
  end;
  
  
  { --------------------------------------------------------------------
    TTimelineResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TTimelineResource, method List
  
  TTimelineListOptions = Record
    bundleId : string;
    includeDeleted : boolean;
    maxResults : integer;
    orderBy : string;
    pageToken : string;
    pinnedOnly : boolean;
    sourceItemId : string;
  end;
  
  TTimelineResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(id: string);
    Function Get(id: string) : TTimelineItem;
    Function Insert(aTimelineItem : TTimelineItem) : TTimelineItem;
    Function List(AQuery : string  = '') : TTimelineListResponse;
    Function List(AQuery : TTimelinelistOptions) : TTimelineListResponse;
    Function Patch(id: string; aTimelineItem : TTimelineItem) : TTimelineItem;
    Function Update(id: string; aTimelineItem : TTimelineItem) : TTimelineItem;
  end;
  
  
  { --------------------------------------------------------------------
    TMirrorAPI
    --------------------------------------------------------------------}
  
  TMirrorAPI = Class(TGoogleAPI)
  Private
    FAccountsInstance : TAccountsResource;
    FContactsInstance : TContactsResource;
    FLocationsInstance : TLocationsResource;
    FSettingsInstance : TSettingsResource;
    FSubscriptionsInstance : TSubscriptionsResource;
    FTimelineInstance : TTimelineResource;
    Function GetAccountsInstance : TAccountsResource;virtual;
    Function GetContactsInstance : TContactsResource;virtual;
    Function GetLocationsInstance : TLocationsResource;virtual;
    Function GetSettingsInstance : TSettingsResource;virtual;
    Function GetSubscriptionsInstance : TSubscriptionsResource;virtual;
    Function GetTimelineInstance : TTimelineResource;virtual;
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
    Function CreateAccountsResource(AOwner : TComponent) : TAccountsResource;virtual;overload;
    Function CreateAccountsResource : TAccountsResource;virtual;overload;
    Function CreateContactsResource(AOwner : TComponent) : TContactsResource;virtual;overload;
    Function CreateContactsResource : TContactsResource;virtual;overload;
    Function CreateLocationsResource(AOwner : TComponent) : TLocationsResource;virtual;overload;
    Function CreateLocationsResource : TLocationsResource;virtual;overload;
    Function CreateSettingsResource(AOwner : TComponent) : TSettingsResource;virtual;overload;
    Function CreateSettingsResource : TSettingsResource;virtual;overload;
    Function CreateSubscriptionsResource(AOwner : TComponent) : TSubscriptionsResource;virtual;overload;
    Function CreateSubscriptionsResource : TSubscriptionsResource;virtual;overload;
    Function CreateTimelineResource(AOwner : TComponent) : TTimelineResource;virtual;overload;
    Function CreateTimelineResource : TTimelineResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AccountsResource : TAccountsResource Read GetAccountsInstance;
    Property ContactsResource : TContactsResource Read GetContactsInstance;
    Property LocationsResource : TLocationsResource Read GetLocationsInstance;
    Property SettingsResource : TSettingsResource Read GetSettingsInstance;
    Property SubscriptionsResource : TSubscriptionsResource Read GetSubscriptionsInstance;
    Property TimelineResource : TTimelineResource Read GetTimelineInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAccount
  --------------------------------------------------------------------}


Procedure TAccount.SetauthTokens(AIndex : Integer; AValue : TAccountauthTokens); 

begin
  If (FauthTokens=AValue) then exit;
  FauthTokens:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setfeatures(AIndex : Integer; AValue : TAccountfeatures); 

begin
  If (Ffeatures=AValue) then exit;
  Ffeatures:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setpassword(AIndex : Integer; AValue : string); 

begin
  If (Fpassword=AValue) then exit;
  Fpassword:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetuserData(AIndex : Integer; AValue : TAccountuserData); 

begin
  If (FuserData=AValue) then exit;
  FuserData:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountauthTokens
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccountfeatures
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccountuserData
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAttachment
  --------------------------------------------------------------------}


Procedure TAttachment.SetcontentType(AIndex : Integer; AValue : string); 

begin
  If (FcontentType=AValue) then exit;
  FcontentType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAttachment.SetcontentUrl(AIndex : Integer; AValue : string); 

begin
  If (FcontentUrl=AValue) then exit;
  FcontentUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAttachment.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAttachment.SetisProcessingContent(AIndex : Integer; AValue : boolean); 

begin
  If (FisProcessingContent=AValue) then exit;
  FisProcessingContent:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAttachmentsListResponse
  --------------------------------------------------------------------}


Procedure TAttachmentsListResponse.Setitems(AIndex : Integer; AValue : TAttachmentsListResponseitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAttachmentsListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAttachmentsListResponseitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAuthToken
  --------------------------------------------------------------------}


Procedure TAuthToken.SetauthToken(AIndex : Integer; AValue : string); 

begin
  If (FauthToken=AValue) then exit;
  FauthToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAuthToken.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAuthToken.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TCommand
  --------------------------------------------------------------------}


Procedure TCommand.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TCommand.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TContact
  --------------------------------------------------------------------}


Procedure TContact.SetacceptCommands(AIndex : Integer; AValue : TContactacceptCommands); 

begin
  If (FacceptCommands=AValue) then exit;
  FacceptCommands:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContact.SetacceptTypes(AIndex : Integer; AValue : TContactacceptTypes); 

begin
  If (FacceptTypes=AValue) then exit;
  FacceptTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContact.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContact.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContact.SetimageUrls(AIndex : Integer; AValue : TContactimageUrls); 

begin
  If (FimageUrls=AValue) then exit;
  FimageUrls:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContact.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContact.SetphoneNumber(AIndex : Integer; AValue : string); 

begin
  If (FphoneNumber=AValue) then exit;
  FphoneNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContact.Setpriority(AIndex : Integer; AValue : integer); 

begin
  If (Fpriority=AValue) then exit;
  Fpriority:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContact.SetsharingFeatures(AIndex : Integer; AValue : TContactsharingFeatures); 

begin
  If (FsharingFeatures=AValue) then exit;
  FsharingFeatures:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContact.Setsource(AIndex : Integer; AValue : string); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContact.SetspeakableName(AIndex : Integer; AValue : string); 

begin
  If (FspeakableName=AValue) then exit;
  FspeakableName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContact.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TContact.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TContactacceptCommands
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TContactacceptTypes
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TContactimageUrls
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TContactsharingFeatures
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TContactsListResponse
  --------------------------------------------------------------------}


Procedure TContactsListResponse.Setitems(AIndex : Integer; AValue : TContactsListResponseitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContactsListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TContactsListResponseitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLocation
  --------------------------------------------------------------------}


Procedure TLocation.Setaccuracy(AIndex : Integer; AValue : double); 

begin
  If (Faccuracy=AValue) then exit;
  Faccuracy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocation.Setaddress(AIndex : Integer; AValue : string); 

begin
  If (Faddress=AValue) then exit;
  Faddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocation.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocation.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocation.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocation.Setlatitude(AIndex : Integer; AValue : double); 

begin
  If (Flatitude=AValue) then exit;
  Flatitude:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocation.Setlongitude(AIndex : Integer; AValue : double); 

begin
  If (Flongitude=AValue) then exit;
  Flongitude:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocation.Settimestamp(AIndex : Integer; AValue : TDatetime); 

begin
  If (Ftimestamp=AValue) then exit;
  Ftimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLocationsListResponse
  --------------------------------------------------------------------}


Procedure TLocationsListResponse.Setitems(AIndex : Integer; AValue : TLocationsListResponseitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocationsListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLocationsListResponseitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMenuItem
  --------------------------------------------------------------------}


Procedure TMenuItem.Setaction(AIndex : Integer; AValue : string); 

begin
  If (Faction=AValue) then exit;
  Faction:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMenuItem.Setcontextual_command(AIndex : Integer; AValue : string); 

begin
  If (Fcontextual_command=AValue) then exit;
  Fcontextual_command:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMenuItem.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMenuItem.Setpayload(AIndex : Integer; AValue : string); 

begin
  If (Fpayload=AValue) then exit;
  Fpayload:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMenuItem.SetremoveWhenSelected(AIndex : Integer; AValue : boolean); 

begin
  If (FremoveWhenSelected=AValue) then exit;
  FremoveWhenSelected:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMenuItem.Setvalues(AIndex : Integer; AValue : TMenuItemvalues); 

begin
  If (Fvalues=AValue) then exit;
  Fvalues:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMenuItemvalues
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMenuValue
  --------------------------------------------------------------------}


Procedure TMenuValue.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMenuValue.SeticonUrl(AIndex : Integer; AValue : string); 

begin
  If (FiconUrl=AValue) then exit;
  FiconUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMenuValue.Setstate(AIndex : Integer; AValue : string); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TNotification
  --------------------------------------------------------------------}


Procedure TNotification.Setcollection(AIndex : Integer; AValue : string); 

begin
  If (Fcollection=AValue) then exit;
  Fcollection:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNotification.SetitemId(AIndex : Integer; AValue : string); 

begin
  If (FitemId=AValue) then exit;
  FitemId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNotification.Setoperation(AIndex : Integer; AValue : string); 

begin
  If (Foperation=AValue) then exit;
  Foperation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNotification.SetuserActions(AIndex : Integer; AValue : TNotificationuserActions); 

begin
  If (FuserActions=AValue) then exit;
  FuserActions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNotification.SetuserToken(AIndex : Integer; AValue : string); 

begin
  If (FuserToken=AValue) then exit;
  FuserToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNotification.SetverifyToken(AIndex : Integer; AValue : string); 

begin
  If (FverifyToken=AValue) then exit;
  FverifyToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TNotificationuserActions
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TNotificationConfig
  --------------------------------------------------------------------}


Procedure TNotificationConfig.SetdeliveryTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FdeliveryTime=AValue) then exit;
  FdeliveryTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNotificationConfig.Setlevel(AIndex : Integer; AValue : string); 

begin
  If (Flevel=AValue) then exit;
  Flevel:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSetting
  --------------------------------------------------------------------}


Procedure TSetting.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetting.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetting.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSubscription
  --------------------------------------------------------------------}


Procedure TSubscription.SetcallbackUrl(AIndex : Integer; AValue : string); 

begin
  If (FcallbackUrl=AValue) then exit;
  FcallbackUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.Setcollection(AIndex : Integer; AValue : string); 

begin
  If (Fcollection=AValue) then exit;
  Fcollection:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.Setnotification(AIndex : Integer; AValue : TNotification); 

begin
  If (Fnotification=AValue) then exit;
  Fnotification:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.Setoperation(AIndex : Integer; AValue : TSubscriptionoperation); 

begin
  If (Foperation=AValue) then exit;
  Foperation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.SetuserToken(AIndex : Integer; AValue : string); 

begin
  If (FuserToken=AValue) then exit;
  FuserToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.SetverifyToken(AIndex : Integer; AValue : string); 

begin
  If (FverifyToken=AValue) then exit;
  FverifyToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSubscriptionoperation
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSubscriptionsListResponse
  --------------------------------------------------------------------}


Procedure TSubscriptionsListResponse.Setitems(AIndex : Integer; AValue : TSubscriptionsListResponseitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionsListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSubscriptionsListResponseitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTimelineItem
  --------------------------------------------------------------------}


Procedure TTimelineItem.Setattachments(AIndex : Integer; AValue : TTimelineItemattachments); 

begin
  If (Fattachments=AValue) then exit;
  Fattachments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.SetbundleId(AIndex : Integer; AValue : string); 

begin
  If (FbundleId=AValue) then exit;
  FbundleId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.SetcanonicalUrl(AIndex : Integer; AValue : string); 

begin
  If (FcanonicalUrl=AValue) then exit;
  FcanonicalUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.Setcreated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.Setcreator(AIndex : Integer; AValue : TContact); 

begin
  If (Fcreator=AValue) then exit;
  Fcreator:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.SetdisplayTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FdisplayTime=AValue) then exit;
  FdisplayTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.Sethtml(AIndex : Integer; AValue : string); 

begin
  If (Fhtml=AValue) then exit;
  Fhtml:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.SetinReplyTo(AIndex : Integer; AValue : string); 

begin
  If (FinReplyTo=AValue) then exit;
  FinReplyTo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.SetisBundleCover(AIndex : Integer; AValue : boolean); 

begin
  If (FisBundleCover=AValue) then exit;
  FisBundleCover:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.SetisDeleted(AIndex : Integer; AValue : boolean); 

begin
  If (FisDeleted=AValue) then exit;
  FisDeleted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.SetisPinned(AIndex : Integer; AValue : boolean); 

begin
  If (FisPinned=AValue) then exit;
  FisPinned:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.Setlocation(AIndex : Integer; AValue : TLocation); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.SetmenuItems(AIndex : Integer; AValue : TTimelineItemmenuItems); 

begin
  If (FmenuItems=AValue) then exit;
  FmenuItems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.Setnotification(AIndex : Integer; AValue : TNotificationConfig); 

begin
  If (Fnotification=AValue) then exit;
  Fnotification:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.SetpinScore(AIndex : Integer; AValue : integer); 

begin
  If (FpinScore=AValue) then exit;
  FpinScore:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.Setrecipients(AIndex : Integer; AValue : TTimelineItemrecipients); 

begin
  If (Frecipients=AValue) then exit;
  Frecipients:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.SetsourceItemId(AIndex : Integer; AValue : string); 

begin
  If (FsourceItemId=AValue) then exit;
  FsourceItemId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.SetspeakableText(AIndex : Integer; AValue : string); 

begin
  If (FspeakableText=AValue) then exit;
  FspeakableText:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.SetspeakableType(AIndex : Integer; AValue : string); 

begin
  If (FspeakableType=AValue) then exit;
  FspeakableType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.Settext(AIndex : Integer; AValue : string); 

begin
  If (Ftext=AValue) then exit;
  Ftext:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTimelineItemattachments
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTimelineItemmenuItems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTimelineItemrecipients
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTimelineListResponse
  --------------------------------------------------------------------}


Procedure TTimelineListResponse.Setitems(AIndex : Integer; AValue : TTimelineListResponseitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTimelineListResponseitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TUserAction
  --------------------------------------------------------------------}


Procedure TUserAction.Setpayload(AIndex : Integer; AValue : string); 

begin
  If (Fpayload=AValue) then exit;
  Fpayload:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserAction.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TUserAction.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TUserData
  --------------------------------------------------------------------}


Procedure TUserData.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserData.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountsResource
  --------------------------------------------------------------------}


Class Function TAccountsResource.ResourceName : String;

begin
  Result:='accounts';
end;

Class Function TAccountsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TmirrorAPI;
end;

Function TAccountsResource.Insert(accountName: string; accountType: string; userToken: string; aAccount : TAccount) : TAccount;

Const
  _HTTPMethod = 'POST';
  _Path       = 'accounts/{userToken}/{accountType}/{accountName}';
  _Methodid   = 'mirror.accounts.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountName',accountName,'accountType',accountType,'userToken',userToken]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAccount,TAccount) as TAccount;
end;



{ --------------------------------------------------------------------
  TContactsResource
  --------------------------------------------------------------------}


Class Function TContactsResource.ResourceName : String;

begin
  Result:='contacts';
end;

Class Function TContactsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TmirrorAPI;
end;

Procedure TContactsResource.Delete(id: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'contacts/{id}';
  _Methodid   = 'mirror.contacts.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TContactsResource.Get(id: string) : TContact;

Const
  _HTTPMethod = 'GET';
  _Path       = 'contacts/{id}';
  _Methodid   = 'mirror.contacts.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TContact) as TContact;
end;

Function TContactsResource.Insert(aContact : TContact) : TContact;

Const
  _HTTPMethod = 'POST';
  _Path       = 'contacts';
  _Methodid   = 'mirror.contacts.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aContact,TContact) as TContact;
end;

Function TContactsResource.List : TContactsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'contacts';
  _Methodid   = 'mirror.contacts.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TContactsListResponse) as TContactsListResponse;
end;

Function TContactsResource.Patch(id: string; aContact : TContact) : TContact;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'contacts/{id}';
  _Methodid   = 'mirror.contacts.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aContact,TContact) as TContact;
end;

Function TContactsResource.Update(id: string; aContact : TContact) : TContact;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'contacts/{id}';
  _Methodid   = 'mirror.contacts.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aContact,TContact) as TContact;
end;



{ --------------------------------------------------------------------
  TLocationsResource
  --------------------------------------------------------------------}


Class Function TLocationsResource.ResourceName : String;

begin
  Result:='locations';
end;

Class Function TLocationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TmirrorAPI;
end;

Function TLocationsResource.Get(id: string) : TLocation;

Const
  _HTTPMethod = 'GET';
  _Path       = 'locations/{id}';
  _Methodid   = 'mirror.locations.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TLocation) as TLocation;
end;

Function TLocationsResource.List : TLocationsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'locations';
  _Methodid   = 'mirror.locations.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TLocationsListResponse) as TLocationsListResponse;
end;



{ --------------------------------------------------------------------
  TSettingsResource
  --------------------------------------------------------------------}


Class Function TSettingsResource.ResourceName : String;

begin
  Result:='settings';
end;

Class Function TSettingsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TmirrorAPI;
end;

Function TSettingsResource.Get(id: string) : TSetting;

Const
  _HTTPMethod = 'GET';
  _Path       = 'settings/{id}';
  _Methodid   = 'mirror.settings.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TSetting) as TSetting;
end;



{ --------------------------------------------------------------------
  TSubscriptionsResource
  --------------------------------------------------------------------}


Class Function TSubscriptionsResource.ResourceName : String;

begin
  Result:='subscriptions';
end;

Class Function TSubscriptionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TmirrorAPI;
end;

Procedure TSubscriptionsResource.Delete(id: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'subscriptions/{id}';
  _Methodid   = 'mirror.subscriptions.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TSubscriptionsResource.Insert(aSubscription : TSubscription) : TSubscription;

Const
  _HTTPMethod = 'POST';
  _Path       = 'subscriptions';
  _Methodid   = 'mirror.subscriptions.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aSubscription,TSubscription) as TSubscription;
end;

Function TSubscriptionsResource.List : TSubscriptionsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'subscriptions';
  _Methodid   = 'mirror.subscriptions.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TSubscriptionsListResponse) as TSubscriptionsListResponse;
end;

Function TSubscriptionsResource.Update(id: string; aSubscription : TSubscription) : TSubscription;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'subscriptions/{id}';
  _Methodid   = 'mirror.subscriptions.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aSubscription,TSubscription) as TSubscription;
end;



{ --------------------------------------------------------------------
  TTimelineResource
  --------------------------------------------------------------------}


Class Function TTimelineResource.ResourceName : String;

begin
  Result:='timeline';
end;

Class Function TTimelineResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TmirrorAPI;
end;

Procedure TTimelineResource.Delete(id: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'timeline/{id}';
  _Methodid   = 'mirror.timeline.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TTimelineResource.Get(id: string) : TTimelineItem;

Const
  _HTTPMethod = 'GET';
  _Path       = 'timeline/{id}';
  _Methodid   = 'mirror.timeline.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TTimelineItem) as TTimelineItem;
end;

Function TTimelineResource.Insert(aTimelineItem : TTimelineItem) : TTimelineItem;

Const
  _HTTPMethod = 'POST';
  _Path       = 'timeline';
  _Methodid   = 'mirror.timeline.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aTimelineItem,TTimelineItem) as TTimelineItem;
end;

Function TTimelineResource.List(AQuery : string = '') : TTimelineListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'timeline';
  _Methodid   = 'mirror.timeline.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TTimelineListResponse) as TTimelineListResponse;
end;


Function TTimelineResource.List(AQuery : TTimelinelistOptions) : TTimelineListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'bundleId',AQuery.bundleId);
  AddToQuery(_Q,'includeDeleted',AQuery.includeDeleted);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'orderBy',AQuery.orderBy);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'pinnedOnly',AQuery.pinnedOnly);
  AddToQuery(_Q,'sourceItemId',AQuery.sourceItemId);
  Result:=List(_Q);
end;

Function TTimelineResource.Patch(id: string; aTimelineItem : TTimelineItem) : TTimelineItem;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'timeline/{id}';
  _Methodid   = 'mirror.timeline.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTimelineItem,TTimelineItem) as TTimelineItem;
end;

Function TTimelineResource.Update(id: string; aTimelineItem : TTimelineItem) : TTimelineItem;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'timeline/{id}';
  _Methodid   = 'mirror.timeline.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTimelineItem,TTimelineItem) as TTimelineItem;
end;



{ --------------------------------------------------------------------
  TMirrorAPI
  --------------------------------------------------------------------}

Class Function TMirrorAPI.APIName : String;

begin
  Result:='mirror';
end;

Class Function TMirrorAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TMirrorAPI.APIRevision : String;

begin
  Result:='20150326';
end;

Class Function TMirrorAPI.APIID : String;

begin
  Result:='mirror:v1';
end;

Class Function TMirrorAPI.APITitle : String;

begin
  Result:='Google Mirror API';
end;

Class Function TMirrorAPI.APIDescription : String;

begin
  Result:='API for interacting with Glass users via the timeline.';
end;

Class Function TMirrorAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TMirrorAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TMirrorAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TMirrorAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TMirrorAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/glass';
end;

Class Function TMirrorAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TMirrorAPI.APIbasePath : string;

begin
  Result:='/mirror/v1/';
end;

Class Function TMirrorAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/mirror/v1/';
end;

Class Function TMirrorAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TMirrorAPI.APIservicePath : string;

begin
  Result:='mirror/v1/';
end;

Class Function TMirrorAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TMirrorAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,2);
  Result[0].Name:='https://www.googleapis.com/auth/glass.location';
  Result[0].Description:='View your location';
  Result[1].Name:='https://www.googleapis.com/auth/glass.timeline';
  Result[1].Description:='View and manage your Glass timeline';
  
end;

Class Function TMirrorAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TMirrorAPI.RegisterAPIResources;

begin
  TAccount.RegisterObject;
  TAccountauthTokens.RegisterObject;
  TAccountfeatures.RegisterObject;
  TAccountuserData.RegisterObject;
  TAttachment.RegisterObject;
  TAttachmentsListResponse.RegisterObject;
  TAttachmentsListResponseitems.RegisterObject;
  TAuthToken.RegisterObject;
  TCommand.RegisterObject;
  TContact.RegisterObject;
  TContactacceptCommands.RegisterObject;
  TContactacceptTypes.RegisterObject;
  TContactimageUrls.RegisterObject;
  TContactsharingFeatures.RegisterObject;
  TContactsListResponse.RegisterObject;
  TContactsListResponseitems.RegisterObject;
  TLocation.RegisterObject;
  TLocationsListResponse.RegisterObject;
  TLocationsListResponseitems.RegisterObject;
  TMenuItem.RegisterObject;
  TMenuItemvalues.RegisterObject;
  TMenuValue.RegisterObject;
  TNotification.RegisterObject;
  TNotificationuserActions.RegisterObject;
  TNotificationConfig.RegisterObject;
  TSetting.RegisterObject;
  TSubscription.RegisterObject;
  TSubscriptionoperation.RegisterObject;
  TSubscriptionsListResponse.RegisterObject;
  TSubscriptionsListResponseitems.RegisterObject;
  TTimelineItem.RegisterObject;
  TTimelineItemattachments.RegisterObject;
  TTimelineItemmenuItems.RegisterObject;
  TTimelineItemrecipients.RegisterObject;
  TTimelineListResponse.RegisterObject;
  TTimelineListResponseitems.RegisterObject;
  TUserAction.RegisterObject;
  TUserData.RegisterObject;
end;


Function TMirrorAPI.GetAccountsInstance : TAccountsResource;

begin
  if (FAccountsInstance=Nil) then
    FAccountsInstance:=CreateAccountsResource;
  Result:=FAccountsInstance;
end;

Function TMirrorAPI.CreateAccountsResource : TAccountsResource;

begin
  Result:=CreateAccountsResource(Self);
end;


Function TMirrorAPI.CreateAccountsResource(AOwner : TComponent) : TAccountsResource;

begin
  Result:=TAccountsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TMirrorAPI.GetContactsInstance : TContactsResource;

begin
  if (FContactsInstance=Nil) then
    FContactsInstance:=CreateContactsResource;
  Result:=FContactsInstance;
end;

Function TMirrorAPI.CreateContactsResource : TContactsResource;

begin
  Result:=CreateContactsResource(Self);
end;


Function TMirrorAPI.CreateContactsResource(AOwner : TComponent) : TContactsResource;

begin
  Result:=TContactsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TMirrorAPI.GetLocationsInstance : TLocationsResource;

begin
  if (FLocationsInstance=Nil) then
    FLocationsInstance:=CreateLocationsResource;
  Result:=FLocationsInstance;
end;

Function TMirrorAPI.CreateLocationsResource : TLocationsResource;

begin
  Result:=CreateLocationsResource(Self);
end;


Function TMirrorAPI.CreateLocationsResource(AOwner : TComponent) : TLocationsResource;

begin
  Result:=TLocationsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TMirrorAPI.GetSettingsInstance : TSettingsResource;

begin
  if (FSettingsInstance=Nil) then
    FSettingsInstance:=CreateSettingsResource;
  Result:=FSettingsInstance;
end;

Function TMirrorAPI.CreateSettingsResource : TSettingsResource;

begin
  Result:=CreateSettingsResource(Self);
end;


Function TMirrorAPI.CreateSettingsResource(AOwner : TComponent) : TSettingsResource;

begin
  Result:=TSettingsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TMirrorAPI.GetSubscriptionsInstance : TSubscriptionsResource;

begin
  if (FSubscriptionsInstance=Nil) then
    FSubscriptionsInstance:=CreateSubscriptionsResource;
  Result:=FSubscriptionsInstance;
end;

Function TMirrorAPI.CreateSubscriptionsResource : TSubscriptionsResource;

begin
  Result:=CreateSubscriptionsResource(Self);
end;


Function TMirrorAPI.CreateSubscriptionsResource(AOwner : TComponent) : TSubscriptionsResource;

begin
  Result:=TSubscriptionsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TMirrorAPI.GetTimelineInstance : TTimelineResource;

begin
  if (FTimelineInstance=Nil) then
    FTimelineInstance:=CreateTimelineResource;
  Result:=FTimelineInstance;
end;

Function TMirrorAPI.CreateTimelineResource : TTimelineResource;

begin
  Result:=CreateTimelineResource(Self);
end;


Function TMirrorAPI.CreateTimelineResource(AOwner : TComponent) : TTimelineResource;

begin
  Result:=TTimelineResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TMirrorAPI.RegisterAPI;
end.
