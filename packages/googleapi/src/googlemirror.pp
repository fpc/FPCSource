unit googlemirror;
{
   **********************************************************************
      This file is part of the Free Component Library (FCL)
      Copyright (c) 2015 The free pascal team.
  
      See the file COPYING.FPC, included in this distribution,
      for details about the copyright.
  
      This program is distributed in the hope that it will be useful,
      but WITHOUT ANY WARRANTY; without even the implied warranty of
      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  
   **********************************************************************
}
//Generated on: 16-5-15 08:53:06
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAccount = Class;
  TAttachment = Class;
  TAttachmentsListResponse = Class;
  TAuthToken = Class;
  TCommand = Class;
  TContact = Class;
  TContactsListResponse = Class;
  TLocation = Class;
  TLocationsListResponse = Class;
  TMenuItem = Class;
  TMenuValue = Class;
  TNotification = Class;
  TNotificationConfig = Class;
  TSetting = Class;
  TSubscription = Class;
  TSubscriptionsListResponse = Class;
  TTimelineItem = Class;
  TTimelineListResponse = Class;
  TUserAction = Class;
  TUserData = Class;
  TAccountArray = Array of TAccount;
  TAttachmentArray = Array of TAttachment;
  TAttachmentsListResponseArray = Array of TAttachmentsListResponse;
  TAuthTokenArray = Array of TAuthToken;
  TCommandArray = Array of TCommand;
  TContactArray = Array of TContact;
  TContactsListResponseArray = Array of TContactsListResponse;
  TLocationArray = Array of TLocation;
  TLocationsListResponseArray = Array of TLocationsListResponse;
  TMenuItemArray = Array of TMenuItem;
  TMenuValueArray = Array of TMenuValue;
  TNotificationArray = Array of TNotification;
  TNotificationConfigArray = Array of TNotificationConfig;
  TSettingArray = Array of TSetting;
  TSubscriptionArray = Array of TSubscription;
  TSubscriptionsListResponseArray = Array of TSubscriptionsListResponse;
  TTimelineItemArray = Array of TTimelineItem;
  TTimelineListResponseArray = Array of TTimelineListResponse;
  TUserActionArray = Array of TUserAction;
  TUserDataArray = Array of TUserData;
  //Anonymous types, using auto-generated names
  TAccountTypeauthTokensArray = Array of TAuthToken;
  TAccountTypeuserDataArray = Array of TUserData;
  TAttachmentsListResponseTypeitemsArray = Array of TAttachment;
  TContactTypeacceptCommandsArray = Array of TCommand;
  TContactsListResponseTypeitemsArray = Array of TContact;
  TLocationsListResponseTypeitemsArray = Array of TLocation;
  TMenuItemTypevaluesArray = Array of TMenuValue;
  TNotificationTypeuserActionsArray = Array of TUserAction;
  TSubscriptionsListResponseTypeitemsArray = Array of TSubscription;
  TTimelineItemTypeattachmentsArray = Array of TAttachment;
  TTimelineItemTypemenuItemsArray = Array of TMenuItem;
  TTimelineItemTyperecipientsArray = Array of TContact;
  TTimelineListResponseTypeitemsArray = Array of TTimelineItem;
  
  { --------------------------------------------------------------------
    TAccount
    --------------------------------------------------------------------}
  
  TAccount = Class(TGoogleBaseObject)
  Private
    FauthTokens : TAccountTypeauthTokensArray;
    Ffeatures : TStringArray;
    Fpassword : String;
    FuserData : TAccountTypeuserDataArray;
  Protected
    //Property setters
    Procedure SetauthTokens(AIndex : Integer; AValue : TAccountTypeauthTokensArray); virtual;
    Procedure Setfeatures(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setpassword(AIndex : Integer; AValue : String); virtual;
    Procedure SetuserData(AIndex : Integer; AValue : TAccountTypeuserDataArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property authTokens : TAccountTypeauthTokensArray Index 0 Read FauthTokens Write SetauthTokens;
    Property features : TStringArray Index 8 Read Ffeatures Write Setfeatures;
    Property password : String Index 16 Read Fpassword Write Setpassword;
    Property userData : TAccountTypeuserDataArray Index 24 Read FuserData Write SetuserData;
  end;
  TAccountClass = Class of TAccount;
  
  { --------------------------------------------------------------------
    TAttachment
    --------------------------------------------------------------------}
  
  TAttachment = Class(TGoogleBaseObject)
  Private
    FcontentType : String;
    FcontentUrl : String;
    Fid : String;
    FisProcessingContent : boolean;
  Protected
    //Property setters
    Procedure SetcontentType(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontentUrl(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetisProcessingContent(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property contentType : String Index 0 Read FcontentType Write SetcontentType;
    Property contentUrl : String Index 8 Read FcontentUrl Write SetcontentUrl;
    Property id : String Index 16 Read Fid Write Setid;
    Property isProcessingContent : boolean Index 24 Read FisProcessingContent Write SetisProcessingContent;
  end;
  TAttachmentClass = Class of TAttachment;
  
  { --------------------------------------------------------------------
    TAttachmentsListResponse
    --------------------------------------------------------------------}
  
  TAttachmentsListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TAttachmentsListResponseTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TAttachmentsListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TAttachmentsListResponseTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TAttachmentsListResponseClass = Class of TAttachmentsListResponse;
  
  { --------------------------------------------------------------------
    TAuthToken
    --------------------------------------------------------------------}
  
  TAuthToken = Class(TGoogleBaseObject)
  Private
    FauthToken : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetauthToken(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property authToken : String Index 0 Read FauthToken Write SetauthToken;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TAuthTokenClass = Class of TAuthToken;
  
  { --------------------------------------------------------------------
    TCommand
    --------------------------------------------------------------------}
  
  TCommand = Class(TGoogleBaseObject)
  Private
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property _type : String Index 0 Read F_type Write Set_type;
  end;
  TCommandClass = Class of TCommand;
  
  { --------------------------------------------------------------------
    TContact
    --------------------------------------------------------------------}
  
  TContact = Class(TGoogleBaseObject)
  Private
    FacceptCommands : TContactTypeacceptCommandsArray;
    FacceptTypes : TStringArray;
    FdisplayName : String;
    Fid : String;
    FimageUrls : TStringArray;
    Fkind : String;
    FphoneNumber : String;
    Fpriority : integer;
    FsharingFeatures : TStringArray;
    Fsource : String;
    FspeakableName : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetacceptCommands(AIndex : Integer; AValue : TContactTypeacceptCommandsArray); virtual;
    Procedure SetacceptTypes(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetdisplayName(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetimageUrls(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetphoneNumber(AIndex : Integer; AValue : String); virtual;
    Procedure Setpriority(AIndex : Integer; AValue : integer); virtual;
    Procedure SetsharingFeatures(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setsource(AIndex : Integer; AValue : String); virtual;
    Procedure SetspeakableName(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property acceptCommands : TContactTypeacceptCommandsArray Index 0 Read FacceptCommands Write SetacceptCommands;
    Property acceptTypes : TStringArray Index 8 Read FacceptTypes Write SetacceptTypes;
    Property displayName : String Index 16 Read FdisplayName Write SetdisplayName;
    Property id : String Index 24 Read Fid Write Setid;
    Property imageUrls : TStringArray Index 32 Read FimageUrls Write SetimageUrls;
    Property kind : String Index 40 Read Fkind Write Setkind;
    Property phoneNumber : String Index 48 Read FphoneNumber Write SetphoneNumber;
    Property priority : integer Index 56 Read Fpriority Write Setpriority;
    Property sharingFeatures : TStringArray Index 64 Read FsharingFeatures Write SetsharingFeatures;
    Property source : String Index 72 Read Fsource Write Setsource;
    Property speakableName : String Index 80 Read FspeakableName Write SetspeakableName;
    Property _type : String Index 88 Read F_type Write Set_type;
  end;
  TContactClass = Class of TContact;
  
  { --------------------------------------------------------------------
    TContactsListResponse
    --------------------------------------------------------------------}
  
  TContactsListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TContactsListResponseTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TContactsListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TContactsListResponseTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TContactsListResponseClass = Class of TContactsListResponse;
  
  { --------------------------------------------------------------------
    TLocation
    --------------------------------------------------------------------}
  
  TLocation = Class(TGoogleBaseObject)
  Private
    Faccuracy : double;
    Faddress : String;
    FdisplayName : String;
    Fid : String;
    Fkind : String;
    Flatitude : double;
    Flongitude : double;
    Ftimestamp : TDatetime;
  Protected
    //Property setters
    Procedure Setaccuracy(AIndex : Integer; AValue : double); virtual;
    Procedure Setaddress(AIndex : Integer; AValue : String); virtual;
    Procedure SetdisplayName(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlatitude(AIndex : Integer; AValue : double); virtual;
    Procedure Setlongitude(AIndex : Integer; AValue : double); virtual;
    Procedure Settimestamp(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property accuracy : double Index 0 Read Faccuracy Write Setaccuracy;
    Property address : String Index 8 Read Faddress Write Setaddress;
    Property displayName : String Index 16 Read FdisplayName Write SetdisplayName;
    Property id : String Index 24 Read Fid Write Setid;
    Property kind : String Index 32 Read Fkind Write Setkind;
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
    Fitems : TLocationsListResponseTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TLocationsListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TLocationsListResponseTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TLocationsListResponseClass = Class of TLocationsListResponse;
  
  { --------------------------------------------------------------------
    TMenuItem
    --------------------------------------------------------------------}
  
  TMenuItem = Class(TGoogleBaseObject)
  Private
    Faction : String;
    Fcontextual_command : String;
    Fid : String;
    Fpayload : String;
    FremoveWhenSelected : boolean;
    Fvalues : TMenuItemTypevaluesArray;
  Protected
    //Property setters
    Procedure Setaction(AIndex : Integer; AValue : String); virtual;
    Procedure Setcontextual_command(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setpayload(AIndex : Integer; AValue : String); virtual;
    Procedure SetremoveWhenSelected(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setvalues(AIndex : Integer; AValue : TMenuItemTypevaluesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property action : String Index 0 Read Faction Write Setaction;
    Property contextual_command : String Index 8 Read Fcontextual_command Write Setcontextual_command;
    Property id : String Index 16 Read Fid Write Setid;
    Property payload : String Index 24 Read Fpayload Write Setpayload;
    Property removeWhenSelected : boolean Index 32 Read FremoveWhenSelected Write SetremoveWhenSelected;
    Property values : TMenuItemTypevaluesArray Index 40 Read Fvalues Write Setvalues;
  end;
  TMenuItemClass = Class of TMenuItem;
  
  { --------------------------------------------------------------------
    TMenuValue
    --------------------------------------------------------------------}
  
  TMenuValue = Class(TGoogleBaseObject)
  Private
    FdisplayName : String;
    FiconUrl : String;
    Fstate : String;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : String); virtual;
    Procedure SeticonUrl(AIndex : Integer; AValue : String); virtual;
    Procedure Setstate(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property displayName : String Index 0 Read FdisplayName Write SetdisplayName;
    Property iconUrl : String Index 8 Read FiconUrl Write SeticonUrl;
    Property state : String Index 16 Read Fstate Write Setstate;
  end;
  TMenuValueClass = Class of TMenuValue;
  
  { --------------------------------------------------------------------
    TNotification
    --------------------------------------------------------------------}
  
  TNotification = Class(TGoogleBaseObject)
  Private
    Fcollection : String;
    FitemId : String;
    Foperation : String;
    FuserActions : TNotificationTypeuserActionsArray;
    FuserToken : String;
    FverifyToken : String;
  Protected
    //Property setters
    Procedure Setcollection(AIndex : Integer; AValue : String); virtual;
    Procedure SetitemId(AIndex : Integer; AValue : String); virtual;
    Procedure Setoperation(AIndex : Integer; AValue : String); virtual;
    Procedure SetuserActions(AIndex : Integer; AValue : TNotificationTypeuserActionsArray); virtual;
    Procedure SetuserToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetverifyToken(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property collection : String Index 0 Read Fcollection Write Setcollection;
    Property itemId : String Index 8 Read FitemId Write SetitemId;
    Property operation : String Index 16 Read Foperation Write Setoperation;
    Property userActions : TNotificationTypeuserActionsArray Index 24 Read FuserActions Write SetuserActions;
    Property userToken : String Index 32 Read FuserToken Write SetuserToken;
    Property verifyToken : String Index 40 Read FverifyToken Write SetverifyToken;
  end;
  TNotificationClass = Class of TNotification;
  
  { --------------------------------------------------------------------
    TNotificationConfig
    --------------------------------------------------------------------}
  
  TNotificationConfig = Class(TGoogleBaseObject)
  Private
    FdeliveryTime : TDatetime;
    Flevel : String;
  Protected
    //Property setters
    Procedure SetdeliveryTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setlevel(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property deliveryTime : TDatetime Index 0 Read FdeliveryTime Write SetdeliveryTime;
    Property level : String Index 8 Read Flevel Write Setlevel;
  end;
  TNotificationConfigClass = Class of TNotificationConfig;
  
  { --------------------------------------------------------------------
    TSetting
    --------------------------------------------------------------------}
  
  TSetting = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fkind : String;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property value : String Index 16 Read Fvalue Write Setvalue;
  end;
  TSettingClass = Class of TSetting;
  
  { --------------------------------------------------------------------
    TSubscription
    --------------------------------------------------------------------}
  
  TSubscription = Class(TGoogleBaseObject)
  Private
    FcallbackUrl : String;
    Fcollection : String;
    Fid : String;
    Fkind : String;
    Fnotification : TNotification;
    Foperation : TStringArray;
    Fupdated : TDatetime;
    FuserToken : String;
    FverifyToken : String;
  Protected
    //Property setters
    Procedure SetcallbackUrl(AIndex : Integer; AValue : String); virtual;
    Procedure Setcollection(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setnotification(AIndex : Integer; AValue : TNotification); virtual;
    Procedure Setoperation(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetuserToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetverifyToken(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property callbackUrl : String Index 0 Read FcallbackUrl Write SetcallbackUrl;
    Property collection : String Index 8 Read Fcollection Write Setcollection;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property notification : TNotification Index 32 Read Fnotification Write Setnotification;
    Property operation : TStringArray Index 40 Read Foperation Write Setoperation;
    Property updated : TDatetime Index 48 Read Fupdated Write Setupdated;
    Property userToken : String Index 56 Read FuserToken Write SetuserToken;
    Property verifyToken : String Index 64 Read FverifyToken Write SetverifyToken;
  end;
  TSubscriptionClass = Class of TSubscription;
  
  { --------------------------------------------------------------------
    TSubscriptionsListResponse
    --------------------------------------------------------------------}
  
  TSubscriptionsListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TSubscriptionsListResponseTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TSubscriptionsListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TSubscriptionsListResponseTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TSubscriptionsListResponseClass = Class of TSubscriptionsListResponse;
  
  { --------------------------------------------------------------------
    TTimelineItem
    --------------------------------------------------------------------}
  
  TTimelineItem = Class(TGoogleBaseObject)
  Private
    Fattachments : TTimelineItemTypeattachmentsArray;
    FbundleId : String;
    FcanonicalUrl : String;
    Fcreated : TDatetime;
    Fcreator : TContact;
    FdisplayTime : TDatetime;
    Fetag : String;
    Fhtml : String;
    Fid : String;
    FinReplyTo : String;
    FisBundleCover : boolean;
    FisDeleted : boolean;
    FisPinned : boolean;
    Fkind : String;
    Flocation : TLocation;
    FmenuItems : TTimelineItemTypemenuItemsArray;
    Fnotification : TNotificationConfig;
    FpinScore : integer;
    Frecipients : TTimelineItemTyperecipientsArray;
    FselfLink : String;
    FsourceItemId : String;
    FspeakableText : String;
    FspeakableType : String;
    Ftext : String;
    Ftitle : String;
    Fupdated : TDatetime;
  Protected
    //Property setters
    Procedure Setattachments(AIndex : Integer; AValue : TTimelineItemTypeattachmentsArray); virtual;
    Procedure SetbundleId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcanonicalUrl(AIndex : Integer; AValue : String); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setcreator(AIndex : Integer; AValue : TContact); virtual;
    Procedure SetdisplayTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Sethtml(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetinReplyTo(AIndex : Integer; AValue : String); virtual;
    Procedure SetisBundleCover(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetisDeleted(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetisPinned(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : TLocation); virtual;
    Procedure SetmenuItems(AIndex : Integer; AValue : TTimelineItemTypemenuItemsArray); virtual;
    Procedure Setnotification(AIndex : Integer; AValue : TNotificationConfig); virtual;
    Procedure SetpinScore(AIndex : Integer; AValue : integer); virtual;
    Procedure Setrecipients(AIndex : Integer; AValue : TTimelineItemTyperecipientsArray); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetsourceItemId(AIndex : Integer; AValue : String); virtual;
    Procedure SetspeakableText(AIndex : Integer; AValue : String); virtual;
    Procedure SetspeakableType(AIndex : Integer; AValue : String); virtual;
    Procedure Settext(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property attachments : TTimelineItemTypeattachmentsArray Index 0 Read Fattachments Write Setattachments;
    Property bundleId : String Index 8 Read FbundleId Write SetbundleId;
    Property canonicalUrl : String Index 16 Read FcanonicalUrl Write SetcanonicalUrl;
    Property created : TDatetime Index 24 Read Fcreated Write Setcreated;
    Property creator : TContact Index 32 Read Fcreator Write Setcreator;
    Property displayTime : TDatetime Index 40 Read FdisplayTime Write SetdisplayTime;
    Property etag : String Index 48 Read Fetag Write Setetag;
    Property html : String Index 56 Read Fhtml Write Sethtml;
    Property id : String Index 64 Read Fid Write Setid;
    Property inReplyTo : String Index 72 Read FinReplyTo Write SetinReplyTo;
    Property isBundleCover : boolean Index 80 Read FisBundleCover Write SetisBundleCover;
    Property isDeleted : boolean Index 88 Read FisDeleted Write SetisDeleted;
    Property isPinned : boolean Index 96 Read FisPinned Write SetisPinned;
    Property kind : String Index 104 Read Fkind Write Setkind;
    Property location : TLocation Index 112 Read Flocation Write Setlocation;
    Property menuItems : TTimelineItemTypemenuItemsArray Index 120 Read FmenuItems Write SetmenuItems;
    Property notification : TNotificationConfig Index 128 Read Fnotification Write Setnotification;
    Property pinScore : integer Index 136 Read FpinScore Write SetpinScore;
    Property recipients : TTimelineItemTyperecipientsArray Index 144 Read Frecipients Write Setrecipients;
    Property selfLink : String Index 152 Read FselfLink Write SetselfLink;
    Property sourceItemId : String Index 160 Read FsourceItemId Write SetsourceItemId;
    Property speakableText : String Index 168 Read FspeakableText Write SetspeakableText;
    Property speakableType : String Index 176 Read FspeakableType Write SetspeakableType;
    Property text : String Index 184 Read Ftext Write Settext;
    Property title : String Index 192 Read Ftitle Write Settitle;
    Property updated : TDatetime Index 200 Read Fupdated Write Setupdated;
  end;
  TTimelineItemClass = Class of TTimelineItem;
  
  { --------------------------------------------------------------------
    TTimelineListResponse
    --------------------------------------------------------------------}
  
  TTimelineListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TTimelineListResponseTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TTimelineListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TTimelineListResponseTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TTimelineListResponseClass = Class of TTimelineListResponse;
  
  { --------------------------------------------------------------------
    TUserAction
    --------------------------------------------------------------------}
  
  TUserAction = Class(TGoogleBaseObject)
  Private
    Fpayload : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setpayload(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property payload : String Index 0 Read Fpayload Write Setpayload;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TUserActionClass = Class of TUserAction;
  
  { --------------------------------------------------------------------
    TUserData
    --------------------------------------------------------------------}
  
  TUserData = Class(TGoogleBaseObject)
  Private
    Fkey : String;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property key : String Index 0 Read Fkey Write Setkey;
    Property value : String Index 8 Read Fvalue Write Setvalue;
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
    TTimelineAttachmentsResource
    --------------------------------------------------------------------}
  
  TTimelineAttachmentsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(attachmentId: string; itemId: string);
    Function Get(attachmentId: string; itemId: string) : TAttachment;
    Function Insert(itemId: string) : TAttachment;
    Function List(itemId: string) : TAttachmentsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TTimelineResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TTimelineResource, method List
  
  TTimelineListOptions = Record
    bundleId : String;
    includeDeleted : boolean;
    maxResults : integer;
    orderBy : String;
    pageToken : String;
    pinnedOnly : boolean;
    sourceItemId : String;
  end;
  
  TTimelineResource = Class(TGoogleResource)
  Private
    FAttachmentsInstance : TTimelineAttachmentsResource;
    Function GetAttachmentsInstance : TTimelineAttachmentsResource;virtual;
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
    Function CreateAttachmentsResource(AOwner : TComponent) : TTimelineAttachmentsResource;virtual;overload;
    Function CreateAttachmentsResource : TTimelineAttachmentsResource;virtual;overload;
    Property AttachmentsResource : TTimelineAttachmentsResource Read GetAttachmentsInstance;
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
    FTimelineAttachmentsInstance : TTimelineAttachmentsResource;
    FTimelineInstance : TTimelineResource;
    Function GetAccountsInstance : TAccountsResource;virtual;
    Function GetContactsInstance : TContactsResource;virtual;
    Function GetLocationsInstance : TLocationsResource;virtual;
    Function GetSettingsInstance : TSettingsResource;virtual;
    Function GetSubscriptionsInstance : TSubscriptionsResource;virtual;
    Function GetTimelineAttachmentsInstance : TTimelineAttachmentsResource;virtual;
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
    Function CreateTimelineAttachmentsResource(AOwner : TComponent) : TTimelineAttachmentsResource;virtual;overload;
    Function CreateTimelineAttachmentsResource : TTimelineAttachmentsResource;virtual;overload;
    Function CreateTimelineResource(AOwner : TComponent) : TTimelineResource;virtual;overload;
    Function CreateTimelineResource : TTimelineResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AccountsResource : TAccountsResource Read GetAccountsInstance;
    Property ContactsResource : TContactsResource Read GetContactsInstance;
    Property LocationsResource : TLocationsResource Read GetLocationsInstance;
    Property SettingsResource : TSettingsResource Read GetSettingsInstance;
    Property SubscriptionsResource : TSubscriptionsResource Read GetSubscriptionsInstance;
    Property TimelineAttachmentsResource : TTimelineAttachmentsResource Read GetTimelineAttachmentsInstance;
    Property TimelineResource : TTimelineResource Read GetTimelineInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAccount
  --------------------------------------------------------------------}


Procedure TAccount.SetauthTokens(AIndex : Integer; AValue : TAccountTypeauthTokensArray); 

begin
  If (FauthTokens=AValue) then exit;
  FauthTokens:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setfeatures(AIndex : Integer; AValue : TStringArray); 

begin
  If (Ffeatures=AValue) then exit;
  Ffeatures:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setpassword(AIndex : Integer; AValue : String); 

begin
  If (Fpassword=AValue) then exit;
  Fpassword:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetuserData(AIndex : Integer; AValue : TAccountTypeuserDataArray); 

begin
  If (FuserData=AValue) then exit;
  FuserData:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccount.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'authtokens' : SetLength(FauthTokens,ALength);
  'features' : SetLength(Ffeatures,ALength);
  'userdata' : SetLength(FuserData,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAttachment
  --------------------------------------------------------------------}


Procedure TAttachment.SetcontentType(AIndex : Integer; AValue : String); 

begin
  If (FcontentType=AValue) then exit;
  FcontentType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAttachment.SetcontentUrl(AIndex : Integer; AValue : String); 

begin
  If (FcontentUrl=AValue) then exit;
  FcontentUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAttachment.Setid(AIndex : Integer; AValue : String); 

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


Procedure TAttachmentsListResponse.Setitems(AIndex : Integer; AValue : TAttachmentsListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAttachmentsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAttachmentsListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAuthToken
  --------------------------------------------------------------------}


Procedure TAuthToken.SetauthToken(AIndex : Integer; AValue : String); 

begin
  If (FauthToken=AValue) then exit;
  FauthToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAuthToken.Set_type(AIndex : Integer; AValue : String); 

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


Procedure TCommand.Set_type(AIndex : Integer; AValue : String); 

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


Procedure TContact.SetacceptCommands(AIndex : Integer; AValue : TContactTypeacceptCommandsArray); 

begin
  If (FacceptCommands=AValue) then exit;
  FacceptCommands:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContact.SetacceptTypes(AIndex : Integer; AValue : TStringArray); 

begin
  If (FacceptTypes=AValue) then exit;
  FacceptTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContact.SetdisplayName(AIndex : Integer; AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContact.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContact.SetimageUrls(AIndex : Integer; AValue : TStringArray); 

begin
  If (FimageUrls=AValue) then exit;
  FimageUrls:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContact.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContact.SetphoneNumber(AIndex : Integer; AValue : String); 

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



Procedure TContact.SetsharingFeatures(AIndex : Integer; AValue : TStringArray); 

begin
  If (FsharingFeatures=AValue) then exit;
  FsharingFeatures:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContact.Setsource(AIndex : Integer; AValue : String); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContact.SetspeakableName(AIndex : Integer; AValue : String); 

begin
  If (FspeakableName=AValue) then exit;
  FspeakableName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContact.Set_type(AIndex : Integer; AValue : String); 

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

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TContact.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'acceptcommands' : SetLength(FacceptCommands,ALength);
  'accepttypes' : SetLength(FacceptTypes,ALength);
  'imageurls' : SetLength(FimageUrls,ALength);
  'sharingfeatures' : SetLength(FsharingFeatures,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TContactsListResponse
  --------------------------------------------------------------------}


Procedure TContactsListResponse.Setitems(AIndex : Integer; AValue : TContactsListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContactsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TContactsListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TLocation
  --------------------------------------------------------------------}


Procedure TLocation.Setaccuracy(AIndex : Integer; AValue : double); 

begin
  If (Faccuracy=AValue) then exit;
  Faccuracy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocation.Setaddress(AIndex : Integer; AValue : String); 

begin
  If (Faddress=AValue) then exit;
  Faddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocation.SetdisplayName(AIndex : Integer; AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocation.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocation.Setkind(AIndex : Integer; AValue : String); 

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


Procedure TLocationsListResponse.Setitems(AIndex : Integer; AValue : TLocationsListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocationsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TLocationsListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMenuItem
  --------------------------------------------------------------------}


Procedure TMenuItem.Setaction(AIndex : Integer; AValue : String); 

begin
  If (Faction=AValue) then exit;
  Faction:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMenuItem.Setcontextual_command(AIndex : Integer; AValue : String); 

begin
  If (Fcontextual_command=AValue) then exit;
  Fcontextual_command:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMenuItem.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMenuItem.Setpayload(AIndex : Integer; AValue : String); 

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



Procedure TMenuItem.Setvalues(AIndex : Integer; AValue : TMenuItemTypevaluesArray); 

begin
  If (Fvalues=AValue) then exit;
  Fvalues:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TMenuItem.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'values' : SetLength(Fvalues,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMenuValue
  --------------------------------------------------------------------}


Procedure TMenuValue.SetdisplayName(AIndex : Integer; AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMenuValue.SeticonUrl(AIndex : Integer; AValue : String); 

begin
  If (FiconUrl=AValue) then exit;
  FiconUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMenuValue.Setstate(AIndex : Integer; AValue : String); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TNotification
  --------------------------------------------------------------------}


Procedure TNotification.Setcollection(AIndex : Integer; AValue : String); 

begin
  If (Fcollection=AValue) then exit;
  Fcollection:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNotification.SetitemId(AIndex : Integer; AValue : String); 

begin
  If (FitemId=AValue) then exit;
  FitemId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNotification.Setoperation(AIndex : Integer; AValue : String); 

begin
  If (Foperation=AValue) then exit;
  Foperation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNotification.SetuserActions(AIndex : Integer; AValue : TNotificationTypeuserActionsArray); 

begin
  If (FuserActions=AValue) then exit;
  FuserActions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNotification.SetuserToken(AIndex : Integer; AValue : String); 

begin
  If (FuserToken=AValue) then exit;
  FuserToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNotification.SetverifyToken(AIndex : Integer; AValue : String); 

begin
  If (FverifyToken=AValue) then exit;
  FverifyToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TNotification.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'useractions' : SetLength(FuserActions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TNotificationConfig
  --------------------------------------------------------------------}


Procedure TNotificationConfig.SetdeliveryTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FdeliveryTime=AValue) then exit;
  FdeliveryTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNotificationConfig.Setlevel(AIndex : Integer; AValue : String); 

begin
  If (Flevel=AValue) then exit;
  Flevel:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSetting
  --------------------------------------------------------------------}


Procedure TSetting.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetting.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetting.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSubscription
  --------------------------------------------------------------------}


Procedure TSubscription.SetcallbackUrl(AIndex : Integer; AValue : String); 

begin
  If (FcallbackUrl=AValue) then exit;
  FcallbackUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.Setcollection(AIndex : Integer; AValue : String); 

begin
  If (Fcollection=AValue) then exit;
  Fcollection:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.Setkind(AIndex : Integer; AValue : String); 

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



Procedure TSubscription.Setoperation(AIndex : Integer; AValue : TStringArray); 

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



Procedure TSubscription.SetuserToken(AIndex : Integer; AValue : String); 

begin
  If (FuserToken=AValue) then exit;
  FuserToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscription.SetverifyToken(AIndex : Integer; AValue : String); 

begin
  If (FverifyToken=AValue) then exit;
  FverifyToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSubscription.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'operation' : SetLength(Foperation,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSubscriptionsListResponse
  --------------------------------------------------------------------}


Procedure TSubscriptionsListResponse.Setitems(AIndex : Integer; AValue : TSubscriptionsListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSubscriptionsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSubscriptionsListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTimelineItem
  --------------------------------------------------------------------}


Procedure TTimelineItem.Setattachments(AIndex : Integer; AValue : TTimelineItemTypeattachmentsArray); 

begin
  If (Fattachments=AValue) then exit;
  Fattachments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.SetbundleId(AIndex : Integer; AValue : String); 

begin
  If (FbundleId=AValue) then exit;
  FbundleId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.SetcanonicalUrl(AIndex : Integer; AValue : String); 

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



Procedure TTimelineItem.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.Sethtml(AIndex : Integer; AValue : String); 

begin
  If (Fhtml=AValue) then exit;
  Fhtml:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.SetinReplyTo(AIndex : Integer; AValue : String); 

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



Procedure TTimelineItem.Setkind(AIndex : Integer; AValue : String); 

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



Procedure TTimelineItem.SetmenuItems(AIndex : Integer; AValue : TTimelineItemTypemenuItemsArray); 

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



Procedure TTimelineItem.Setrecipients(AIndex : Integer; AValue : TTimelineItemTyperecipientsArray); 

begin
  If (Frecipients=AValue) then exit;
  Frecipients:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.SetsourceItemId(AIndex : Integer; AValue : String); 

begin
  If (FsourceItemId=AValue) then exit;
  FsourceItemId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.SetspeakableText(AIndex : Integer; AValue : String); 

begin
  If (FspeakableText=AValue) then exit;
  FspeakableText:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.SetspeakableType(AIndex : Integer; AValue : String); 

begin
  If (FspeakableType=AValue) then exit;
  FspeakableType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.Settext(AIndex : Integer; AValue : String); 

begin
  If (Ftext=AValue) then exit;
  Ftext:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineItem.Settitle(AIndex : Integer; AValue : String); 

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


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTimelineItem.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'attachments' : SetLength(Fattachments,ALength);
  'menuitems' : SetLength(FmenuItems,ALength);
  'recipients' : SetLength(Frecipients,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTimelineListResponse
  --------------------------------------------------------------------}


Procedure TTimelineListResponse.Setitems(AIndex : Integer; AValue : TTimelineListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimelineListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTimelineListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TUserAction
  --------------------------------------------------------------------}


Procedure TUserAction.Setpayload(AIndex : Integer; AValue : String); 

begin
  If (Fpayload=AValue) then exit;
  Fpayload:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserAction.Set_type(AIndex : Integer; AValue : String); 

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


Procedure TUserData.Setkey(AIndex : Integer; AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserData.Setvalue(AIndex : Integer; AValue : String); 

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
  TTimelineAttachmentsResource
  --------------------------------------------------------------------}


Class Function TTimelineAttachmentsResource.ResourceName : String;

begin
  Result:='attachments';
end;

Class Function TTimelineAttachmentsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TmirrorAPI;
end;

Procedure TTimelineAttachmentsResource.Delete(attachmentId: string; itemId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'timeline/{itemId}/attachments/{attachmentId}';
  _Methodid   = 'mirror.timeline.attachments.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['attachmentId',attachmentId,'itemId',itemId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TTimelineAttachmentsResource.Get(attachmentId: string; itemId: string) : TAttachment;

Const
  _HTTPMethod = 'GET';
  _Path       = 'timeline/{itemId}/attachments/{attachmentId}';
  _Methodid   = 'mirror.timeline.attachments.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['attachmentId',attachmentId,'itemId',itemId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAttachment) as TAttachment;
end;

Function TTimelineAttachmentsResource.Insert(itemId: string) : TAttachment;

Const
  _HTTPMethod = 'POST';
  _Path       = 'timeline/{itemId}/attachments';
  _Methodid   = 'mirror.timeline.attachments.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['itemId',itemId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAttachment) as TAttachment;
end;

Function TTimelineAttachmentsResource.List(itemId: string) : TAttachmentsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'timeline/{itemId}/attachments';
  _Methodid   = 'mirror.timeline.attachments.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['itemId',itemId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAttachmentsListResponse) as TAttachmentsListResponse;
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



Function TTimelineResource.GetAttachmentsInstance : TTimelineAttachmentsResource;

begin
  if (FAttachmentsInstance=Nil) then
    FAttachmentsInstance:=CreateAttachmentsResource;
  Result:=FAttachmentsInstance;
end;

Function TTimelineResource.CreateAttachmentsResource : TTimelineAttachmentsResource;

begin
  Result:=CreateAttachmentsResource(Self);
end;


Function TTimelineResource.CreateAttachmentsResource(AOwner : TComponent) : TTimelineAttachmentsResource;

begin
  Result:=TTimelineAttachmentsResource.Create(AOwner);
  Result.API:=Self.API;
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
  Result:='https://www.googleapis.com:443/';
end;

Class Function TMirrorAPI.APIbasePath : string;

begin
  Result:='/mirror/v1/';
end;

Class Function TMirrorAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/mirror/v1/';
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
  TAttachment.RegisterObject;
  TAttachmentsListResponse.RegisterObject;
  TAuthToken.RegisterObject;
  TCommand.RegisterObject;
  TContact.RegisterObject;
  TContactsListResponse.RegisterObject;
  TLocation.RegisterObject;
  TLocationsListResponse.RegisterObject;
  TMenuItem.RegisterObject;
  TMenuValue.RegisterObject;
  TNotification.RegisterObject;
  TNotificationConfig.RegisterObject;
  TSetting.RegisterObject;
  TSubscription.RegisterObject;
  TSubscriptionsListResponse.RegisterObject;
  TTimelineItem.RegisterObject;
  TTimelineListResponse.RegisterObject;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
end;



Function TMirrorAPI.GetTimelineAttachmentsInstance : TTimelineAttachmentsResource;

begin
  if (FTimelineAttachmentsInstance=Nil) then
    FTimelineAttachmentsInstance:=CreateTimelineAttachmentsResource;
  Result:=FTimelineAttachmentsInstance;
end;

Function TMirrorAPI.CreateTimelineAttachmentsResource : TTimelineAttachmentsResource;

begin
  Result:=CreateTimelineAttachmentsResource(Self);
end;


Function TMirrorAPI.CreateTimelineAttachmentsResource(AOwner : TComponent) : TTimelineAttachmentsResource;

begin
  Result:=TTimelineAttachmentsResource.Create(AOwner);
  Result.API:=Self.API;
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
  Result.API:=Self.API;
end;



initialization
  TMirrorAPI.RegisterAPI;
end.
