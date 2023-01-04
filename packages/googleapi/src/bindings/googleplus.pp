unit googleplus;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAcl = Class;
  TActivity = Class;
  TActivityFeed = Class;
  TComment = Class;
  TCommentFeed = Class;
  TPeopleFeed = Class;
  TPerson = Class;
  TPlace = Class;
  TPlusAclentryResource = Class;
  TAclArray = Array of TAcl;
  TActivityArray = Array of TActivity;
  TActivityFeedArray = Array of TActivityFeed;
  TCommentArray = Array of TComment;
  TCommentFeedArray = Array of TCommentFeed;
  TPeopleFeedArray = Array of TPeopleFeed;
  TPersonArray = Array of TPerson;
  TPlaceArray = Array of TPlace;
  TPlusAclentryResourceArray = Array of TPlusAclentryResource;
  //Anonymous types, using auto-generated names
  TActivityTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo = Class;
  TActivityTypeactorTypeclientSpecificActorInfo = Class;
  TActivityTypeactorTypeimage = Class;
  TActivityTypeactorTypename = Class;
  TActivityTypeactorTypeverification = Class;
  TActivityTypeactor = Class;
  TActivityTypeobjectTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo = Class;
  TActivityTypeobjectTypeactorTypeclientSpecificActorInfo = Class;
  TActivityTypeobjectTypeactorTypeimage = Class;
  TActivityTypeobjectTypeactorTypeverification = Class;
  TActivityTypeobjectTypeactor = Class;
  TActivityTypeobjectTypeattachmentsItemTypeembed = Class;
  TActivityTypeobjectTypeattachmentsItemTypefullImage = Class;
  TActivityTypeobjectTypeattachmentsItemTypeimage = Class;
  TActivityTypeobjectTypeattachmentsItemTypethumbnailsItemTypeimage = Class;
  TActivityTypeobjectTypeattachmentsItemTypethumbnailsItem = Class;
  TActivityTypeobjectTypeattachmentsItem = Class;
  TActivityTypeobjectTypeplusoners = Class;
  TActivityTypeobjectTypereplies = Class;
  TActivityTypeobjectTyperesharers = Class;
  TActivityTypeobject = Class;
  TActivityTypeprovider = Class;
  TCommentTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo = Class;
  TCommentTypeactorTypeclientSpecificActorInfo = Class;
  TCommentTypeactorTypeimage = Class;
  TCommentTypeactorTypeverification = Class;
  TCommentTypeactor = Class;
  TCommentTypeinReplyToItem = Class;
  TCommentTypeobject = Class;
  TCommentTypeplusoners = Class;
  TPersonTypeageRange = Class;
  TPersonTypecoverTypecoverInfo = Class;
  TPersonTypecoverTypecoverPhoto = Class;
  TPersonTypecover = Class;
  TPersonTypeemailsItem = Class;
  TPersonTypeimage = Class;
  TPersonTypename = Class;
  TPersonTypeorganizationsItem = Class;
  TPersonTypeplacesLivedItem = Class;
  TPersonTypeurlsItem = Class;
  TPlaceTypeaddress = Class;
  TPlaceTypeposition = Class;
  TAclTypeitemsArray = Array of TPlusAclentryResource;
  TActivityTypeobjectTypeattachmentsItemTypethumbnailsArray = Array of TActivityTypeobjectTypeattachmentsItemTypethumbnailsItem;
  TActivityTypeobjectTypeattachmentsArray = Array of TActivityTypeobjectTypeattachmentsItem;
  TActivityFeedTypeitemsArray = Array of TActivity;
  TCommentTypeinReplyToArray = Array of TCommentTypeinReplyToItem;
  TCommentFeedTypeitemsArray = Array of TComment;
  TPeopleFeedTypeitemsArray = Array of TPerson;
  TPersonTypeemailsArray = Array of TPersonTypeemailsItem;
  TPersonTypeorganizationsArray = Array of TPersonTypeorganizationsItem;
  TPersonTypeplacesLivedArray = Array of TPersonTypeplacesLivedItem;
  TPersonTypeurlsArray = Array of TPersonTypeurlsItem;
  
  { --------------------------------------------------------------------
    TAcl
    --------------------------------------------------------------------}
  
  TAcl = Class(TGoogleBaseObject)
  Private
    Fdescription : String;
    Fitems : TAclTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; const AValue : TAclTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property description : String Index 0 Read Fdescription Write Setdescription;
    Property items : TAclTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
  end;
  TAclClass = Class of TAcl;
  
  { --------------------------------------------------------------------
    TActivityTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo
    --------------------------------------------------------------------}
  
  TActivityTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo = Class(TGoogleBaseObject)
  Private
    FchannelId : String;
  Protected
    //Property setters
    Procedure SetchannelId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property channelId : String Index 0 Read FchannelId Write SetchannelId;
  end;
  TActivityTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfoClass = Class of TActivityTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo;
  
  { --------------------------------------------------------------------
    TActivityTypeactorTypeclientSpecificActorInfo
    --------------------------------------------------------------------}
  
  TActivityTypeactorTypeclientSpecificActorInfo = Class(TGoogleBaseObject)
  Private
    FyoutubeActorInfo : TActivityTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo;
  Protected
    //Property setters
    Procedure SetyoutubeActorInfo(AIndex : Integer; const AValue : TActivityTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo); virtual;
  Public
  Published
    Property youtubeActorInfo : TActivityTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo Index 0 Read FyoutubeActorInfo Write SetyoutubeActorInfo;
  end;
  TActivityTypeactorTypeclientSpecificActorInfoClass = Class of TActivityTypeactorTypeclientSpecificActorInfo;
  
  { --------------------------------------------------------------------
    TActivityTypeactorTypeimage
    --------------------------------------------------------------------}
  
  TActivityTypeactorTypeimage = Class(TGoogleBaseObject)
  Private
    Furl : String;
  Protected
    //Property setters
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property url : String Index 0 Read Furl Write Seturl;
  end;
  TActivityTypeactorTypeimageClass = Class of TActivityTypeactorTypeimage;
  
  { --------------------------------------------------------------------
    TActivityTypeactorTypename
    --------------------------------------------------------------------}
  
  TActivityTypeactorTypename = Class(TGoogleBaseObject)
  Private
    FfamilyName : String;
    FgivenName : String;
  Protected
    //Property setters
    Procedure SetfamilyName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetgivenName(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property familyName : String Index 0 Read FfamilyName Write SetfamilyName;
    Property givenName : String Index 8 Read FgivenName Write SetgivenName;
  end;
  TActivityTypeactorTypenameClass = Class of TActivityTypeactorTypename;
  
  { --------------------------------------------------------------------
    TActivityTypeactorTypeverification
    --------------------------------------------------------------------}
  
  TActivityTypeactorTypeverification = Class(TGoogleBaseObject)
  Private
    FadHocVerified : String;
  Protected
    //Property setters
    Procedure SetadHocVerified(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property adHocVerified : String Index 0 Read FadHocVerified Write SetadHocVerified;
  end;
  TActivityTypeactorTypeverificationClass = Class of TActivityTypeactorTypeverification;
  
  { --------------------------------------------------------------------
    TActivityTypeactor
    --------------------------------------------------------------------}
  
  TActivityTypeactor = Class(TGoogleBaseObject)
  Private
    FclientSpecificActorInfo : TActivityTypeactorTypeclientSpecificActorInfo;
    FdisplayName : String;
    Fid : String;
    Fimage : TActivityTypeactorTypeimage;
    Fname : TActivityTypeactorTypename;
    Furl : String;
    Fverification : TActivityTypeactorTypeverification;
  Protected
    //Property setters
    Procedure SetclientSpecificActorInfo(AIndex : Integer; const AValue : TActivityTypeactorTypeclientSpecificActorInfo); virtual;
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setimage(AIndex : Integer; const AValue : TActivityTypeactorTypeimage); virtual;
    Procedure Setname(AIndex : Integer; const AValue : TActivityTypeactorTypename); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setverification(AIndex : Integer; const AValue : TActivityTypeactorTypeverification); virtual;
  Public
  Published
    Property clientSpecificActorInfo : TActivityTypeactorTypeclientSpecificActorInfo Index 0 Read FclientSpecificActorInfo Write SetclientSpecificActorInfo;
    Property displayName : String Index 8 Read FdisplayName Write SetdisplayName;
    Property id : String Index 16 Read Fid Write Setid;
    Property image : TActivityTypeactorTypeimage Index 24 Read Fimage Write Setimage;
    Property name : TActivityTypeactorTypename Index 32 Read Fname Write Setname;
    Property url : String Index 40 Read Furl Write Seturl;
    Property verification : TActivityTypeactorTypeverification Index 48 Read Fverification Write Setverification;
  end;
  TActivityTypeactorClass = Class of TActivityTypeactor;
  
  { --------------------------------------------------------------------
    TActivityTypeobjectTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo
    --------------------------------------------------------------------}
  
  TActivityTypeobjectTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo = Class(TGoogleBaseObject)
  Private
    FchannelId : String;
  Protected
    //Property setters
    Procedure SetchannelId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property channelId : String Index 0 Read FchannelId Write SetchannelId;
  end;
  TActivityTypeobjectTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfoClass = Class of TActivityTypeobjectTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo;
  
  { --------------------------------------------------------------------
    TActivityTypeobjectTypeactorTypeclientSpecificActorInfo
    --------------------------------------------------------------------}
  
  TActivityTypeobjectTypeactorTypeclientSpecificActorInfo = Class(TGoogleBaseObject)
  Private
    FyoutubeActorInfo : TActivityTypeobjectTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo;
  Protected
    //Property setters
    Procedure SetyoutubeActorInfo(AIndex : Integer; const AValue : TActivityTypeobjectTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo); virtual;
  Public
  Published
    Property youtubeActorInfo : TActivityTypeobjectTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo Index 0 Read FyoutubeActorInfo Write SetyoutubeActorInfo;
  end;
  TActivityTypeobjectTypeactorTypeclientSpecificActorInfoClass = Class of TActivityTypeobjectTypeactorTypeclientSpecificActorInfo;
  
  { --------------------------------------------------------------------
    TActivityTypeobjectTypeactorTypeimage
    --------------------------------------------------------------------}
  
  TActivityTypeobjectTypeactorTypeimage = Class(TGoogleBaseObject)
  Private
    Furl : String;
  Protected
    //Property setters
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property url : String Index 0 Read Furl Write Seturl;
  end;
  TActivityTypeobjectTypeactorTypeimageClass = Class of TActivityTypeobjectTypeactorTypeimage;
  
  { --------------------------------------------------------------------
    TActivityTypeobjectTypeactorTypeverification
    --------------------------------------------------------------------}
  
  TActivityTypeobjectTypeactorTypeverification = Class(TGoogleBaseObject)
  Private
    FadHocVerified : String;
  Protected
    //Property setters
    Procedure SetadHocVerified(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property adHocVerified : String Index 0 Read FadHocVerified Write SetadHocVerified;
  end;
  TActivityTypeobjectTypeactorTypeverificationClass = Class of TActivityTypeobjectTypeactorTypeverification;
  
  { --------------------------------------------------------------------
    TActivityTypeobjectTypeactor
    --------------------------------------------------------------------}
  
  TActivityTypeobjectTypeactor = Class(TGoogleBaseObject)
  Private
    FclientSpecificActorInfo : TActivityTypeobjectTypeactorTypeclientSpecificActorInfo;
    FdisplayName : String;
    Fid : String;
    Fimage : TActivityTypeobjectTypeactorTypeimage;
    Furl : String;
    Fverification : TActivityTypeobjectTypeactorTypeverification;
  Protected
    //Property setters
    Procedure SetclientSpecificActorInfo(AIndex : Integer; const AValue : TActivityTypeobjectTypeactorTypeclientSpecificActorInfo); virtual;
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setimage(AIndex : Integer; const AValue : TActivityTypeobjectTypeactorTypeimage); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setverification(AIndex : Integer; const AValue : TActivityTypeobjectTypeactorTypeverification); virtual;
  Public
  Published
    Property clientSpecificActorInfo : TActivityTypeobjectTypeactorTypeclientSpecificActorInfo Index 0 Read FclientSpecificActorInfo Write SetclientSpecificActorInfo;
    Property displayName : String Index 8 Read FdisplayName Write SetdisplayName;
    Property id : String Index 16 Read Fid Write Setid;
    Property image : TActivityTypeobjectTypeactorTypeimage Index 24 Read Fimage Write Setimage;
    Property url : String Index 32 Read Furl Write Seturl;
    Property verification : TActivityTypeobjectTypeactorTypeverification Index 40 Read Fverification Write Setverification;
  end;
  TActivityTypeobjectTypeactorClass = Class of TActivityTypeobjectTypeactor;
  
  { --------------------------------------------------------------------
    TActivityTypeobjectTypeattachmentsItemTypeembed
    --------------------------------------------------------------------}
  
  TActivityTypeobjectTypeattachmentsItemTypeembed = Class(TGoogleBaseObject)
  Private
    F_type : String;
    Furl : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property _type : String Index 0 Read F_type Write Set_type;
    Property url : String Index 8 Read Furl Write Seturl;
  end;
  TActivityTypeobjectTypeattachmentsItemTypeembedClass = Class of TActivityTypeobjectTypeattachmentsItemTypeembed;
  
  { --------------------------------------------------------------------
    TActivityTypeobjectTypeattachmentsItemTypefullImage
    --------------------------------------------------------------------}
  
  TActivityTypeobjectTypeattachmentsItemTypefullImage = Class(TGoogleBaseObject)
  Private
    Fheight : integer;
    F_type : String;
    Furl : String;
    Fwidth : integer;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setheight(AIndex : Integer; const AValue : integer); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setwidth(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property height : integer Index 0 Read Fheight Write Setheight;
    Property _type : String Index 8 Read F_type Write Set_type;
    Property url : String Index 16 Read Furl Write Seturl;
    Property width : integer Index 24 Read Fwidth Write Setwidth;
  end;
  TActivityTypeobjectTypeattachmentsItemTypefullImageClass = Class of TActivityTypeobjectTypeattachmentsItemTypefullImage;
  
  { --------------------------------------------------------------------
    TActivityTypeobjectTypeattachmentsItemTypeimage
    --------------------------------------------------------------------}
  
  TActivityTypeobjectTypeattachmentsItemTypeimage = Class(TGoogleBaseObject)
  Private
    Fheight : integer;
    F_type : String;
    Furl : String;
    Fwidth : integer;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setheight(AIndex : Integer; const AValue : integer); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setwidth(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property height : integer Index 0 Read Fheight Write Setheight;
    Property _type : String Index 8 Read F_type Write Set_type;
    Property url : String Index 16 Read Furl Write Seturl;
    Property width : integer Index 24 Read Fwidth Write Setwidth;
  end;
  TActivityTypeobjectTypeattachmentsItemTypeimageClass = Class of TActivityTypeobjectTypeattachmentsItemTypeimage;
  
  { --------------------------------------------------------------------
    TActivityTypeobjectTypeattachmentsItemTypethumbnailsItemTypeimage
    --------------------------------------------------------------------}
  
  TActivityTypeobjectTypeattachmentsItemTypethumbnailsItemTypeimage = Class(TGoogleBaseObject)
  Private
    Fheight : integer;
    F_type : String;
    Furl : String;
    Fwidth : integer;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setheight(AIndex : Integer; const AValue : integer); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setwidth(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property height : integer Index 0 Read Fheight Write Setheight;
    Property _type : String Index 8 Read F_type Write Set_type;
    Property url : String Index 16 Read Furl Write Seturl;
    Property width : integer Index 24 Read Fwidth Write Setwidth;
  end;
  TActivityTypeobjectTypeattachmentsItemTypethumbnailsItemTypeimageClass = Class of TActivityTypeobjectTypeattachmentsItemTypethumbnailsItemTypeimage;
  
  { --------------------------------------------------------------------
    TActivityTypeobjectTypeattachmentsItemTypethumbnailsItem
    --------------------------------------------------------------------}
  
  TActivityTypeobjectTypeattachmentsItemTypethumbnailsItem = Class(TGoogleBaseObject)
  Private
    Fdescription : String;
    Fimage : TActivityTypeobjectTypeattachmentsItemTypethumbnailsItemTypeimage;
    Furl : String;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure Setimage(AIndex : Integer; const AValue : TActivityTypeobjectTypeattachmentsItemTypethumbnailsItemTypeimage); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property description : String Index 0 Read Fdescription Write Setdescription;
    Property image : TActivityTypeobjectTypeattachmentsItemTypethumbnailsItemTypeimage Index 8 Read Fimage Write Setimage;
    Property url : String Index 16 Read Furl Write Seturl;
  end;
  TActivityTypeobjectTypeattachmentsItemTypethumbnailsItemClass = Class of TActivityTypeobjectTypeattachmentsItemTypethumbnailsItem;
  
  { --------------------------------------------------------------------
    TActivityTypeobjectTypeattachmentsItem
    --------------------------------------------------------------------}
  
  TActivityTypeobjectTypeattachmentsItem = Class(TGoogleBaseObject)
  Private
    Fcontent : String;
    FdisplayName : String;
    Fembed : TActivityTypeobjectTypeattachmentsItemTypeembed;
    FfullImage : TActivityTypeobjectTypeattachmentsItemTypefullImage;
    Fid : String;
    Fimage : TActivityTypeobjectTypeattachmentsItemTypeimage;
    FobjectType : String;
    Fthumbnails : TActivityTypeobjectTypeattachmentsItemTypethumbnailsArray;
    Furl : String;
  Protected
    //Property setters
    Procedure Setcontent(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setembed(AIndex : Integer; const AValue : TActivityTypeobjectTypeattachmentsItemTypeembed); virtual;
    Procedure SetfullImage(AIndex : Integer; const AValue : TActivityTypeobjectTypeattachmentsItemTypefullImage); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setimage(AIndex : Integer; const AValue : TActivityTypeobjectTypeattachmentsItemTypeimage); virtual;
    Procedure SetobjectType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setthumbnails(AIndex : Integer; const AValue : TActivityTypeobjectTypeattachmentsItemTypethumbnailsArray); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property content : String Index 0 Read Fcontent Write Setcontent;
    Property displayName : String Index 8 Read FdisplayName Write SetdisplayName;
    Property embed : TActivityTypeobjectTypeattachmentsItemTypeembed Index 16 Read Fembed Write Setembed;
    Property fullImage : TActivityTypeobjectTypeattachmentsItemTypefullImage Index 24 Read FfullImage Write SetfullImage;
    Property id : String Index 32 Read Fid Write Setid;
    Property image : TActivityTypeobjectTypeattachmentsItemTypeimage Index 40 Read Fimage Write Setimage;
    Property objectType : String Index 48 Read FobjectType Write SetobjectType;
    Property thumbnails : TActivityTypeobjectTypeattachmentsItemTypethumbnailsArray Index 56 Read Fthumbnails Write Setthumbnails;
    Property url : String Index 64 Read Furl Write Seturl;
  end;
  TActivityTypeobjectTypeattachmentsItemClass = Class of TActivityTypeobjectTypeattachmentsItem;
  
  { --------------------------------------------------------------------
    TActivityTypeobjectTypeplusoners
    --------------------------------------------------------------------}
  
  TActivityTypeobjectTypeplusoners = Class(TGoogleBaseObject)
  Private
    FselfLink : String;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure SetselfLink(AIndex : Integer; const AValue : String); virtual;
    Procedure SettotalItems(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property selfLink : String Index 0 Read FselfLink Write SetselfLink;
    Property totalItems : integer Index 8 Read FtotalItems Write SettotalItems;
  end;
  TActivityTypeobjectTypeplusonersClass = Class of TActivityTypeobjectTypeplusoners;
  
  { --------------------------------------------------------------------
    TActivityTypeobjectTypereplies
    --------------------------------------------------------------------}
  
  TActivityTypeobjectTypereplies = Class(TGoogleBaseObject)
  Private
    FselfLink : String;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure SetselfLink(AIndex : Integer; const AValue : String); virtual;
    Procedure SettotalItems(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property selfLink : String Index 0 Read FselfLink Write SetselfLink;
    Property totalItems : integer Index 8 Read FtotalItems Write SettotalItems;
  end;
  TActivityTypeobjectTyperepliesClass = Class of TActivityTypeobjectTypereplies;
  
  { --------------------------------------------------------------------
    TActivityTypeobjectTyperesharers
    --------------------------------------------------------------------}
  
  TActivityTypeobjectTyperesharers = Class(TGoogleBaseObject)
  Private
    FselfLink : String;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure SetselfLink(AIndex : Integer; const AValue : String); virtual;
    Procedure SettotalItems(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property selfLink : String Index 0 Read FselfLink Write SetselfLink;
    Property totalItems : integer Index 8 Read FtotalItems Write SettotalItems;
  end;
  TActivityTypeobjectTyperesharersClass = Class of TActivityTypeobjectTyperesharers;
  
  { --------------------------------------------------------------------
    TActivityTypeobject
    --------------------------------------------------------------------}
  
  TActivityTypeobject = Class(TGoogleBaseObject)
  Private
    Factor : TActivityTypeobjectTypeactor;
    Fattachments : TActivityTypeobjectTypeattachmentsArray;
    Fcontent : String;
    Fid : String;
    FobjectType : String;
    ForiginalContent : String;
    Fplusoners : TActivityTypeobjectTypeplusoners;
    Freplies : TActivityTypeobjectTypereplies;
    Fresharers : TActivityTypeobjectTyperesharers;
    Furl : String;
  Protected
    //Property setters
    Procedure Setactor(AIndex : Integer; const AValue : TActivityTypeobjectTypeactor); virtual;
    Procedure Setattachments(AIndex : Integer; const AValue : TActivityTypeobjectTypeattachmentsArray); virtual;
    Procedure Setcontent(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure SetobjectType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoriginalContent(AIndex : Integer; const AValue : String); virtual;
    Procedure Setplusoners(AIndex : Integer; const AValue : TActivityTypeobjectTypeplusoners); virtual;
    Procedure Setreplies(AIndex : Integer; const AValue : TActivityTypeobjectTypereplies); virtual;
    Procedure Setresharers(AIndex : Integer; const AValue : TActivityTypeobjectTyperesharers); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property actor : TActivityTypeobjectTypeactor Index 0 Read Factor Write Setactor;
    Property attachments : TActivityTypeobjectTypeattachmentsArray Index 8 Read Fattachments Write Setattachments;
    Property content : String Index 16 Read Fcontent Write Setcontent;
    Property id : String Index 24 Read Fid Write Setid;
    Property objectType : String Index 32 Read FobjectType Write SetobjectType;
    Property originalContent : String Index 40 Read ForiginalContent Write SetoriginalContent;
    Property plusoners : TActivityTypeobjectTypeplusoners Index 48 Read Fplusoners Write Setplusoners;
    Property replies : TActivityTypeobjectTypereplies Index 56 Read Freplies Write Setreplies;
    Property resharers : TActivityTypeobjectTyperesharers Index 64 Read Fresharers Write Setresharers;
    Property url : String Index 72 Read Furl Write Seturl;
  end;
  TActivityTypeobjectClass = Class of TActivityTypeobject;
  
  { --------------------------------------------------------------------
    TActivityTypeprovider
    --------------------------------------------------------------------}
  
  TActivityTypeprovider = Class(TGoogleBaseObject)
  Private
    Ftitle : String;
  Protected
    //Property setters
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property title : String Index 0 Read Ftitle Write Settitle;
  end;
  TActivityTypeproviderClass = Class of TActivityTypeprovider;
  
  { --------------------------------------------------------------------
    TActivity
    --------------------------------------------------------------------}
  
  TActivity = Class(TGoogleBaseObject)
  Private
    Faccess : TAcl;
    Factor : TActivityTypeactor;
    Faddress : String;
    Fannotation : String;
    FcrosspostSource : String;
    Fetag : String;
    Fgeocode : String;
    Fid : String;
    Fkind : String;
    Flocation : TPlace;
    F_object : TActivityTypeobject;
    FplaceId : String;
    FplaceName : String;
    Fprovider : TActivityTypeprovider;
    F_published : TDatetime;
    Fradius : String;
    Ftitle : String;
    Fupdated : TDatetime;
    Furl : String;
    Fverb : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setaccess(AIndex : Integer; const AValue : TAcl); virtual;
    Procedure Setactor(AIndex : Integer; const AValue : TActivityTypeactor); virtual;
    Procedure Setaddress(AIndex : Integer; const AValue : String); virtual;
    Procedure Setannotation(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcrosspostSource(AIndex : Integer; const AValue : String); virtual;
    Procedure Setetag(AIndex : Integer; const AValue : String); virtual;
    Procedure Setgeocode(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlocation(AIndex : Integer; const AValue : TPlace); virtual;
    Procedure Set_object(AIndex : Integer; const AValue : TActivityTypeobject); virtual;
    Procedure SetplaceId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetplaceName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setprovider(AIndex : Integer; const AValue : TActivityTypeprovider); virtual;
    Procedure Set_published(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure Setradius(AIndex : Integer; const AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setverb(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property access : TAcl Index 0 Read Faccess Write Setaccess;
    Property actor : TActivityTypeactor Index 8 Read Factor Write Setactor;
    Property address : String Index 16 Read Faddress Write Setaddress;
    Property annotation : String Index 24 Read Fannotation Write Setannotation;
    Property crosspostSource : String Index 32 Read FcrosspostSource Write SetcrosspostSource;
    Property etag : String Index 40 Read Fetag Write Setetag;
    Property geocode : String Index 48 Read Fgeocode Write Setgeocode;
    Property id : String Index 56 Read Fid Write Setid;
    Property kind : String Index 64 Read Fkind Write Setkind;
    Property location : TPlace Index 72 Read Flocation Write Setlocation;
    Property _object : TActivityTypeobject Index 80 Read F_object Write Set_object;
    Property placeId : String Index 88 Read FplaceId Write SetplaceId;
    Property placeName : String Index 96 Read FplaceName Write SetplaceName;
    Property provider : TActivityTypeprovider Index 104 Read Fprovider Write Setprovider;
    Property _published : TDatetime Index 112 Read F_published Write Set_published;
    Property radius : String Index 120 Read Fradius Write Setradius;
    Property title : String Index 128 Read Ftitle Write Settitle;
    Property updated : TDatetime Index 136 Read Fupdated Write Setupdated;
    Property url : String Index 144 Read Furl Write Seturl;
    Property verb : String Index 152 Read Fverb Write Setverb;
  end;
  TActivityClass = Class of TActivity;
  
  { --------------------------------------------------------------------
    TActivityFeed
    --------------------------------------------------------------------}
  
  TActivityFeed = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fid : String;
    Fitems : TActivityFeedTypeitemsArray;
    Fkind : String;
    FnextLink : String;
    FnextPageToken : String;
    FselfLink : String;
    Ftitle : String;
    Fupdated : TDatetime;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; const AValue : TActivityFeedTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnextLink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; const AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; const AValue : TDatetime); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property id : String Index 8 Read Fid Write Setid;
    Property items : TActivityFeedTypeitemsArray Index 16 Read Fitems Write Setitems;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property nextLink : String Index 32 Read FnextLink Write SetnextLink;
    Property nextPageToken : String Index 40 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : String Index 48 Read FselfLink Write SetselfLink;
    Property title : String Index 56 Read Ftitle Write Settitle;
    Property updated : TDatetime Index 64 Read Fupdated Write Setupdated;
  end;
  TActivityFeedClass = Class of TActivityFeed;
  
  { --------------------------------------------------------------------
    TCommentTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo
    --------------------------------------------------------------------}
  
  TCommentTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo = Class(TGoogleBaseObject)
  Private
    FchannelId : String;
  Protected
    //Property setters
    Procedure SetchannelId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property channelId : String Index 0 Read FchannelId Write SetchannelId;
  end;
  TCommentTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfoClass = Class of TCommentTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo;
  
  { --------------------------------------------------------------------
    TCommentTypeactorTypeclientSpecificActorInfo
    --------------------------------------------------------------------}
  
  TCommentTypeactorTypeclientSpecificActorInfo = Class(TGoogleBaseObject)
  Private
    FyoutubeActorInfo : TCommentTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo;
  Protected
    //Property setters
    Procedure SetyoutubeActorInfo(AIndex : Integer; const AValue : TCommentTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo); virtual;
  Public
  Published
    Property youtubeActorInfo : TCommentTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo Index 0 Read FyoutubeActorInfo Write SetyoutubeActorInfo;
  end;
  TCommentTypeactorTypeclientSpecificActorInfoClass = Class of TCommentTypeactorTypeclientSpecificActorInfo;
  
  { --------------------------------------------------------------------
    TCommentTypeactorTypeimage
    --------------------------------------------------------------------}
  
  TCommentTypeactorTypeimage = Class(TGoogleBaseObject)
  Private
    Furl : String;
  Protected
    //Property setters
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property url : String Index 0 Read Furl Write Seturl;
  end;
  TCommentTypeactorTypeimageClass = Class of TCommentTypeactorTypeimage;
  
  { --------------------------------------------------------------------
    TCommentTypeactorTypeverification
    --------------------------------------------------------------------}
  
  TCommentTypeactorTypeverification = Class(TGoogleBaseObject)
  Private
    FadHocVerified : String;
  Protected
    //Property setters
    Procedure SetadHocVerified(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property adHocVerified : String Index 0 Read FadHocVerified Write SetadHocVerified;
  end;
  TCommentTypeactorTypeverificationClass = Class of TCommentTypeactorTypeverification;
  
  { --------------------------------------------------------------------
    TCommentTypeactor
    --------------------------------------------------------------------}
  
  TCommentTypeactor = Class(TGoogleBaseObject)
  Private
    FclientSpecificActorInfo : TCommentTypeactorTypeclientSpecificActorInfo;
    FdisplayName : String;
    Fid : String;
    Fimage : TCommentTypeactorTypeimage;
    Furl : String;
    Fverification : TCommentTypeactorTypeverification;
  Protected
    //Property setters
    Procedure SetclientSpecificActorInfo(AIndex : Integer; const AValue : TCommentTypeactorTypeclientSpecificActorInfo); virtual;
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setimage(AIndex : Integer; const AValue : TCommentTypeactorTypeimage); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setverification(AIndex : Integer; const AValue : TCommentTypeactorTypeverification); virtual;
  Public
  Published
    Property clientSpecificActorInfo : TCommentTypeactorTypeclientSpecificActorInfo Index 0 Read FclientSpecificActorInfo Write SetclientSpecificActorInfo;
    Property displayName : String Index 8 Read FdisplayName Write SetdisplayName;
    Property id : String Index 16 Read Fid Write Setid;
    Property image : TCommentTypeactorTypeimage Index 24 Read Fimage Write Setimage;
    Property url : String Index 32 Read Furl Write Seturl;
    Property verification : TCommentTypeactorTypeverification Index 40 Read Fverification Write Setverification;
  end;
  TCommentTypeactorClass = Class of TCommentTypeactor;
  
  { --------------------------------------------------------------------
    TCommentTypeinReplyToItem
    --------------------------------------------------------------------}
  
  TCommentTypeinReplyToItem = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Furl : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property url : String Index 8 Read Furl Write Seturl;
  end;
  TCommentTypeinReplyToItemClass = Class of TCommentTypeinReplyToItem;
  
  { --------------------------------------------------------------------
    TCommentTypeobject
    --------------------------------------------------------------------}
  
  TCommentTypeobject = Class(TGoogleBaseObject)
  Private
    Fcontent : String;
    FobjectType : String;
    ForiginalContent : String;
  Protected
    //Property setters
    Procedure Setcontent(AIndex : Integer; const AValue : String); virtual;
    Procedure SetobjectType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoriginalContent(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property content : String Index 0 Read Fcontent Write Setcontent;
    Property objectType : String Index 8 Read FobjectType Write SetobjectType;
    Property originalContent : String Index 16 Read ForiginalContent Write SetoriginalContent;
  end;
  TCommentTypeobjectClass = Class of TCommentTypeobject;
  
  { --------------------------------------------------------------------
    TCommentTypeplusoners
    --------------------------------------------------------------------}
  
  TCommentTypeplusoners = Class(TGoogleBaseObject)
  Private
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure SettotalItems(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property totalItems : integer Index 0 Read FtotalItems Write SettotalItems;
  end;
  TCommentTypeplusonersClass = Class of TCommentTypeplusoners;
  
  { --------------------------------------------------------------------
    TComment
    --------------------------------------------------------------------}
  
  TComment = Class(TGoogleBaseObject)
  Private
    Factor : TCommentTypeactor;
    Fetag : String;
    Fid : String;
    FinReplyTo : TCommentTypeinReplyToArray;
    Fkind : String;
    F_object : TCommentTypeobject;
    Fplusoners : TCommentTypeplusoners;
    F_published : TDatetime;
    FselfLink : String;
    Fupdated : TDatetime;
    Fverb : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setactor(AIndex : Integer; const AValue : TCommentTypeactor); virtual;
    Procedure Setetag(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinReplyTo(AIndex : Integer; const AValue : TCommentTypeinReplyToArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_object(AIndex : Integer; const AValue : TCommentTypeobject); virtual;
    Procedure Setplusoners(AIndex : Integer; const AValue : TCommentTypeplusoners); virtual;
    Procedure Set_published(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure SetselfLink(AIndex : Integer; const AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure Setverb(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property actor : TCommentTypeactor Index 0 Read Factor Write Setactor;
    Property etag : String Index 8 Read Fetag Write Setetag;
    Property id : String Index 16 Read Fid Write Setid;
    Property inReplyTo : TCommentTypeinReplyToArray Index 24 Read FinReplyTo Write SetinReplyTo;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property _object : TCommentTypeobject Index 40 Read F_object Write Set_object;
    Property plusoners : TCommentTypeplusoners Index 48 Read Fplusoners Write Setplusoners;
    Property _published : TDatetime Index 56 Read F_published Write Set_published;
    Property selfLink : String Index 64 Read FselfLink Write SetselfLink;
    Property updated : TDatetime Index 72 Read Fupdated Write Setupdated;
    Property verb : String Index 80 Read Fverb Write Setverb;
  end;
  TCommentClass = Class of TComment;
  
  { --------------------------------------------------------------------
    TCommentFeed
    --------------------------------------------------------------------}
  
  TCommentFeed = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fid : String;
    Fitems : TCommentFeedTypeitemsArray;
    Fkind : String;
    FnextLink : String;
    FnextPageToken : String;
    Ftitle : String;
    Fupdated : TDatetime;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; const AValue : TCommentFeedTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnextLink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; const AValue : TDatetime); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property id : String Index 8 Read Fid Write Setid;
    Property items : TCommentFeedTypeitemsArray Index 16 Read Fitems Write Setitems;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property nextLink : String Index 32 Read FnextLink Write SetnextLink;
    Property nextPageToken : String Index 40 Read FnextPageToken Write SetnextPageToken;
    Property title : String Index 48 Read Ftitle Write Settitle;
    Property updated : TDatetime Index 56 Read Fupdated Write Setupdated;
  end;
  TCommentFeedClass = Class of TCommentFeed;
  
  { --------------------------------------------------------------------
    TPeopleFeed
    --------------------------------------------------------------------}
  
  TPeopleFeed = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fitems : TPeopleFeedTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FselfLink : String;
    Ftitle : String;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; const AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; const AValue : TPeopleFeedTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; const AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
    Procedure SettotalItems(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property items : TPeopleFeedTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : String Index 32 Read FselfLink Write SetselfLink;
    Property title : String Index 40 Read Ftitle Write Settitle;
    Property totalItems : integer Index 48 Read FtotalItems Write SettotalItems;
  end;
  TPeopleFeedClass = Class of TPeopleFeed;
  
  { --------------------------------------------------------------------
    TPersonTypeageRange
    --------------------------------------------------------------------}
  
  TPersonTypeageRange = Class(TGoogleBaseObject)
  Private
    Fmax : integer;
    Fmin : integer;
  Protected
    //Property setters
    Procedure Setmax(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setmin(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property max : integer Index 0 Read Fmax Write Setmax;
    Property min : integer Index 8 Read Fmin Write Setmin;
  end;
  TPersonTypeageRangeClass = Class of TPersonTypeageRange;
  
  { --------------------------------------------------------------------
    TPersonTypecoverTypecoverInfo
    --------------------------------------------------------------------}
  
  TPersonTypecoverTypecoverInfo = Class(TGoogleBaseObject)
  Private
    FleftImageOffset : integer;
    FtopImageOffset : integer;
  Protected
    //Property setters
    Procedure SetleftImageOffset(AIndex : Integer; const AValue : integer); virtual;
    Procedure SettopImageOffset(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property leftImageOffset : integer Index 0 Read FleftImageOffset Write SetleftImageOffset;
    Property topImageOffset : integer Index 8 Read FtopImageOffset Write SettopImageOffset;
  end;
  TPersonTypecoverTypecoverInfoClass = Class of TPersonTypecoverTypecoverInfo;
  
  { --------------------------------------------------------------------
    TPersonTypecoverTypecoverPhoto
    --------------------------------------------------------------------}
  
  TPersonTypecoverTypecoverPhoto = Class(TGoogleBaseObject)
  Private
    Fheight : integer;
    Furl : String;
    Fwidth : integer;
  Protected
    //Property setters
    Procedure Setheight(AIndex : Integer; const AValue : integer); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setwidth(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property height : integer Index 0 Read Fheight Write Setheight;
    Property url : String Index 8 Read Furl Write Seturl;
    Property width : integer Index 16 Read Fwidth Write Setwidth;
  end;
  TPersonTypecoverTypecoverPhotoClass = Class of TPersonTypecoverTypecoverPhoto;
  
  { --------------------------------------------------------------------
    TPersonTypecover
    --------------------------------------------------------------------}
  
  TPersonTypecover = Class(TGoogleBaseObject)
  Private
    FcoverInfo : TPersonTypecoverTypecoverInfo;
    FcoverPhoto : TPersonTypecoverTypecoverPhoto;
    Flayout : String;
  Protected
    //Property setters
    Procedure SetcoverInfo(AIndex : Integer; const AValue : TPersonTypecoverTypecoverInfo); virtual;
    Procedure SetcoverPhoto(AIndex : Integer; const AValue : TPersonTypecoverTypecoverPhoto); virtual;
    Procedure Setlayout(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property coverInfo : TPersonTypecoverTypecoverInfo Index 0 Read FcoverInfo Write SetcoverInfo;
    Property coverPhoto : TPersonTypecoverTypecoverPhoto Index 8 Read FcoverPhoto Write SetcoverPhoto;
    Property layout : String Index 16 Read Flayout Write Setlayout;
  end;
  TPersonTypecoverClass = Class of TPersonTypecover;
  
  { --------------------------------------------------------------------
    TPersonTypeemailsItem
    --------------------------------------------------------------------}
  
  TPersonTypeemailsItem = Class(TGoogleBaseObject)
  Private
    F_type : String;
    Fvalue : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property _type : String Index 0 Read F_type Write Set_type;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TPersonTypeemailsItemClass = Class of TPersonTypeemailsItem;
  
  { --------------------------------------------------------------------
    TPersonTypeimage
    --------------------------------------------------------------------}
  
  TPersonTypeimage = Class(TGoogleBaseObject)
  Private
    FisDefault : boolean;
    Furl : String;
  Protected
    //Property setters
    Procedure SetisDefault(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property isDefault : boolean Index 0 Read FisDefault Write SetisDefault;
    Property url : String Index 8 Read Furl Write Seturl;
  end;
  TPersonTypeimageClass = Class of TPersonTypeimage;
  
  { --------------------------------------------------------------------
    TPersonTypename
    --------------------------------------------------------------------}
  
  TPersonTypename = Class(TGoogleBaseObject)
  Private
    FfamilyName : String;
    Fformatted : String;
    FgivenName : String;
    FhonorificPrefix : String;
    FhonorificSuffix : String;
    FmiddleName : String;
  Protected
    //Property setters
    Procedure SetfamilyName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setformatted(AIndex : Integer; const AValue : String); virtual;
    Procedure SetgivenName(AIndex : Integer; const AValue : String); virtual;
    Procedure SethonorificPrefix(AIndex : Integer; const AValue : String); virtual;
    Procedure SethonorificSuffix(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmiddleName(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property familyName : String Index 0 Read FfamilyName Write SetfamilyName;
    Property formatted : String Index 8 Read Fformatted Write Setformatted;
    Property givenName : String Index 16 Read FgivenName Write SetgivenName;
    Property honorificPrefix : String Index 24 Read FhonorificPrefix Write SethonorificPrefix;
    Property honorificSuffix : String Index 32 Read FhonorificSuffix Write SethonorificSuffix;
    Property middleName : String Index 40 Read FmiddleName Write SetmiddleName;
  end;
  TPersonTypenameClass = Class of TPersonTypename;
  
  { --------------------------------------------------------------------
    TPersonTypeorganizationsItem
    --------------------------------------------------------------------}
  
  TPersonTypeorganizationsItem = Class(TGoogleBaseObject)
  Private
    Fdepartment : String;
    Fdescription : String;
    FendDate : String;
    Flocation : String;
    Fname : String;
    Fprimary : boolean;
    FstartDate : String;
    Ftitle : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setdepartment(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure SetendDate(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlocation(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setprimary(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetstartDate(AIndex : Integer; const AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property department : String Index 0 Read Fdepartment Write Setdepartment;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property endDate : String Index 16 Read FendDate Write SetendDate;
    Property location : String Index 24 Read Flocation Write Setlocation;
    Property name : String Index 32 Read Fname Write Setname;
    Property primary : boolean Index 40 Read Fprimary Write Setprimary;
    Property startDate : String Index 48 Read FstartDate Write SetstartDate;
    Property title : String Index 56 Read Ftitle Write Settitle;
    Property _type : String Index 64 Read F_type Write Set_type;
  end;
  TPersonTypeorganizationsItemClass = Class of TPersonTypeorganizationsItem;
  
  { --------------------------------------------------------------------
    TPersonTypeplacesLivedItem
    --------------------------------------------------------------------}
  
  TPersonTypeplacesLivedItem = Class(TGoogleBaseObject)
  Private
    Fprimary : boolean;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setprimary(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property primary : boolean Index 0 Read Fprimary Write Setprimary;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TPersonTypeplacesLivedItemClass = Class of TPersonTypeplacesLivedItem;
  
  { --------------------------------------------------------------------
    TPersonTypeurlsItem
    --------------------------------------------------------------------}
  
  TPersonTypeurlsItem = Class(TGoogleBaseObject)
  Private
    F_label : String;
    F_type : String;
    Fvalue : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_label(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property _label : String Index 0 Read F_label Write Set_label;
    Property _type : String Index 8 Read F_type Write Set_type;
    Property value : String Index 16 Read Fvalue Write Setvalue;
  end;
  TPersonTypeurlsItemClass = Class of TPersonTypeurlsItem;
  
  { --------------------------------------------------------------------
    TPerson
    --------------------------------------------------------------------}
  
  TPerson = Class(TGoogleBaseObject)
  Private
    FaboutMe : String;
    FageRange : TPersonTypeageRange;
    Fbirthday : String;
    FbraggingRights : String;
    FcircledByCount : integer;
    Fcover : TPersonTypecover;
    FcurrentLocation : String;
    FdisplayName : String;
    Fdomain : String;
    Femails : TPersonTypeemailsArray;
    Fetag : String;
    Fgender : String;
    Fid : String;
    Fimage : TPersonTypeimage;
    FisPlusUser : boolean;
    Fkind : String;
    Flanguage : String;
    Fname : TPersonTypename;
    Fnickname : String;
    FobjectType : String;
    Foccupation : String;
    Forganizations : TPersonTypeorganizationsArray;
    FplacesLived : TPersonTypeplacesLivedArray;
    FplusOneCount : integer;
    FrelationshipStatus : String;
    Fskills : String;
    Ftagline : String;
    Furl : String;
    Furls : TPersonTypeurlsArray;
    Fverified : boolean;
  Protected
    //Property setters
    Procedure SetaboutMe(AIndex : Integer; const AValue : String); virtual;
    Procedure SetageRange(AIndex : Integer; const AValue : TPersonTypeageRange); virtual;
    Procedure Setbirthday(AIndex : Integer; const AValue : String); virtual;
    Procedure SetbraggingRights(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcircledByCount(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setcover(AIndex : Integer; const AValue : TPersonTypecover); virtual;
    Procedure SetcurrentLocation(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdomain(AIndex : Integer; const AValue : String); virtual;
    Procedure Setemails(AIndex : Integer; const AValue : TPersonTypeemailsArray); virtual;
    Procedure Setetag(AIndex : Integer; const AValue : String); virtual;
    Procedure Setgender(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setimage(AIndex : Integer; const AValue : TPersonTypeimage); virtual;
    Procedure SetisPlusUser(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlanguage(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : TPersonTypename); virtual;
    Procedure Setnickname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetobjectType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setoccupation(AIndex : Integer; const AValue : String); virtual;
    Procedure Setorganizations(AIndex : Integer; const AValue : TPersonTypeorganizationsArray); virtual;
    Procedure SetplacesLived(AIndex : Integer; const AValue : TPersonTypeplacesLivedArray); virtual;
    Procedure SetplusOneCount(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetrelationshipStatus(AIndex : Integer; const AValue : String); virtual;
    Procedure Setskills(AIndex : Integer; const AValue : String); virtual;
    Procedure Settagline(AIndex : Integer; const AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
    Procedure Seturls(AIndex : Integer; const AValue : TPersonTypeurlsArray); virtual;
    Procedure Setverified(AIndex : Integer; const AValue : boolean); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property aboutMe : String Index 0 Read FaboutMe Write SetaboutMe;
    Property ageRange : TPersonTypeageRange Index 8 Read FageRange Write SetageRange;
    Property birthday : String Index 16 Read Fbirthday Write Setbirthday;
    Property braggingRights : String Index 24 Read FbraggingRights Write SetbraggingRights;
    Property circledByCount : integer Index 32 Read FcircledByCount Write SetcircledByCount;
    Property cover : TPersonTypecover Index 40 Read Fcover Write Setcover;
    Property currentLocation : String Index 48 Read FcurrentLocation Write SetcurrentLocation;
    Property displayName : String Index 56 Read FdisplayName Write SetdisplayName;
    Property domain : String Index 64 Read Fdomain Write Setdomain;
    Property emails : TPersonTypeemailsArray Index 72 Read Femails Write Setemails;
    Property etag : String Index 80 Read Fetag Write Setetag;
    Property gender : String Index 88 Read Fgender Write Setgender;
    Property id : String Index 96 Read Fid Write Setid;
    Property image : TPersonTypeimage Index 104 Read Fimage Write Setimage;
    Property isPlusUser : boolean Index 112 Read FisPlusUser Write SetisPlusUser;
    Property kind : String Index 120 Read Fkind Write Setkind;
    Property language : String Index 128 Read Flanguage Write Setlanguage;
    Property name : TPersonTypename Index 136 Read Fname Write Setname;
    Property nickname : String Index 144 Read Fnickname Write Setnickname;
    Property objectType : String Index 152 Read FobjectType Write SetobjectType;
    Property occupation : String Index 160 Read Foccupation Write Setoccupation;
    Property organizations : TPersonTypeorganizationsArray Index 168 Read Forganizations Write Setorganizations;
    Property placesLived : TPersonTypeplacesLivedArray Index 176 Read FplacesLived Write SetplacesLived;
    Property plusOneCount : integer Index 184 Read FplusOneCount Write SetplusOneCount;
    Property relationshipStatus : String Index 192 Read FrelationshipStatus Write SetrelationshipStatus;
    Property skills : String Index 200 Read Fskills Write Setskills;
    Property tagline : String Index 208 Read Ftagline Write Settagline;
    Property url : String Index 216 Read Furl Write Seturl;
    Property urls : TPersonTypeurlsArray Index 224 Read Furls Write Seturls;
    Property verified : boolean Index 232 Read Fverified Write Setverified;
  end;
  TPersonClass = Class of TPerson;
  
  { --------------------------------------------------------------------
    TPlaceTypeaddress
    --------------------------------------------------------------------}
  
  TPlaceTypeaddress = Class(TGoogleBaseObject)
  Private
    Fformatted : String;
  Protected
    //Property setters
    Procedure Setformatted(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property formatted : String Index 0 Read Fformatted Write Setformatted;
  end;
  TPlaceTypeaddressClass = Class of TPlaceTypeaddress;
  
  { --------------------------------------------------------------------
    TPlaceTypeposition
    --------------------------------------------------------------------}
  
  TPlaceTypeposition = Class(TGoogleBaseObject)
  Private
    Flatitude : double;
    Flongitude : double;
  Protected
    //Property setters
    Procedure Setlatitude(AIndex : Integer; const AValue : double); virtual;
    Procedure Setlongitude(AIndex : Integer; const AValue : double); virtual;
  Public
  Published
    Property latitude : double Index 0 Read Flatitude Write Setlatitude;
    Property longitude : double Index 8 Read Flongitude Write Setlongitude;
  end;
  TPlaceTypepositionClass = Class of TPlaceTypeposition;
  
  { --------------------------------------------------------------------
    TPlace
    --------------------------------------------------------------------}
  
  TPlace = Class(TGoogleBaseObject)
  Private
    Faddress : TPlaceTypeaddress;
    FdisplayName : String;
    Fid : String;
    Fkind : String;
    Fposition : TPlaceTypeposition;
  Protected
    //Property setters
    Procedure Setaddress(AIndex : Integer; const AValue : TPlaceTypeaddress); virtual;
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setposition(AIndex : Integer; const AValue : TPlaceTypeposition); virtual;
  Public
  Published
    Property address : TPlaceTypeaddress Index 0 Read Faddress Write Setaddress;
    Property displayName : String Index 8 Read FdisplayName Write SetdisplayName;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property position : TPlaceTypeposition Index 32 Read Fposition Write Setposition;
  end;
  TPlaceClass = Class of TPlace;
  
  { --------------------------------------------------------------------
    TPlusAclentryResource
    --------------------------------------------------------------------}
  
  TPlusAclentryResource = Class(TGoogleBaseObject)
  Private
    FdisplayName : String;
    Fid : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property displayName : String Index 0 Read FdisplayName Write SetdisplayName;
    Property id : String Index 8 Read Fid Write Setid;
    Property _type : String Index 16 Read F_type Write Set_type;
  end;
  TPlusAclentryResourceClass = Class of TPlusAclentryResource;
  
  { --------------------------------------------------------------------
    TActivitiesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TActivitiesResource, method List
  
  TActivitiesListOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  
  //Optional query Options for TActivitiesResource, method Search
  
  TActivitiesSearchOptions = Record
    language : String;
    maxResults : integer;
    orderBy : String;
    pageToken : String;
    query : String;
  end;
  
  TActivitiesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(activityId: string) : TActivity;
    Function List(collection: string; userId: string; AQuery : string  = '') : TActivityFeed;
    Function List(collection: string; userId: string; AQuery : TActivitieslistOptions) : TActivityFeed;
    Function Search(AQuery : string  = '') : TActivityFeed;
    Function Search(AQuery : TActivitiessearchOptions) : TActivityFeed;
  end;
  
  
  { --------------------------------------------------------------------
    TCommentsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCommentsResource, method List
  
  TCommentsListOptions = Record
    maxResults : integer;
    pageToken : String;
    sortOrder : String;
  end;
  
  TCommentsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(commentId: string) : TComment;
    Function List(activityId: string; AQuery : string  = '') : TCommentFeed;
    Function List(activityId: string; AQuery : TCommentslistOptions) : TCommentFeed;
  end;
  
  
  { --------------------------------------------------------------------
    TPeopleResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TPeopleResource, method List
  
  TPeopleListOptions = Record
    maxResults : integer;
    orderBy : String;
    pageToken : String;
  end;
  
  
  //Optional query Options for TPeopleResource, method ListByActivity
  
  TPeopleListByActivityOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  
  //Optional query Options for TPeopleResource, method Search
  
  TPeopleSearchOptions = Record
    language : String;
    maxResults : integer;
    pageToken : String;
    query : String;
  end;
  
  TPeopleResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(userId: string) : TPerson;
    Function List(collection: string; userId: string; AQuery : string  = '') : TPeopleFeed;
    Function List(collection: string; userId: string; AQuery : TPeoplelistOptions) : TPeopleFeed;
    Function ListByActivity(activityId: string; collection: string; AQuery : string  = '') : TPeopleFeed;
    Function ListByActivity(activityId: string; collection: string; AQuery : TPeoplelistByActivityOptions) : TPeopleFeed;
    Function Search(AQuery : string  = '') : TPeopleFeed;
    Function Search(AQuery : TPeoplesearchOptions) : TPeopleFeed;
  end;
  
  
  { --------------------------------------------------------------------
    TPlusAPI
    --------------------------------------------------------------------}
  
  TPlusAPI = Class(TGoogleAPI)
  Private
    FActivitiesInstance : TActivitiesResource;
    FCommentsInstance : TCommentsResource;
    FPeopleInstance : TPeopleResource;
    Function GetActivitiesInstance : TActivitiesResource;virtual;
    Function GetCommentsInstance : TCommentsResource;virtual;
    Function GetPeopleInstance : TPeopleResource;virtual;
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
    Function CreateActivitiesResource(AOwner : TComponent) : TActivitiesResource;virtual;overload;
    Function CreateActivitiesResource : TActivitiesResource;virtual;overload;
    Function CreateCommentsResource(AOwner : TComponent) : TCommentsResource;virtual;overload;
    Function CreateCommentsResource : TCommentsResource;virtual;overload;
    Function CreatePeopleResource(AOwner : TComponent) : TPeopleResource;virtual;overload;
    Function CreatePeopleResource : TPeopleResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ActivitiesResource : TActivitiesResource Read GetActivitiesInstance;
    Property CommentsResource : TCommentsResource Read GetCommentsInstance;
    Property PeopleResource : TPeopleResource Read GetPeopleInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAcl
  --------------------------------------------------------------------}


Procedure TAcl.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAcl.Setitems(AIndex : Integer; const AValue : TAclTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAcl.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAcl.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TActivityTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo
  --------------------------------------------------------------------}


Procedure TActivityTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo.SetchannelId(AIndex : Integer; const AValue : String); 

begin
  If (FchannelId=AValue) then exit;
  FchannelId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityTypeactorTypeclientSpecificActorInfo
  --------------------------------------------------------------------}


Procedure TActivityTypeactorTypeclientSpecificActorInfo.SetyoutubeActorInfo(AIndex : Integer; const AValue : TActivityTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo); 

begin
  If (FyoutubeActorInfo=AValue) then exit;
  FyoutubeActorInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityTypeactorTypeimage
  --------------------------------------------------------------------}


Procedure TActivityTypeactorTypeimage.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityTypeactorTypename
  --------------------------------------------------------------------}


Procedure TActivityTypeactorTypename.SetfamilyName(AIndex : Integer; const AValue : String); 

begin
  If (FfamilyName=AValue) then exit;
  FfamilyName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeactorTypename.SetgivenName(AIndex : Integer; const AValue : String); 

begin
  If (FgivenName=AValue) then exit;
  FgivenName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityTypeactorTypeverification
  --------------------------------------------------------------------}


Procedure TActivityTypeactorTypeverification.SetadHocVerified(AIndex : Integer; const AValue : String); 

begin
  If (FadHocVerified=AValue) then exit;
  FadHocVerified:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityTypeactor
  --------------------------------------------------------------------}


Procedure TActivityTypeactor.SetclientSpecificActorInfo(AIndex : Integer; const AValue : TActivityTypeactorTypeclientSpecificActorInfo); 

begin
  If (FclientSpecificActorInfo=AValue) then exit;
  FclientSpecificActorInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeactor.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeactor.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeactor.Setimage(AIndex : Integer; const AValue : TActivityTypeactorTypeimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeactor.Setname(AIndex : Integer; const AValue : TActivityTypeactorTypename); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeactor.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeactor.Setverification(AIndex : Integer; const AValue : TActivityTypeactorTypeverification); 

begin
  If (Fverification=AValue) then exit;
  Fverification:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityTypeobjectTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo
  --------------------------------------------------------------------}


Procedure TActivityTypeobjectTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo.SetchannelId(AIndex : Integer; const AValue : String); 

begin
  If (FchannelId=AValue) then exit;
  FchannelId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityTypeobjectTypeactorTypeclientSpecificActorInfo
  --------------------------------------------------------------------}


Procedure TActivityTypeobjectTypeactorTypeclientSpecificActorInfo.SetyoutubeActorInfo(AIndex : Integer; const AValue : TActivityTypeobjectTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo); 

begin
  If (FyoutubeActorInfo=AValue) then exit;
  FyoutubeActorInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityTypeobjectTypeactorTypeimage
  --------------------------------------------------------------------}


Procedure TActivityTypeobjectTypeactorTypeimage.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityTypeobjectTypeactorTypeverification
  --------------------------------------------------------------------}


Procedure TActivityTypeobjectTypeactorTypeverification.SetadHocVerified(AIndex : Integer; const AValue : String); 

begin
  If (FadHocVerified=AValue) then exit;
  FadHocVerified:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityTypeobjectTypeactor
  --------------------------------------------------------------------}


Procedure TActivityTypeobjectTypeactor.SetclientSpecificActorInfo(AIndex : Integer; const AValue : TActivityTypeobjectTypeactorTypeclientSpecificActorInfo); 

begin
  If (FclientSpecificActorInfo=AValue) then exit;
  FclientSpecificActorInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeactor.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeactor.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeactor.Setimage(AIndex : Integer; const AValue : TActivityTypeobjectTypeactorTypeimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeactor.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeactor.Setverification(AIndex : Integer; const AValue : TActivityTypeobjectTypeactorTypeverification); 

begin
  If (Fverification=AValue) then exit;
  Fverification:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityTypeobjectTypeattachmentsItemTypeembed
  --------------------------------------------------------------------}


Procedure TActivityTypeobjectTypeattachmentsItemTypeembed.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItemTypeembed.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TActivityTypeobjectTypeattachmentsItemTypeembed.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TActivityTypeobjectTypeattachmentsItemTypefullImage
  --------------------------------------------------------------------}


Procedure TActivityTypeobjectTypeattachmentsItemTypefullImage.Setheight(AIndex : Integer; const AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItemTypefullImage.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItemTypefullImage.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItemTypefullImage.Setwidth(AIndex : Integer; const AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TActivityTypeobjectTypeattachmentsItemTypefullImage.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TActivityTypeobjectTypeattachmentsItemTypeimage
  --------------------------------------------------------------------}


Procedure TActivityTypeobjectTypeattachmentsItemTypeimage.Setheight(AIndex : Integer; const AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItemTypeimage.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItemTypeimage.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItemTypeimage.Setwidth(AIndex : Integer; const AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TActivityTypeobjectTypeattachmentsItemTypeimage.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TActivityTypeobjectTypeattachmentsItemTypethumbnailsItemTypeimage
  --------------------------------------------------------------------}


Procedure TActivityTypeobjectTypeattachmentsItemTypethumbnailsItemTypeimage.Setheight(AIndex : Integer; const AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItemTypethumbnailsItemTypeimage.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItemTypethumbnailsItemTypeimage.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItemTypethumbnailsItemTypeimage.Setwidth(AIndex : Integer; const AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TActivityTypeobjectTypeattachmentsItemTypethumbnailsItemTypeimage.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TActivityTypeobjectTypeattachmentsItemTypethumbnailsItem
  --------------------------------------------------------------------}


Procedure TActivityTypeobjectTypeattachmentsItemTypethumbnailsItem.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItemTypethumbnailsItem.Setimage(AIndex : Integer; const AValue : TActivityTypeobjectTypeattachmentsItemTypethumbnailsItemTypeimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItemTypethumbnailsItem.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityTypeobjectTypeattachmentsItem
  --------------------------------------------------------------------}


Procedure TActivityTypeobjectTypeattachmentsItem.Setcontent(AIndex : Integer; const AValue : String); 

begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItem.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItem.Setembed(AIndex : Integer; const AValue : TActivityTypeobjectTypeattachmentsItemTypeembed); 

begin
  If (Fembed=AValue) then exit;
  Fembed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItem.SetfullImage(AIndex : Integer; const AValue : TActivityTypeobjectTypeattachmentsItemTypefullImage); 

begin
  If (FfullImage=AValue) then exit;
  FfullImage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItem.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItem.Setimage(AIndex : Integer; const AValue : TActivityTypeobjectTypeattachmentsItemTypeimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItem.SetobjectType(AIndex : Integer; const AValue : String); 

begin
  If (FobjectType=AValue) then exit;
  FobjectType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItem.Setthumbnails(AIndex : Integer; const AValue : TActivityTypeobjectTypeattachmentsItemTypethumbnailsArray); 

begin
  If (Fthumbnails=AValue) then exit;
  Fthumbnails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItem.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TActivityTypeobjectTypeattachmentsItem.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'thumbnails' : SetLength(Fthumbnails,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TActivityTypeobjectTypeplusoners
  --------------------------------------------------------------------}


Procedure TActivityTypeobjectTypeplusoners.SetselfLink(AIndex : Integer; const AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeplusoners.SettotalItems(AIndex : Integer; const AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityTypeobjectTypereplies
  --------------------------------------------------------------------}


Procedure TActivityTypeobjectTypereplies.SetselfLink(AIndex : Integer; const AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypereplies.SettotalItems(AIndex : Integer; const AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityTypeobjectTyperesharers
  --------------------------------------------------------------------}


Procedure TActivityTypeobjectTyperesharers.SetselfLink(AIndex : Integer; const AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTyperesharers.SettotalItems(AIndex : Integer; const AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityTypeobject
  --------------------------------------------------------------------}


Procedure TActivityTypeobject.Setactor(AIndex : Integer; const AValue : TActivityTypeobjectTypeactor); 

begin
  If (Factor=AValue) then exit;
  Factor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobject.Setattachments(AIndex : Integer; const AValue : TActivityTypeobjectTypeattachmentsArray); 

begin
  If (Fattachments=AValue) then exit;
  Fattachments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobject.Setcontent(AIndex : Integer; const AValue : String); 

begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobject.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobject.SetobjectType(AIndex : Integer; const AValue : String); 

begin
  If (FobjectType=AValue) then exit;
  FobjectType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobject.SetoriginalContent(AIndex : Integer; const AValue : String); 

begin
  If (ForiginalContent=AValue) then exit;
  ForiginalContent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobject.Setplusoners(AIndex : Integer; const AValue : TActivityTypeobjectTypeplusoners); 

begin
  If (Fplusoners=AValue) then exit;
  Fplusoners:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobject.Setreplies(AIndex : Integer; const AValue : TActivityTypeobjectTypereplies); 

begin
  If (Freplies=AValue) then exit;
  Freplies:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobject.Setresharers(AIndex : Integer; const AValue : TActivityTypeobjectTyperesharers); 

begin
  If (Fresharers=AValue) then exit;
  Fresharers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobject.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TActivityTypeobject.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'attachments' : SetLength(Fattachments,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TActivityTypeprovider
  --------------------------------------------------------------------}


Procedure TActivityTypeprovider.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivity
  --------------------------------------------------------------------}


Procedure TActivity.Setaccess(AIndex : Integer; const AValue : TAcl); 

begin
  If (Faccess=AValue) then exit;
  Faccess:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setactor(AIndex : Integer; const AValue : TActivityTypeactor); 

begin
  If (Factor=AValue) then exit;
  Factor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setaddress(AIndex : Integer; const AValue : String); 

begin
  If (Faddress=AValue) then exit;
  Faddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setannotation(AIndex : Integer; const AValue : String); 

begin
  If (Fannotation=AValue) then exit;
  Fannotation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.SetcrosspostSource(AIndex : Integer; const AValue : String); 

begin
  If (FcrosspostSource=AValue) then exit;
  FcrosspostSource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setetag(AIndex : Integer; const AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setgeocode(AIndex : Integer; const AValue : String); 

begin
  If (Fgeocode=AValue) then exit;
  Fgeocode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setlocation(AIndex : Integer; const AValue : TPlace); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Set_object(AIndex : Integer; const AValue : TActivityTypeobject); 

begin
  If (F_object=AValue) then exit;
  F_object:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.SetplaceId(AIndex : Integer; const AValue : String); 

begin
  If (FplaceId=AValue) then exit;
  FplaceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.SetplaceName(AIndex : Integer; const AValue : String); 

begin
  If (FplaceName=AValue) then exit;
  FplaceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setprovider(AIndex : Integer; const AValue : TActivityTypeprovider); 

begin
  If (Fprovider=AValue) then exit;
  Fprovider:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Set_published(AIndex : Integer; const AValue : TDatetime); 

begin
  If (F_published=AValue) then exit;
  F_published:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setradius(AIndex : Integer; const AValue : String); 

begin
  If (Fradius=AValue) then exit;
  Fradius:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setupdated(AIndex : Integer; const AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setverb(AIndex : Integer; const AValue : String); 

begin
  If (Fverb=AValue) then exit;
  Fverb:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TActivity.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_object' : Result:='object';
  '_published' : Result:='published';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TActivityFeed
  --------------------------------------------------------------------}


Procedure TActivityFeed.Setetag(AIndex : Integer; const AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityFeed.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityFeed.Setitems(AIndex : Integer; const AValue : TActivityFeedTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityFeed.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityFeed.SetnextLink(AIndex : Integer; const AValue : String); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityFeed.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityFeed.SetselfLink(AIndex : Integer; const AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityFeed.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityFeed.Setupdated(AIndex : Integer; const AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TActivityFeed.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCommentTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo
  --------------------------------------------------------------------}


Procedure TCommentTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo.SetchannelId(AIndex : Integer; const AValue : String); 

begin
  If (FchannelId=AValue) then exit;
  FchannelId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentTypeactorTypeclientSpecificActorInfo
  --------------------------------------------------------------------}


Procedure TCommentTypeactorTypeclientSpecificActorInfo.SetyoutubeActorInfo(AIndex : Integer; const AValue : TCommentTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo); 

begin
  If (FyoutubeActorInfo=AValue) then exit;
  FyoutubeActorInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentTypeactorTypeimage
  --------------------------------------------------------------------}


Procedure TCommentTypeactorTypeimage.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentTypeactorTypeverification
  --------------------------------------------------------------------}


Procedure TCommentTypeactorTypeverification.SetadHocVerified(AIndex : Integer; const AValue : String); 

begin
  If (FadHocVerified=AValue) then exit;
  FadHocVerified:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentTypeactor
  --------------------------------------------------------------------}


Procedure TCommentTypeactor.SetclientSpecificActorInfo(AIndex : Integer; const AValue : TCommentTypeactorTypeclientSpecificActorInfo); 

begin
  If (FclientSpecificActorInfo=AValue) then exit;
  FclientSpecificActorInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentTypeactor.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentTypeactor.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentTypeactor.Setimage(AIndex : Integer; const AValue : TCommentTypeactorTypeimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentTypeactor.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentTypeactor.Setverification(AIndex : Integer; const AValue : TCommentTypeactorTypeverification); 

begin
  If (Fverification=AValue) then exit;
  Fverification:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentTypeinReplyToItem
  --------------------------------------------------------------------}


Procedure TCommentTypeinReplyToItem.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentTypeinReplyToItem.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentTypeobject
  --------------------------------------------------------------------}


Procedure TCommentTypeobject.Setcontent(AIndex : Integer; const AValue : String); 

begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentTypeobject.SetobjectType(AIndex : Integer; const AValue : String); 

begin
  If (FobjectType=AValue) then exit;
  FobjectType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentTypeobject.SetoriginalContent(AIndex : Integer; const AValue : String); 

begin
  If (ForiginalContent=AValue) then exit;
  ForiginalContent:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentTypeplusoners
  --------------------------------------------------------------------}


Procedure TCommentTypeplusoners.SettotalItems(AIndex : Integer; const AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TComment
  --------------------------------------------------------------------}


Procedure TComment.Setactor(AIndex : Integer; const AValue : TCommentTypeactor); 

begin
  If (Factor=AValue) then exit;
  Factor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setetag(AIndex : Integer; const AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.SetinReplyTo(AIndex : Integer; const AValue : TCommentTypeinReplyToArray); 

begin
  If (FinReplyTo=AValue) then exit;
  FinReplyTo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Set_object(AIndex : Integer; const AValue : TCommentTypeobject); 

begin
  If (F_object=AValue) then exit;
  F_object:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setplusoners(AIndex : Integer; const AValue : TCommentTypeplusoners); 

begin
  If (Fplusoners=AValue) then exit;
  Fplusoners:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Set_published(AIndex : Integer; const AValue : TDatetime); 

begin
  If (F_published=AValue) then exit;
  F_published:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.SetselfLink(AIndex : Integer; const AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setupdated(AIndex : Integer; const AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setverb(AIndex : Integer; const AValue : String); 

begin
  If (Fverb=AValue) then exit;
  Fverb:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TComment.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_object' : Result:='object';
  '_published' : Result:='published';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TComment.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'inreplyto' : SetLength(FinReplyTo,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCommentFeed
  --------------------------------------------------------------------}


Procedure TCommentFeed.Setetag(AIndex : Integer; const AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentFeed.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentFeed.Setitems(AIndex : Integer; const AValue : TCommentFeedTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentFeed.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentFeed.SetnextLink(AIndex : Integer; const AValue : String); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentFeed.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentFeed.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentFeed.Setupdated(AIndex : Integer; const AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCommentFeed.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPeopleFeed
  --------------------------------------------------------------------}


Procedure TPeopleFeed.Setetag(AIndex : Integer; const AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPeopleFeed.Setitems(AIndex : Integer; const AValue : TPeopleFeedTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPeopleFeed.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPeopleFeed.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPeopleFeed.SetselfLink(AIndex : Integer; const AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPeopleFeed.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPeopleFeed.SettotalItems(AIndex : Integer; const AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPeopleFeed.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPersonTypeageRange
  --------------------------------------------------------------------}


Procedure TPersonTypeageRange.Setmax(AIndex : Integer; const AValue : integer); 

begin
  If (Fmax=AValue) then exit;
  Fmax:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypeageRange.Setmin(AIndex : Integer; const AValue : integer); 

begin
  If (Fmin=AValue) then exit;
  Fmin:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPersonTypecoverTypecoverInfo
  --------------------------------------------------------------------}


Procedure TPersonTypecoverTypecoverInfo.SetleftImageOffset(AIndex : Integer; const AValue : integer); 

begin
  If (FleftImageOffset=AValue) then exit;
  FleftImageOffset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypecoverTypecoverInfo.SettopImageOffset(AIndex : Integer; const AValue : integer); 

begin
  If (FtopImageOffset=AValue) then exit;
  FtopImageOffset:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPersonTypecoverTypecoverPhoto
  --------------------------------------------------------------------}


Procedure TPersonTypecoverTypecoverPhoto.Setheight(AIndex : Integer; const AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypecoverTypecoverPhoto.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypecoverTypecoverPhoto.Setwidth(AIndex : Integer; const AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPersonTypecover
  --------------------------------------------------------------------}


Procedure TPersonTypecover.SetcoverInfo(AIndex : Integer; const AValue : TPersonTypecoverTypecoverInfo); 

begin
  If (FcoverInfo=AValue) then exit;
  FcoverInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypecover.SetcoverPhoto(AIndex : Integer; const AValue : TPersonTypecoverTypecoverPhoto); 

begin
  If (FcoverPhoto=AValue) then exit;
  FcoverPhoto:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypecover.Setlayout(AIndex : Integer; const AValue : String); 

begin
  If (Flayout=AValue) then exit;
  Flayout:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPersonTypeemailsItem
  --------------------------------------------------------------------}


Procedure TPersonTypeemailsItem.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypeemailsItem.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPersonTypeemailsItem.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPersonTypeimage
  --------------------------------------------------------------------}


Procedure TPersonTypeimage.SetisDefault(AIndex : Integer; const AValue : boolean); 

begin
  If (FisDefault=AValue) then exit;
  FisDefault:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypeimage.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPersonTypename
  --------------------------------------------------------------------}


Procedure TPersonTypename.SetfamilyName(AIndex : Integer; const AValue : String); 

begin
  If (FfamilyName=AValue) then exit;
  FfamilyName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypename.Setformatted(AIndex : Integer; const AValue : String); 

begin
  If (Fformatted=AValue) then exit;
  Fformatted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypename.SetgivenName(AIndex : Integer; const AValue : String); 

begin
  If (FgivenName=AValue) then exit;
  FgivenName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypename.SethonorificPrefix(AIndex : Integer; const AValue : String); 

begin
  If (FhonorificPrefix=AValue) then exit;
  FhonorificPrefix:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypename.SethonorificSuffix(AIndex : Integer; const AValue : String); 

begin
  If (FhonorificSuffix=AValue) then exit;
  FhonorificSuffix:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypename.SetmiddleName(AIndex : Integer; const AValue : String); 

begin
  If (FmiddleName=AValue) then exit;
  FmiddleName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPersonTypeorganizationsItem
  --------------------------------------------------------------------}


Procedure TPersonTypeorganizationsItem.Setdepartment(AIndex : Integer; const AValue : String); 

begin
  If (Fdepartment=AValue) then exit;
  Fdepartment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypeorganizationsItem.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypeorganizationsItem.SetendDate(AIndex : Integer; const AValue : String); 

begin
  If (FendDate=AValue) then exit;
  FendDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypeorganizationsItem.Setlocation(AIndex : Integer; const AValue : String); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypeorganizationsItem.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypeorganizationsItem.Setprimary(AIndex : Integer; const AValue : boolean); 

begin
  If (Fprimary=AValue) then exit;
  Fprimary:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypeorganizationsItem.SetstartDate(AIndex : Integer; const AValue : String); 

begin
  If (FstartDate=AValue) then exit;
  FstartDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypeorganizationsItem.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypeorganizationsItem.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPersonTypeorganizationsItem.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPersonTypeplacesLivedItem
  --------------------------------------------------------------------}


Procedure TPersonTypeplacesLivedItem.Setprimary(AIndex : Integer; const AValue : boolean); 

begin
  If (Fprimary=AValue) then exit;
  Fprimary:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypeplacesLivedItem.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPersonTypeurlsItem
  --------------------------------------------------------------------}


Procedure TPersonTypeurlsItem.Set_label(AIndex : Integer; const AValue : String); 

begin
  If (F_label=AValue) then exit;
  F_label:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypeurlsItem.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypeurlsItem.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPersonTypeurlsItem.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_label' : Result:='label';
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPerson
  --------------------------------------------------------------------}


Procedure TPerson.SetaboutMe(AIndex : Integer; const AValue : String); 

begin
  If (FaboutMe=AValue) then exit;
  FaboutMe:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetageRange(AIndex : Integer; const AValue : TPersonTypeageRange); 

begin
  If (FageRange=AValue) then exit;
  FageRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setbirthday(AIndex : Integer; const AValue : String); 

begin
  If (Fbirthday=AValue) then exit;
  Fbirthday:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetbraggingRights(AIndex : Integer; const AValue : String); 

begin
  If (FbraggingRights=AValue) then exit;
  FbraggingRights:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetcircledByCount(AIndex : Integer; const AValue : integer); 

begin
  If (FcircledByCount=AValue) then exit;
  FcircledByCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setcover(AIndex : Integer; const AValue : TPersonTypecover); 

begin
  If (Fcover=AValue) then exit;
  Fcover:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetcurrentLocation(AIndex : Integer; const AValue : String); 

begin
  If (FcurrentLocation=AValue) then exit;
  FcurrentLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setdomain(AIndex : Integer; const AValue : String); 

begin
  If (Fdomain=AValue) then exit;
  Fdomain:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setemails(AIndex : Integer; const AValue : TPersonTypeemailsArray); 

begin
  If (Femails=AValue) then exit;
  Femails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setetag(AIndex : Integer; const AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setgender(AIndex : Integer; const AValue : String); 

begin
  If (Fgender=AValue) then exit;
  Fgender:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setimage(AIndex : Integer; const AValue : TPersonTypeimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetisPlusUser(AIndex : Integer; const AValue : boolean); 

begin
  If (FisPlusUser=AValue) then exit;
  FisPlusUser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setlanguage(AIndex : Integer; const AValue : String); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setname(AIndex : Integer; const AValue : TPersonTypename); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setnickname(AIndex : Integer; const AValue : String); 

begin
  If (Fnickname=AValue) then exit;
  Fnickname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetobjectType(AIndex : Integer; const AValue : String); 

begin
  If (FobjectType=AValue) then exit;
  FobjectType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setoccupation(AIndex : Integer; const AValue : String); 

begin
  If (Foccupation=AValue) then exit;
  Foccupation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setorganizations(AIndex : Integer; const AValue : TPersonTypeorganizationsArray); 

begin
  If (Forganizations=AValue) then exit;
  Forganizations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetplacesLived(AIndex : Integer; const AValue : TPersonTypeplacesLivedArray); 

begin
  If (FplacesLived=AValue) then exit;
  FplacesLived:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetplusOneCount(AIndex : Integer; const AValue : integer); 

begin
  If (FplusOneCount=AValue) then exit;
  FplusOneCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetrelationshipStatus(AIndex : Integer; const AValue : String); 

begin
  If (FrelationshipStatus=AValue) then exit;
  FrelationshipStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setskills(AIndex : Integer; const AValue : String); 

begin
  If (Fskills=AValue) then exit;
  Fskills:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Settagline(AIndex : Integer; const AValue : String); 

begin
  If (Ftagline=AValue) then exit;
  Ftagline:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Seturls(AIndex : Integer; const AValue : TPersonTypeurlsArray); 

begin
  If (Furls=AValue) then exit;
  Furls:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setverified(AIndex : Integer; const AValue : boolean); 

begin
  If (Fverified=AValue) then exit;
  Fverified:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPerson.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'emails' : SetLength(Femails,ALength);
  'organizations' : SetLength(Forganizations,ALength);
  'placeslived' : SetLength(FplacesLived,ALength);
  'urls' : SetLength(Furls,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPlaceTypeaddress
  --------------------------------------------------------------------}


Procedure TPlaceTypeaddress.Setformatted(AIndex : Integer; const AValue : String); 

begin
  If (Fformatted=AValue) then exit;
  Fformatted:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlaceTypeposition
  --------------------------------------------------------------------}


Procedure TPlaceTypeposition.Setlatitude(AIndex : Integer; const AValue : double); 

begin
  If (Flatitude=AValue) then exit;
  Flatitude:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaceTypeposition.Setlongitude(AIndex : Integer; const AValue : double); 

begin
  If (Flongitude=AValue) then exit;
  Flongitude:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlace
  --------------------------------------------------------------------}


Procedure TPlace.Setaddress(AIndex : Integer; const AValue : TPlaceTypeaddress); 

begin
  If (Faddress=AValue) then exit;
  Faddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlace.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlace.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlace.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlace.Setposition(AIndex : Integer; const AValue : TPlaceTypeposition); 

begin
  If (Fposition=AValue) then exit;
  Fposition:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlusAclentryResource
  --------------------------------------------------------------------}


Procedure TPlusAclentryResource.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlusAclentryResource.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlusAclentryResource.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPlusAclentryResource.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TActivitiesResource
  --------------------------------------------------------------------}


Class Function TActivitiesResource.ResourceName : String;

begin
  Result:='activities';
end;

Class Function TActivitiesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TplusAPI;
end;

Function TActivitiesResource.Get(activityId: string) : TActivity;

Const
  _HTTPMethod = 'GET';
  _Path       = 'activities/{activityId}';
  _Methodid   = 'plus.activities.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['activityId',activityId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TActivity) as TActivity;
end;

Function TActivitiesResource.List(collection: string; userId: string; AQuery : string = '') : TActivityFeed;

Const
  _HTTPMethod = 'GET';
  _Path       = 'people/{userId}/activities/{collection}';
  _Methodid   = 'plus.activities.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['collection',collection,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TActivityFeed) as TActivityFeed;
end;


Function TActivitiesResource.List(collection: string; userId: string; AQuery : TActivitieslistOptions) : TActivityFeed;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(collection,userId,_Q);
end;

Function TActivitiesResource.Search(AQuery : string = '') : TActivityFeed;

Const
  _HTTPMethod = 'GET';
  _Path       = 'activities';
  _Methodid   = 'plus.activities.search';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TActivityFeed) as TActivityFeed;
end;


Function TActivitiesResource.Search(AQuery : TActivitiessearchOptions) : TActivityFeed;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'orderBy',AQuery.orderBy);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'query',AQuery.query);
  Result:=Search(_Q);
end;



{ --------------------------------------------------------------------
  TCommentsResource
  --------------------------------------------------------------------}


Class Function TCommentsResource.ResourceName : String;

begin
  Result:='comments';
end;

Class Function TCommentsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TplusAPI;
end;

Function TCommentsResource.Get(commentId: string) : TComment;

Const
  _HTTPMethod = 'GET';
  _Path       = 'comments/{commentId}';
  _Methodid   = 'plus.comments.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['commentId',commentId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TComment) as TComment;
end;

Function TCommentsResource.List(activityId: string; AQuery : string = '') : TCommentFeed;

Const
  _HTTPMethod = 'GET';
  _Path       = 'activities/{activityId}/comments';
  _Methodid   = 'plus.comments.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['activityId',activityId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TCommentFeed) as TCommentFeed;
end;


Function TCommentsResource.List(activityId: string; AQuery : TCommentslistOptions) : TCommentFeed;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  Result:=List(activityId,_Q);
end;



{ --------------------------------------------------------------------
  TPeopleResource
  --------------------------------------------------------------------}


Class Function TPeopleResource.ResourceName : String;

begin
  Result:='people';
end;

Class Function TPeopleResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TplusAPI;
end;

Function TPeopleResource.Get(userId: string) : TPerson;

Const
  _HTTPMethod = 'GET';
  _Path       = 'people/{userId}';
  _Methodid   = 'plus.people.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPerson) as TPerson;
end;

Function TPeopleResource.List(collection: string; userId: string; AQuery : string = '') : TPeopleFeed;

Const
  _HTTPMethod = 'GET';
  _Path       = 'people/{userId}/people/{collection}';
  _Methodid   = 'plus.people.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['collection',collection,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPeopleFeed) as TPeopleFeed;
end;


Function TPeopleResource.List(collection: string; userId: string; AQuery : TPeoplelistOptions) : TPeopleFeed;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'orderBy',AQuery.orderBy);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(collection,userId,_Q);
end;

Function TPeopleResource.ListByActivity(activityId: string; collection: string; AQuery : string = '') : TPeopleFeed;

Const
  _HTTPMethod = 'GET';
  _Path       = 'activities/{activityId}/people/{collection}';
  _Methodid   = 'plus.people.listByActivity';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['activityId',activityId,'collection',collection]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPeopleFeed) as TPeopleFeed;
end;


Function TPeopleResource.ListByActivity(activityId: string; collection: string; AQuery : TPeoplelistByActivityOptions) : TPeopleFeed;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=ListByActivity(activityId,collection,_Q);
end;

Function TPeopleResource.Search(AQuery : string = '') : TPeopleFeed;

Const
  _HTTPMethod = 'GET';
  _Path       = 'people';
  _Methodid   = 'plus.people.search';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TPeopleFeed) as TPeopleFeed;
end;


Function TPeopleResource.Search(AQuery : TPeoplesearchOptions) : TPeopleFeed;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'query',AQuery.query);
  Result:=Search(_Q);
end;



{ --------------------------------------------------------------------
  TPlusAPI
  --------------------------------------------------------------------}

Class Function TPlusAPI.APIName : String;

begin
  Result:='plus';
end;

Class Function TPlusAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TPlusAPI.APIRevision : String;

begin
  Result:='20160521';
end;

Class Function TPlusAPI.APIID : String;

begin
  Result:='plus:v1';
end;

Class Function TPlusAPI.APITitle : String;

begin
  Result:='Google+ API';
end;

Class Function TPlusAPI.APIDescription : String;

begin
  Result:='Builds on top of the Google+ platform.';
end;

Class Function TPlusAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TPlusAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TPlusAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/gplus-16.png';
end;

Class Function TPlusAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/gplus-32.png';
end;

Class Function TPlusAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/+/api/';
end;

Class Function TPlusAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TPlusAPI.APIbasePath : string;

begin
  Result:='/plus/v1/';
end;

Class Function TPlusAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/plus/v1/';
end;

Class Function TPlusAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TPlusAPI.APIservicePath : string;

begin
  Result:='plus/v1/';
end;

Class Function TPlusAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TPlusAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,4);
  Result[0].Name:='https://www.googleapis.com/auth/plus.login';
  Result[0].Description:='Know the list of people in your circles, your age range, and language';
  Result[1].Name:='https://www.googleapis.com/auth/plus.me';
  Result[1].Description:='Know who you are on Google';
  Result[2].Name:='https://www.googleapis.com/auth/userinfo.email';
  Result[2].Description:='View your email address';
  Result[3].Name:='https://www.googleapis.com/auth/userinfo.profile';
  Result[3].Description:='View your basic profile info';
  
end;

Class Function TPlusAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TPlusAPI.RegisterAPIResources;

begin
  TAcl.RegisterObject;
  TActivityTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo.RegisterObject;
  TActivityTypeactorTypeclientSpecificActorInfo.RegisterObject;
  TActivityTypeactorTypeimage.RegisterObject;
  TActivityTypeactorTypename.RegisterObject;
  TActivityTypeactorTypeverification.RegisterObject;
  TActivityTypeactor.RegisterObject;
  TActivityTypeobjectTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo.RegisterObject;
  TActivityTypeobjectTypeactorTypeclientSpecificActorInfo.RegisterObject;
  TActivityTypeobjectTypeactorTypeimage.RegisterObject;
  TActivityTypeobjectTypeactorTypeverification.RegisterObject;
  TActivityTypeobjectTypeactor.RegisterObject;
  TActivityTypeobjectTypeattachmentsItemTypeembed.RegisterObject;
  TActivityTypeobjectTypeattachmentsItemTypefullImage.RegisterObject;
  TActivityTypeobjectTypeattachmentsItemTypeimage.RegisterObject;
  TActivityTypeobjectTypeattachmentsItemTypethumbnailsItemTypeimage.RegisterObject;
  TActivityTypeobjectTypeattachmentsItemTypethumbnailsItem.RegisterObject;
  TActivityTypeobjectTypeattachmentsItem.RegisterObject;
  TActivityTypeobjectTypeplusoners.RegisterObject;
  TActivityTypeobjectTypereplies.RegisterObject;
  TActivityTypeobjectTyperesharers.RegisterObject;
  TActivityTypeobject.RegisterObject;
  TActivityTypeprovider.RegisterObject;
  TActivity.RegisterObject;
  TActivityFeed.RegisterObject;
  TCommentTypeactorTypeclientSpecificActorInfoTypeyoutubeActorInfo.RegisterObject;
  TCommentTypeactorTypeclientSpecificActorInfo.RegisterObject;
  TCommentTypeactorTypeimage.RegisterObject;
  TCommentTypeactorTypeverification.RegisterObject;
  TCommentTypeactor.RegisterObject;
  TCommentTypeinReplyToItem.RegisterObject;
  TCommentTypeobject.RegisterObject;
  TCommentTypeplusoners.RegisterObject;
  TComment.RegisterObject;
  TCommentFeed.RegisterObject;
  TPeopleFeed.RegisterObject;
  TPersonTypeageRange.RegisterObject;
  TPersonTypecoverTypecoverInfo.RegisterObject;
  TPersonTypecoverTypecoverPhoto.RegisterObject;
  TPersonTypecover.RegisterObject;
  TPersonTypeemailsItem.RegisterObject;
  TPersonTypeimage.RegisterObject;
  TPersonTypename.RegisterObject;
  TPersonTypeorganizationsItem.RegisterObject;
  TPersonTypeplacesLivedItem.RegisterObject;
  TPersonTypeurlsItem.RegisterObject;
  TPerson.RegisterObject;
  TPlaceTypeaddress.RegisterObject;
  TPlaceTypeposition.RegisterObject;
  TPlace.RegisterObject;
  TPlusAclentryResource.RegisterObject;
end;


Function TPlusAPI.GetActivitiesInstance : TActivitiesResource;

begin
  if (FActivitiesInstance=Nil) then
    FActivitiesInstance:=CreateActivitiesResource;
  Result:=FActivitiesInstance;
end;

Function TPlusAPI.CreateActivitiesResource : TActivitiesResource;

begin
  Result:=CreateActivitiesResource(Self);
end;


Function TPlusAPI.CreateActivitiesResource(AOwner : TComponent) : TActivitiesResource;

begin
  Result:=TActivitiesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TPlusAPI.GetCommentsInstance : TCommentsResource;

begin
  if (FCommentsInstance=Nil) then
    FCommentsInstance:=CreateCommentsResource;
  Result:=FCommentsInstance;
end;

Function TPlusAPI.CreateCommentsResource : TCommentsResource;

begin
  Result:=CreateCommentsResource(Self);
end;


Function TPlusAPI.CreateCommentsResource(AOwner : TComponent) : TCommentsResource;

begin
  Result:=TCommentsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TPlusAPI.GetPeopleInstance : TPeopleResource;

begin
  if (FPeopleInstance=Nil) then
    FPeopleInstance:=CreatePeopleResource;
  Result:=FPeopleInstance;
end;

Function TPlusAPI.CreatePeopleResource : TPeopleResource;

begin
  Result:=CreatePeopleResource(Self);
end;


Function TPlusAPI.CreatePeopleResource(AOwner : TComponent) : TPeopleResource;

begin
  Result:=TPeopleResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TPlusAPI.RegisterAPI;
end.
