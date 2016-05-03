unit googleplus;
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
  TAcl = Class;
  TActivity = Class;
  TActivityFeed = Class;
  TComment = Class;
  TCommentFeed = Class;
  TItemScope = Class;
  TMoment = Class;
  TMomentsFeed = Class;
  TPeopleFeed = Class;
  TPerson = Class;
  TPlace = Class;
  TPlusAclentryResource = Class;
  TAclArray = Array of TAcl;
  TActivityArray = Array of TActivity;
  TActivityFeedArray = Array of TActivityFeed;
  TCommentArray = Array of TComment;
  TCommentFeedArray = Array of TCommentFeed;
  TItemScopeArray = Array of TItemScope;
  TMomentArray = Array of TMoment;
  TMomentsFeedArray = Array of TMomentsFeed;
  TPeopleFeedArray = Array of TPeopleFeed;
  TPersonArray = Array of TPerson;
  TPlaceArray = Array of TPlace;
  TPlusAclentryResourceArray = Array of TPlusAclentryResource;
  //Anonymous types, using auto-generated names
  TActivityTypeactorTypeimage = Class;
  TActivityTypeactorTypename = Class;
  TActivityTypeactor = Class;
  TActivityTypeobjectTypeactorTypeimage = Class;
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
  TCommentTypeactorTypeimage = Class;
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
  TItemScopeTypeassociated_mediaArray = Array of TItemScope;
  TItemScopeTypeattendeesArray = Array of TItemScope;
  TItemScopeTypeauthorArray = Array of TItemScope;
  TItemScopeTypecontributorArray = Array of TItemScope;
  TItemScopeTypeperformersArray = Array of TItemScope;
  TMomentsFeedTypeitemsArray = Array of TMoment;
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
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TAclTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
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
    TActivityTypeactorTypeimage
    --------------------------------------------------------------------}
  
  TActivityTypeactorTypeimage = Class(TGoogleBaseObject)
  Private
    Furl : String;
  Protected
    //Property setters
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
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
    Procedure SetfamilyName(AIndex : Integer; AValue : String); virtual;
    Procedure SetgivenName(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property familyName : String Index 0 Read FfamilyName Write SetfamilyName;
    Property givenName : String Index 8 Read FgivenName Write SetgivenName;
  end;
  TActivityTypeactorTypenameClass = Class of TActivityTypeactorTypename;
  
  { --------------------------------------------------------------------
    TActivityTypeactor
    --------------------------------------------------------------------}
  
  TActivityTypeactor = Class(TGoogleBaseObject)
  Private
    FdisplayName : String;
    Fid : String;
    Fimage : TActivityTypeactorTypeimage;
    Fname : TActivityTypeactorTypename;
    Furl : String;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setimage(AIndex : Integer; AValue : TActivityTypeactorTypeimage); virtual;
    Procedure Setname(AIndex : Integer; AValue : TActivityTypeactorTypename); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property displayName : String Index 0 Read FdisplayName Write SetdisplayName;
    Property id : String Index 8 Read Fid Write Setid;
    Property image : TActivityTypeactorTypeimage Index 16 Read Fimage Write Setimage;
    Property name : TActivityTypeactorTypename Index 24 Read Fname Write Setname;
    Property url : String Index 32 Read Furl Write Seturl;
  end;
  TActivityTypeactorClass = Class of TActivityTypeactor;
  
  { --------------------------------------------------------------------
    TActivityTypeobjectTypeactorTypeimage
    --------------------------------------------------------------------}
  
  TActivityTypeobjectTypeactorTypeimage = Class(TGoogleBaseObject)
  Private
    Furl : String;
  Protected
    //Property setters
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property url : String Index 0 Read Furl Write Seturl;
  end;
  TActivityTypeobjectTypeactorTypeimageClass = Class of TActivityTypeobjectTypeactorTypeimage;
  
  { --------------------------------------------------------------------
    TActivityTypeobjectTypeactor
    --------------------------------------------------------------------}
  
  TActivityTypeobjectTypeactor = Class(TGoogleBaseObject)
  Private
    FdisplayName : String;
    Fid : String;
    Fimage : TActivityTypeobjectTypeactorTypeimage;
    Furl : String;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setimage(AIndex : Integer; AValue : TActivityTypeobjectTypeactorTypeimage); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property displayName : String Index 0 Read FdisplayName Write SetdisplayName;
    Property id : String Index 8 Read Fid Write Setid;
    Property image : TActivityTypeobjectTypeactorTypeimage Index 16 Read Fimage Write Setimage;
    Property url : String Index 24 Read Furl Write Seturl;
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
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
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
    Procedure Setheight(AIndex : Integer; AValue : integer); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : integer); virtual;
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
    Procedure Setheight(AIndex : Integer; AValue : integer); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : integer); virtual;
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
    Procedure Setheight(AIndex : Integer; AValue : integer); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : integer); virtual;
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
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setimage(AIndex : Integer; AValue : TActivityTypeobjectTypeattachmentsItemTypethumbnailsItemTypeimage); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
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
    Procedure Setcontent(AIndex : Integer; AValue : String); virtual;
    Procedure SetdisplayName(AIndex : Integer; AValue : String); virtual;
    Procedure Setembed(AIndex : Integer; AValue : TActivityTypeobjectTypeattachmentsItemTypeembed); virtual;
    Procedure SetfullImage(AIndex : Integer; AValue : TActivityTypeobjectTypeattachmentsItemTypefullImage); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setimage(AIndex : Integer; AValue : TActivityTypeobjectTypeattachmentsItemTypeimage); virtual;
    Procedure SetobjectType(AIndex : Integer; AValue : String); virtual;
    Procedure Setthumbnails(AIndex : Integer; AValue : TActivityTypeobjectTypeattachmentsItemTypethumbnailsArray); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
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
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
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
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
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
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
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
    Procedure Setactor(AIndex : Integer; AValue : TActivityTypeobjectTypeactor); virtual;
    Procedure Setattachments(AIndex : Integer; AValue : TActivityTypeobjectTypeattachmentsArray); virtual;
    Procedure Setcontent(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetobjectType(AIndex : Integer; AValue : String); virtual;
    Procedure SetoriginalContent(AIndex : Integer; AValue : String); virtual;
    Procedure Setplusoners(AIndex : Integer; AValue : TActivityTypeobjectTypeplusoners); virtual;
    Procedure Setreplies(AIndex : Integer; AValue : TActivityTypeobjectTypereplies); virtual;
    Procedure Setresharers(AIndex : Integer; AValue : TActivityTypeobjectTyperesharers); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
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
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
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
    Procedure Setaccess(AIndex : Integer; AValue : TAcl); virtual;
    Procedure Setactor(AIndex : Integer; AValue : TActivityTypeactor); virtual;
    Procedure Setaddress(AIndex : Integer; AValue : String); virtual;
    Procedure Setannotation(AIndex : Integer; AValue : String); virtual;
    Procedure SetcrosspostSource(AIndex : Integer; AValue : String); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setgeocode(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : TPlace); virtual;
    Procedure Set_object(AIndex : Integer; AValue : TActivityTypeobject); virtual;
    Procedure SetplaceId(AIndex : Integer; AValue : String); virtual;
    Procedure SetplaceName(AIndex : Integer; AValue : String); virtual;
    Procedure Setprovider(AIndex : Integer; AValue : TActivityTypeprovider); virtual;
    Procedure Set_published(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setradius(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
    Procedure Setverb(AIndex : Integer; AValue : String); virtual;
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
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TActivityFeedTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
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
    TCommentTypeactorTypeimage
    --------------------------------------------------------------------}
  
  TCommentTypeactorTypeimage = Class(TGoogleBaseObject)
  Private
    Furl : String;
  Protected
    //Property setters
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property url : String Index 0 Read Furl Write Seturl;
  end;
  TCommentTypeactorTypeimageClass = Class of TCommentTypeactorTypeimage;
  
  { --------------------------------------------------------------------
    TCommentTypeactor
    --------------------------------------------------------------------}
  
  TCommentTypeactor = Class(TGoogleBaseObject)
  Private
    FdisplayName : String;
    Fid : String;
    Fimage : TCommentTypeactorTypeimage;
    Furl : String;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setimage(AIndex : Integer; AValue : TCommentTypeactorTypeimage); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property displayName : String Index 0 Read FdisplayName Write SetdisplayName;
    Property id : String Index 8 Read Fid Write Setid;
    Property image : TCommentTypeactorTypeimage Index 16 Read Fimage Write Setimage;
    Property url : String Index 24 Read Furl Write Seturl;
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
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
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
    Procedure Setcontent(AIndex : Integer; AValue : String); virtual;
    Procedure SetobjectType(AIndex : Integer; AValue : String); virtual;
    Procedure SetoriginalContent(AIndex : Integer; AValue : String); virtual;
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
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
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
    Procedure Setactor(AIndex : Integer; AValue : TCommentTypeactor); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetinReplyTo(AIndex : Integer; AValue : TCommentTypeinReplyToArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Set_object(AIndex : Integer; AValue : TCommentTypeobject); virtual;
    Procedure Setplusoners(AIndex : Integer; AValue : TCommentTypeplusoners); virtual;
    Procedure Set_published(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setverb(AIndex : Integer; AValue : String); virtual;
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
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TCommentFeedTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
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
    TItemScope
    --------------------------------------------------------------------}
  
  TItemScope = Class(TGoogleBaseObject)
  Private
    Fabout : TItemScope;
    FadditionalName : TStringArray;
    Faddress : TItemScope;
    FaddressCountry : String;
    FaddressLocality : String;
    FaddressRegion : String;
    Fassociated_media : TItemScopeTypeassociated_mediaArray;
    FattendeeCount : integer;
    Fattendees : TItemScopeTypeattendeesArray;
    Faudio : TItemScope;
    Fauthor : TItemScopeTypeauthorArray;
    FbestRating : String;
    FbirthDate : String;
    FbyArtist : TItemScope;
    Fcaption : String;
    FcontentSize : String;
    FcontentUrl : String;
    Fcontributor : TItemScopeTypecontributorArray;
    FdateCreated : String;
    FdateModified : String;
    FdatePublished : String;
    Fdescription : String;
    Fduration : String;
    FembedUrl : String;
    FendDate : String;
    FfamilyName : String;
    Fgender : String;
    Fgeo : TItemScope;
    FgivenName : String;
    Fheight : String;
    Fid : String;
    Fimage : String;
    FinAlbum : TItemScope;
    Fkind : String;
    Flatitude : double;
    Flocation : TItemScope;
    Flongitude : double;
    Fname : String;
    FpartOfTVSeries : TItemScope;
    Fperformers : TItemScopeTypeperformersArray;
    FplayerType : String;
    FpostOfficeBoxNumber : String;
    FpostalCode : String;
    FratingValue : String;
    FreviewRating : TItemScope;
    FstartDate : String;
    FstreetAddress : String;
    Ftext : String;
    Fthumbnail : TItemScope;
    FthumbnailUrl : String;
    FtickerSymbol : String;
    F_type : String;
    Furl : String;
    Fwidth : String;
    FworstRating : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setabout(AIndex : Integer; AValue : TItemScope); virtual;
    Procedure SetadditionalName(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setaddress(AIndex : Integer; AValue : TItemScope); virtual;
    Procedure SetaddressCountry(AIndex : Integer; AValue : String); virtual;
    Procedure SetaddressLocality(AIndex : Integer; AValue : String); virtual;
    Procedure SetaddressRegion(AIndex : Integer; AValue : String); virtual;
    Procedure Setassociated_media(AIndex : Integer; AValue : TItemScopeTypeassociated_mediaArray); virtual;
    Procedure SetattendeeCount(AIndex : Integer; AValue : integer); virtual;
    Procedure Setattendees(AIndex : Integer; AValue : TItemScopeTypeattendeesArray); virtual;
    Procedure Setaudio(AIndex : Integer; AValue : TItemScope); virtual;
    Procedure Setauthor(AIndex : Integer; AValue : TItemScopeTypeauthorArray); virtual;
    Procedure SetbestRating(AIndex : Integer; AValue : String); virtual;
    Procedure SetbirthDate(AIndex : Integer; AValue : String); virtual;
    Procedure SetbyArtist(AIndex : Integer; AValue : TItemScope); virtual;
    Procedure Setcaption(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontentSize(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontentUrl(AIndex : Integer; AValue : String); virtual;
    Procedure Setcontributor(AIndex : Integer; AValue : TItemScopeTypecontributorArray); virtual;
    Procedure SetdateCreated(AIndex : Integer; AValue : String); virtual;
    Procedure SetdateModified(AIndex : Integer; AValue : String); virtual;
    Procedure SetdatePublished(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setduration(AIndex : Integer; AValue : String); virtual;
    Procedure SetembedUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetendDate(AIndex : Integer; AValue : String); virtual;
    Procedure SetfamilyName(AIndex : Integer; AValue : String); virtual;
    Procedure Setgender(AIndex : Integer; AValue : String); virtual;
    Procedure Setgeo(AIndex : Integer; AValue : TItemScope); virtual;
    Procedure SetgivenName(AIndex : Integer; AValue : String); virtual;
    Procedure Setheight(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setimage(AIndex : Integer; AValue : String); virtual;
    Procedure SetinAlbum(AIndex : Integer; AValue : TItemScope); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlatitude(AIndex : Integer; AValue : double); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : TItemScope); virtual;
    Procedure Setlongitude(AIndex : Integer; AValue : double); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetpartOfTVSeries(AIndex : Integer; AValue : TItemScope); virtual;
    Procedure Setperformers(AIndex : Integer; AValue : TItemScopeTypeperformersArray); virtual;
    Procedure SetplayerType(AIndex : Integer; AValue : String); virtual;
    Procedure SetpostOfficeBoxNumber(AIndex : Integer; AValue : String); virtual;
    Procedure SetpostalCode(AIndex : Integer; AValue : String); virtual;
    Procedure SetratingValue(AIndex : Integer; AValue : String); virtual;
    Procedure SetreviewRating(AIndex : Integer; AValue : TItemScope); virtual;
    Procedure SetstartDate(AIndex : Integer; AValue : String); virtual;
    Procedure SetstreetAddress(AIndex : Integer; AValue : String); virtual;
    Procedure Settext(AIndex : Integer; AValue : String); virtual;
    Procedure Setthumbnail(AIndex : Integer; AValue : TItemScope); virtual;
    Procedure SetthumbnailUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SettickerSymbol(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : String); virtual;
    Procedure SetworstRating(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property about : TItemScope Index 0 Read Fabout Write Setabout;
    Property additionalName : TStringArray Index 8 Read FadditionalName Write SetadditionalName;
    Property address : TItemScope Index 16 Read Faddress Write Setaddress;
    Property addressCountry : String Index 24 Read FaddressCountry Write SetaddressCountry;
    Property addressLocality : String Index 32 Read FaddressLocality Write SetaddressLocality;
    Property addressRegion : String Index 40 Read FaddressRegion Write SetaddressRegion;
    Property associated_media : TItemScopeTypeassociated_mediaArray Index 48 Read Fassociated_media Write Setassociated_media;
    Property attendeeCount : integer Index 56 Read FattendeeCount Write SetattendeeCount;
    Property attendees : TItemScopeTypeattendeesArray Index 64 Read Fattendees Write Setattendees;
    Property audio : TItemScope Index 72 Read Faudio Write Setaudio;
    Property author : TItemScopeTypeauthorArray Index 80 Read Fauthor Write Setauthor;
    Property bestRating : String Index 88 Read FbestRating Write SetbestRating;
    Property birthDate : String Index 96 Read FbirthDate Write SetbirthDate;
    Property byArtist : TItemScope Index 104 Read FbyArtist Write SetbyArtist;
    Property caption : String Index 112 Read Fcaption Write Setcaption;
    Property contentSize : String Index 120 Read FcontentSize Write SetcontentSize;
    Property contentUrl : String Index 128 Read FcontentUrl Write SetcontentUrl;
    Property contributor : TItemScopeTypecontributorArray Index 136 Read Fcontributor Write Setcontributor;
    Property dateCreated : String Index 144 Read FdateCreated Write SetdateCreated;
    Property dateModified : String Index 152 Read FdateModified Write SetdateModified;
    Property datePublished : String Index 160 Read FdatePublished Write SetdatePublished;
    Property description : String Index 168 Read Fdescription Write Setdescription;
    Property duration : String Index 176 Read Fduration Write Setduration;
    Property embedUrl : String Index 184 Read FembedUrl Write SetembedUrl;
    Property endDate : String Index 192 Read FendDate Write SetendDate;
    Property familyName : String Index 200 Read FfamilyName Write SetfamilyName;
    Property gender : String Index 208 Read Fgender Write Setgender;
    Property geo : TItemScope Index 216 Read Fgeo Write Setgeo;
    Property givenName : String Index 224 Read FgivenName Write SetgivenName;
    Property height : String Index 232 Read Fheight Write Setheight;
    Property id : String Index 240 Read Fid Write Setid;
    Property image : String Index 248 Read Fimage Write Setimage;
    Property inAlbum : TItemScope Index 256 Read FinAlbum Write SetinAlbum;
    Property kind : String Index 264 Read Fkind Write Setkind;
    Property latitude : double Index 272 Read Flatitude Write Setlatitude;
    Property location : TItemScope Index 280 Read Flocation Write Setlocation;
    Property longitude : double Index 288 Read Flongitude Write Setlongitude;
    Property name : String Index 296 Read Fname Write Setname;
    Property partOfTVSeries : TItemScope Index 304 Read FpartOfTVSeries Write SetpartOfTVSeries;
    Property performers : TItemScopeTypeperformersArray Index 312 Read Fperformers Write Setperformers;
    Property playerType : String Index 320 Read FplayerType Write SetplayerType;
    Property postOfficeBoxNumber : String Index 328 Read FpostOfficeBoxNumber Write SetpostOfficeBoxNumber;
    Property postalCode : String Index 336 Read FpostalCode Write SetpostalCode;
    Property ratingValue : String Index 344 Read FratingValue Write SetratingValue;
    Property reviewRating : TItemScope Index 352 Read FreviewRating Write SetreviewRating;
    Property startDate : String Index 360 Read FstartDate Write SetstartDate;
    Property streetAddress : String Index 368 Read FstreetAddress Write SetstreetAddress;
    Property text : String Index 376 Read Ftext Write Settext;
    Property thumbnail : TItemScope Index 384 Read Fthumbnail Write Setthumbnail;
    Property thumbnailUrl : String Index 392 Read FthumbnailUrl Write SetthumbnailUrl;
    Property tickerSymbol : String Index 400 Read FtickerSymbol Write SettickerSymbol;
    Property _type : String Index 408 Read F_type Write Set_type;
    Property url : String Index 416 Read Furl Write Seturl;
    Property width : String Index 424 Read Fwidth Write Setwidth;
    Property worstRating : String Index 432 Read FworstRating Write SetworstRating;
  end;
  TItemScopeClass = Class of TItemScope;
  
  { --------------------------------------------------------------------
    TMoment
    --------------------------------------------------------------------}
  
  TMoment = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fkind : String;
    F_object : TItemScope;
    Fresult : TItemScope;
    FstartDate : TDatetime;
    Ftarget : TItemScope;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Set_object(AIndex : Integer; AValue : TItemScope); virtual;
    Procedure Setresult(AIndex : Integer; AValue : TItemScope); virtual;
    Procedure SetstartDate(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Settarget(AIndex : Integer; AValue : TItemScope); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property _object : TItemScope Index 16 Read F_object Write Set_object;
    Property result : TItemScope Index 24 Read Fresult Write Setresult;
    Property startDate : TDatetime Index 32 Read FstartDate Write SetstartDate;
    Property target : TItemScope Index 40 Read Ftarget Write Settarget;
    Property _type : String Index 48 Read F_type Write Set_type;
  end;
  TMomentClass = Class of TMoment;
  
  { --------------------------------------------------------------------
    TMomentsFeed
    --------------------------------------------------------------------}
  
  TMomentsFeed = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fitems : TMomentsFeedTypeitemsArray;
    Fkind : String;
    FnextLink : String;
    FnextPageToken : String;
    FselfLink : String;
    Ftitle : String;
    Fupdated : TDatetime;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TMomentsFeedTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property items : TMomentsFeedTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextLink : String Index 24 Read FnextLink Write SetnextLink;
    Property nextPageToken : String Index 32 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : String Index 40 Read FselfLink Write SetselfLink;
    Property title : String Index 48 Read Ftitle Write Settitle;
    Property updated : TDatetime Index 56 Read Fupdated Write Setupdated;
  end;
  TMomentsFeedClass = Class of TMomentsFeed;
  
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
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TPeopleFeedTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
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
    Procedure Setmax(AIndex : Integer; AValue : integer); virtual;
    Procedure Setmin(AIndex : Integer; AValue : integer); virtual;
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
    Procedure SetleftImageOffset(AIndex : Integer; AValue : integer); virtual;
    Procedure SettopImageOffset(AIndex : Integer; AValue : integer); virtual;
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
    Procedure Setheight(AIndex : Integer; AValue : integer); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : integer); virtual;
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
    Procedure SetcoverInfo(AIndex : Integer; AValue : TPersonTypecoverTypecoverInfo); virtual;
    Procedure SetcoverPhoto(AIndex : Integer; AValue : TPersonTypecoverTypecoverPhoto); virtual;
    Procedure Setlayout(AIndex : Integer; AValue : String); virtual;
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
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
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
    Procedure SetisDefault(AIndex : Integer; AValue : boolean); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
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
    Procedure SetfamilyName(AIndex : Integer; AValue : String); virtual;
    Procedure Setformatted(AIndex : Integer; AValue : String); virtual;
    Procedure SetgivenName(AIndex : Integer; AValue : String); virtual;
    Procedure SethonorificPrefix(AIndex : Integer; AValue : String); virtual;
    Procedure SethonorificSuffix(AIndex : Integer; AValue : String); virtual;
    Procedure SetmiddleName(AIndex : Integer; AValue : String); virtual;
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
    Procedure Setdepartment(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetendDate(AIndex : Integer; AValue : String); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setprimary(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetstartDate(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
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
    Procedure Setprimary(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
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
    Procedure Set_label(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
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
    Procedure SetaboutMe(AIndex : Integer; AValue : String); virtual;
    Procedure SetageRange(AIndex : Integer; AValue : TPersonTypeageRange); virtual;
    Procedure Setbirthday(AIndex : Integer; AValue : String); virtual;
    Procedure SetbraggingRights(AIndex : Integer; AValue : String); virtual;
    Procedure SetcircledByCount(AIndex : Integer; AValue : integer); virtual;
    Procedure Setcover(AIndex : Integer; AValue : TPersonTypecover); virtual;
    Procedure SetcurrentLocation(AIndex : Integer; AValue : String); virtual;
    Procedure SetdisplayName(AIndex : Integer; AValue : String); virtual;
    Procedure Setdomain(AIndex : Integer; AValue : String); virtual;
    Procedure Setemails(AIndex : Integer; AValue : TPersonTypeemailsArray); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setgender(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setimage(AIndex : Integer; AValue : TPersonTypeimage); virtual;
    Procedure SetisPlusUser(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlanguage(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : TPersonTypename); virtual;
    Procedure Setnickname(AIndex : Integer; AValue : String); virtual;
    Procedure SetobjectType(AIndex : Integer; AValue : String); virtual;
    Procedure Setoccupation(AIndex : Integer; AValue : String); virtual;
    Procedure Setorganizations(AIndex : Integer; AValue : TPersonTypeorganizationsArray); virtual;
    Procedure SetplacesLived(AIndex : Integer; AValue : TPersonTypeplacesLivedArray); virtual;
    Procedure SetplusOneCount(AIndex : Integer; AValue : integer); virtual;
    Procedure SetrelationshipStatus(AIndex : Integer; AValue : String); virtual;
    Procedure Setskills(AIndex : Integer; AValue : String); virtual;
    Procedure Settagline(AIndex : Integer; AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
    Procedure Seturls(AIndex : Integer; AValue : TPersonTypeurlsArray); virtual;
    Procedure Setverified(AIndex : Integer; AValue : boolean); virtual;
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
    Procedure Setformatted(AIndex : Integer; AValue : String); virtual;
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
    Procedure Setlatitude(AIndex : Integer; AValue : double); virtual;
    Procedure Setlongitude(AIndex : Integer; AValue : double); virtual;
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
    Procedure Setaddress(AIndex : Integer; AValue : TPlaceTypeaddress); virtual;
    Procedure SetdisplayName(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setposition(AIndex : Integer; AValue : TPlaceTypeposition); virtual;
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
    Procedure SetdisplayName(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
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
    TMomentsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TMomentsResource, method Insert
  
  TMomentsInsertOptions = Record
    debug : boolean;
  end;
  
  
  //Optional query Options for TMomentsResource, method List
  
  TMomentsListOptions = Record
    maxResults : integer;
    pageToken : String;
    targetUrl : String;
    _type : String;
  end;
  
  TMomentsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Insert(collection: string; userId: string; aMoment : TMoment; AQuery : string  = '') : TMoment;
    Function Insert(collection: string; userId: string; aMoment : TMoment; AQuery : TMomentsinsertOptions) : TMoment;
    Function List(collection: string; userId: string; AQuery : string  = '') : TMomentsFeed;
    Function List(collection: string; userId: string; AQuery : TMomentslistOptions) : TMomentsFeed;
    Procedure Remove(id: string);
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
    FMomentsInstance : TMomentsResource;
    FPeopleInstance : TPeopleResource;
    Function GetActivitiesInstance : TActivitiesResource;virtual;
    Function GetCommentsInstance : TCommentsResource;virtual;
    Function GetMomentsInstance : TMomentsResource;virtual;
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
    Function CreateMomentsResource(AOwner : TComponent) : TMomentsResource;virtual;overload;
    Function CreateMomentsResource : TMomentsResource;virtual;overload;
    Function CreatePeopleResource(AOwner : TComponent) : TPeopleResource;virtual;overload;
    Function CreatePeopleResource : TPeopleResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ActivitiesResource : TActivitiesResource Read GetActivitiesInstance;
    Property CommentsResource : TCommentsResource Read GetCommentsInstance;
    Property MomentsResource : TMomentsResource Read GetMomentsInstance;
    Property PeopleResource : TPeopleResource Read GetPeopleInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAcl
  --------------------------------------------------------------------}


Procedure TAcl.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAcl.Setitems(AIndex : Integer; AValue : TAclTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAcl.Setkind(AIndex : Integer; AValue : String); 

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
  TActivityTypeactorTypeimage
  --------------------------------------------------------------------}


Procedure TActivityTypeactorTypeimage.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityTypeactorTypename
  --------------------------------------------------------------------}


Procedure TActivityTypeactorTypename.SetfamilyName(AIndex : Integer; AValue : String); 

begin
  If (FfamilyName=AValue) then exit;
  FfamilyName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeactorTypename.SetgivenName(AIndex : Integer; AValue : String); 

begin
  If (FgivenName=AValue) then exit;
  FgivenName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityTypeactor
  --------------------------------------------------------------------}


Procedure TActivityTypeactor.SetdisplayName(AIndex : Integer; AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeactor.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeactor.Setimage(AIndex : Integer; AValue : TActivityTypeactorTypeimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeactor.Setname(AIndex : Integer; AValue : TActivityTypeactorTypename); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeactor.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityTypeobjectTypeactorTypeimage
  --------------------------------------------------------------------}


Procedure TActivityTypeobjectTypeactorTypeimage.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityTypeobjectTypeactor
  --------------------------------------------------------------------}


Procedure TActivityTypeobjectTypeactor.SetdisplayName(AIndex : Integer; AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeactor.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeactor.Setimage(AIndex : Integer; AValue : TActivityTypeobjectTypeactorTypeimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeactor.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityTypeobjectTypeattachmentsItemTypeembed
  --------------------------------------------------------------------}


Procedure TActivityTypeobjectTypeattachmentsItemTypeembed.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItemTypeembed.Seturl(AIndex : Integer; AValue : String); 

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


Procedure TActivityTypeobjectTypeattachmentsItemTypefullImage.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItemTypefullImage.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItemTypefullImage.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItemTypefullImage.Setwidth(AIndex : Integer; AValue : integer); 

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


Procedure TActivityTypeobjectTypeattachmentsItemTypeimage.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItemTypeimage.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItemTypeimage.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItemTypeimage.Setwidth(AIndex : Integer; AValue : integer); 

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


Procedure TActivityTypeobjectTypeattachmentsItemTypethumbnailsItemTypeimage.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItemTypethumbnailsItemTypeimage.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItemTypethumbnailsItemTypeimage.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItemTypethumbnailsItemTypeimage.Setwidth(AIndex : Integer; AValue : integer); 

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


Procedure TActivityTypeobjectTypeattachmentsItemTypethumbnailsItem.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItemTypethumbnailsItem.Setimage(AIndex : Integer; AValue : TActivityTypeobjectTypeattachmentsItemTypethumbnailsItemTypeimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItemTypethumbnailsItem.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityTypeobjectTypeattachmentsItem
  --------------------------------------------------------------------}


Procedure TActivityTypeobjectTypeattachmentsItem.Setcontent(AIndex : Integer; AValue : String); 

begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItem.SetdisplayName(AIndex : Integer; AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItem.Setembed(AIndex : Integer; AValue : TActivityTypeobjectTypeattachmentsItemTypeembed); 

begin
  If (Fembed=AValue) then exit;
  Fembed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItem.SetfullImage(AIndex : Integer; AValue : TActivityTypeobjectTypeattachmentsItemTypefullImage); 

begin
  If (FfullImage=AValue) then exit;
  FfullImage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItem.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItem.Setimage(AIndex : Integer; AValue : TActivityTypeobjectTypeattachmentsItemTypeimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItem.SetobjectType(AIndex : Integer; AValue : String); 

begin
  If (FobjectType=AValue) then exit;
  FobjectType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItem.Setthumbnails(AIndex : Integer; AValue : TActivityTypeobjectTypeattachmentsItemTypethumbnailsArray); 

begin
  If (Fthumbnails=AValue) then exit;
  Fthumbnails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeattachmentsItem.Seturl(AIndex : Integer; AValue : String); 

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


Procedure TActivityTypeobjectTypeplusoners.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypeplusoners.SettotalItems(AIndex : Integer; AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityTypeobjectTypereplies
  --------------------------------------------------------------------}


Procedure TActivityTypeobjectTypereplies.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTypereplies.SettotalItems(AIndex : Integer; AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityTypeobjectTyperesharers
  --------------------------------------------------------------------}


Procedure TActivityTypeobjectTyperesharers.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobjectTyperesharers.SettotalItems(AIndex : Integer; AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityTypeobject
  --------------------------------------------------------------------}


Procedure TActivityTypeobject.Setactor(AIndex : Integer; AValue : TActivityTypeobjectTypeactor); 

begin
  If (Factor=AValue) then exit;
  Factor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobject.Setattachments(AIndex : Integer; AValue : TActivityTypeobjectTypeattachmentsArray); 

begin
  If (Fattachments=AValue) then exit;
  Fattachments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobject.Setcontent(AIndex : Integer; AValue : String); 

begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobject.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobject.SetobjectType(AIndex : Integer; AValue : String); 

begin
  If (FobjectType=AValue) then exit;
  FobjectType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobject.SetoriginalContent(AIndex : Integer; AValue : String); 

begin
  If (ForiginalContent=AValue) then exit;
  ForiginalContent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobject.Setplusoners(AIndex : Integer; AValue : TActivityTypeobjectTypeplusoners); 

begin
  If (Fplusoners=AValue) then exit;
  Fplusoners:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobject.Setreplies(AIndex : Integer; AValue : TActivityTypeobjectTypereplies); 

begin
  If (Freplies=AValue) then exit;
  Freplies:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobject.Setresharers(AIndex : Integer; AValue : TActivityTypeobjectTyperesharers); 

begin
  If (Fresharers=AValue) then exit;
  Fresharers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeobject.Seturl(AIndex : Integer; AValue : String); 

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


Procedure TActivityTypeprovider.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivity
  --------------------------------------------------------------------}


Procedure TActivity.Setaccess(AIndex : Integer; AValue : TAcl); 

begin
  If (Faccess=AValue) then exit;
  Faccess:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setactor(AIndex : Integer; AValue : TActivityTypeactor); 

begin
  If (Factor=AValue) then exit;
  Factor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setaddress(AIndex : Integer; AValue : String); 

begin
  If (Faddress=AValue) then exit;
  Faddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setannotation(AIndex : Integer; AValue : String); 

begin
  If (Fannotation=AValue) then exit;
  Fannotation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.SetcrosspostSource(AIndex : Integer; AValue : String); 

begin
  If (FcrosspostSource=AValue) then exit;
  FcrosspostSource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setgeocode(AIndex : Integer; AValue : String); 

begin
  If (Fgeocode=AValue) then exit;
  Fgeocode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setlocation(AIndex : Integer; AValue : TPlace); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Set_object(AIndex : Integer; AValue : TActivityTypeobject); 

begin
  If (F_object=AValue) then exit;
  F_object:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.SetplaceId(AIndex : Integer; AValue : String); 

begin
  If (FplaceId=AValue) then exit;
  FplaceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.SetplaceName(AIndex : Integer; AValue : String); 

begin
  If (FplaceName=AValue) then exit;
  FplaceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setprovider(AIndex : Integer; AValue : TActivityTypeprovider); 

begin
  If (Fprovider=AValue) then exit;
  Fprovider:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Set_published(AIndex : Integer; AValue : TDatetime); 

begin
  If (F_published=AValue) then exit;
  F_published:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setradius(AIndex : Integer; AValue : String); 

begin
  If (Fradius=AValue) then exit;
  Fradius:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setverb(AIndex : Integer; AValue : String); 

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


Procedure TActivityFeed.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityFeed.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityFeed.Setitems(AIndex : Integer; AValue : TActivityFeedTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityFeed.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityFeed.SetnextLink(AIndex : Integer; AValue : String); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityFeed.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityFeed.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityFeed.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityFeed.Setupdated(AIndex : Integer; AValue : TDatetime); 

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
  TCommentTypeactorTypeimage
  --------------------------------------------------------------------}


Procedure TCommentTypeactorTypeimage.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentTypeactor
  --------------------------------------------------------------------}


Procedure TCommentTypeactor.SetdisplayName(AIndex : Integer; AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentTypeactor.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentTypeactor.Setimage(AIndex : Integer; AValue : TCommentTypeactorTypeimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentTypeactor.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentTypeinReplyToItem
  --------------------------------------------------------------------}


Procedure TCommentTypeinReplyToItem.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentTypeinReplyToItem.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentTypeobject
  --------------------------------------------------------------------}


Procedure TCommentTypeobject.Setcontent(AIndex : Integer; AValue : String); 

begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentTypeobject.SetobjectType(AIndex : Integer; AValue : String); 

begin
  If (FobjectType=AValue) then exit;
  FobjectType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentTypeobject.SetoriginalContent(AIndex : Integer; AValue : String); 

begin
  If (ForiginalContent=AValue) then exit;
  ForiginalContent:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentTypeplusoners
  --------------------------------------------------------------------}


Procedure TCommentTypeplusoners.SettotalItems(AIndex : Integer; AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TComment
  --------------------------------------------------------------------}


Procedure TComment.Setactor(AIndex : Integer; AValue : TCommentTypeactor); 

begin
  If (Factor=AValue) then exit;
  Factor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.SetinReplyTo(AIndex : Integer; AValue : TCommentTypeinReplyToArray); 

begin
  If (FinReplyTo=AValue) then exit;
  FinReplyTo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Set_object(AIndex : Integer; AValue : TCommentTypeobject); 

begin
  If (F_object=AValue) then exit;
  F_object:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setplusoners(AIndex : Integer; AValue : TCommentTypeplusoners); 

begin
  If (Fplusoners=AValue) then exit;
  Fplusoners:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Set_published(AIndex : Integer; AValue : TDatetime); 

begin
  If (F_published=AValue) then exit;
  F_published:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setverb(AIndex : Integer; AValue : String); 

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


Procedure TCommentFeed.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentFeed.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentFeed.Setitems(AIndex : Integer; AValue : TCommentFeedTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentFeed.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentFeed.SetnextLink(AIndex : Integer; AValue : String); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentFeed.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentFeed.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentFeed.Setupdated(AIndex : Integer; AValue : TDatetime); 

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
  TItemScope
  --------------------------------------------------------------------}


Procedure TItemScope.Setabout(AIndex : Integer; AValue : TItemScope); 

begin
  If (Fabout=AValue) then exit;
  Fabout:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetadditionalName(AIndex : Integer; AValue : TStringArray); 

begin
  If (FadditionalName=AValue) then exit;
  FadditionalName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setaddress(AIndex : Integer; AValue : TItemScope); 

begin
  If (Faddress=AValue) then exit;
  Faddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetaddressCountry(AIndex : Integer; AValue : String); 

begin
  If (FaddressCountry=AValue) then exit;
  FaddressCountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetaddressLocality(AIndex : Integer; AValue : String); 

begin
  If (FaddressLocality=AValue) then exit;
  FaddressLocality:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetaddressRegion(AIndex : Integer; AValue : String); 

begin
  If (FaddressRegion=AValue) then exit;
  FaddressRegion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setassociated_media(AIndex : Integer; AValue : TItemScopeTypeassociated_mediaArray); 

begin
  If (Fassociated_media=AValue) then exit;
  Fassociated_media:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetattendeeCount(AIndex : Integer; AValue : integer); 

begin
  If (FattendeeCount=AValue) then exit;
  FattendeeCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setattendees(AIndex : Integer; AValue : TItemScopeTypeattendeesArray); 

begin
  If (Fattendees=AValue) then exit;
  Fattendees:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setaudio(AIndex : Integer; AValue : TItemScope); 

begin
  If (Faudio=AValue) then exit;
  Faudio:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setauthor(AIndex : Integer; AValue : TItemScopeTypeauthorArray); 

begin
  If (Fauthor=AValue) then exit;
  Fauthor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetbestRating(AIndex : Integer; AValue : String); 

begin
  If (FbestRating=AValue) then exit;
  FbestRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetbirthDate(AIndex : Integer; AValue : String); 

begin
  If (FbirthDate=AValue) then exit;
  FbirthDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetbyArtist(AIndex : Integer; AValue : TItemScope); 

begin
  If (FbyArtist=AValue) then exit;
  FbyArtist:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setcaption(AIndex : Integer; AValue : String); 

begin
  If (Fcaption=AValue) then exit;
  Fcaption:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetcontentSize(AIndex : Integer; AValue : String); 

begin
  If (FcontentSize=AValue) then exit;
  FcontentSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetcontentUrl(AIndex : Integer; AValue : String); 

begin
  If (FcontentUrl=AValue) then exit;
  FcontentUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setcontributor(AIndex : Integer; AValue : TItemScopeTypecontributorArray); 

begin
  If (Fcontributor=AValue) then exit;
  Fcontributor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetdateCreated(AIndex : Integer; AValue : String); 

begin
  If (FdateCreated=AValue) then exit;
  FdateCreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetdateModified(AIndex : Integer; AValue : String); 

begin
  If (FdateModified=AValue) then exit;
  FdateModified:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetdatePublished(AIndex : Integer; AValue : String); 

begin
  If (FdatePublished=AValue) then exit;
  FdatePublished:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setduration(AIndex : Integer; AValue : String); 

begin
  If (Fduration=AValue) then exit;
  Fduration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetembedUrl(AIndex : Integer; AValue : String); 

begin
  If (FembedUrl=AValue) then exit;
  FembedUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetendDate(AIndex : Integer; AValue : String); 

begin
  If (FendDate=AValue) then exit;
  FendDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetfamilyName(AIndex : Integer; AValue : String); 

begin
  If (FfamilyName=AValue) then exit;
  FfamilyName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setgender(AIndex : Integer; AValue : String); 

begin
  If (Fgender=AValue) then exit;
  Fgender:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setgeo(AIndex : Integer; AValue : TItemScope); 

begin
  If (Fgeo=AValue) then exit;
  Fgeo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetgivenName(AIndex : Integer; AValue : String); 

begin
  If (FgivenName=AValue) then exit;
  FgivenName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setheight(AIndex : Integer; AValue : String); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setimage(AIndex : Integer; AValue : String); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetinAlbum(AIndex : Integer; AValue : TItemScope); 

begin
  If (FinAlbum=AValue) then exit;
  FinAlbum:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setlatitude(AIndex : Integer; AValue : double); 

begin
  If (Flatitude=AValue) then exit;
  Flatitude:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setlocation(AIndex : Integer; AValue : TItemScope); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setlongitude(AIndex : Integer; AValue : double); 

begin
  If (Flongitude=AValue) then exit;
  Flongitude:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetpartOfTVSeries(AIndex : Integer; AValue : TItemScope); 

begin
  If (FpartOfTVSeries=AValue) then exit;
  FpartOfTVSeries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setperformers(AIndex : Integer; AValue : TItemScopeTypeperformersArray); 

begin
  If (Fperformers=AValue) then exit;
  Fperformers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetplayerType(AIndex : Integer; AValue : String); 

begin
  If (FplayerType=AValue) then exit;
  FplayerType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetpostOfficeBoxNumber(AIndex : Integer; AValue : String); 

begin
  If (FpostOfficeBoxNumber=AValue) then exit;
  FpostOfficeBoxNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetpostalCode(AIndex : Integer; AValue : String); 

begin
  If (FpostalCode=AValue) then exit;
  FpostalCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetratingValue(AIndex : Integer; AValue : String); 

begin
  If (FratingValue=AValue) then exit;
  FratingValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetreviewRating(AIndex : Integer; AValue : TItemScope); 

begin
  If (FreviewRating=AValue) then exit;
  FreviewRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetstartDate(AIndex : Integer; AValue : String); 

begin
  If (FstartDate=AValue) then exit;
  FstartDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetstreetAddress(AIndex : Integer; AValue : String); 

begin
  If (FstreetAddress=AValue) then exit;
  FstreetAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Settext(AIndex : Integer; AValue : String); 

begin
  If (Ftext=AValue) then exit;
  Ftext:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setthumbnail(AIndex : Integer; AValue : TItemScope); 

begin
  If (Fthumbnail=AValue) then exit;
  Fthumbnail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetthumbnailUrl(AIndex : Integer; AValue : String); 

begin
  If (FthumbnailUrl=AValue) then exit;
  FthumbnailUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SettickerSymbol(AIndex : Integer; AValue : String); 

begin
  If (FtickerSymbol=AValue) then exit;
  FtickerSymbol:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setwidth(AIndex : Integer; AValue : String); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetworstRating(AIndex : Integer; AValue : String); 

begin
  If (FworstRating=AValue) then exit;
  FworstRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TItemScope.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TItemScope.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'additionalname' : SetLength(FadditionalName,ALength);
  'associated_media' : SetLength(Fassociated_media,ALength);
  'attendees' : SetLength(Fattendees,ALength);
  'author' : SetLength(Fauthor,ALength);
  'contributor' : SetLength(Fcontributor,ALength);
  'performers' : SetLength(Fperformers,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMoment
  --------------------------------------------------------------------}


Procedure TMoment.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMoment.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMoment.Set_object(AIndex : Integer; AValue : TItemScope); 

begin
  If (F_object=AValue) then exit;
  F_object:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMoment.Setresult(AIndex : Integer; AValue : TItemScope); 

begin
  If (Fresult=AValue) then exit;
  Fresult:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMoment.SetstartDate(AIndex : Integer; AValue : TDatetime); 

begin
  If (FstartDate=AValue) then exit;
  FstartDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMoment.Settarget(AIndex : Integer; AValue : TItemScope); 

begin
  If (Ftarget=AValue) then exit;
  Ftarget:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMoment.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TMoment.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_object' : Result:='object';
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TMomentsFeed
  --------------------------------------------------------------------}


Procedure TMomentsFeed.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMomentsFeed.Setitems(AIndex : Integer; AValue : TMomentsFeedTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMomentsFeed.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMomentsFeed.SetnextLink(AIndex : Integer; AValue : String); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMomentsFeed.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMomentsFeed.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMomentsFeed.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMomentsFeed.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TMomentsFeed.SetArrayLength(Const AName : String; ALength : Longint); 

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


Procedure TPeopleFeed.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPeopleFeed.Setitems(AIndex : Integer; AValue : TPeopleFeedTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPeopleFeed.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPeopleFeed.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPeopleFeed.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPeopleFeed.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPeopleFeed.SettotalItems(AIndex : Integer; AValue : integer); 

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


Procedure TPersonTypeageRange.Setmax(AIndex : Integer; AValue : integer); 

begin
  If (Fmax=AValue) then exit;
  Fmax:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypeageRange.Setmin(AIndex : Integer; AValue : integer); 

begin
  If (Fmin=AValue) then exit;
  Fmin:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPersonTypecoverTypecoverInfo
  --------------------------------------------------------------------}


Procedure TPersonTypecoverTypecoverInfo.SetleftImageOffset(AIndex : Integer; AValue : integer); 

begin
  If (FleftImageOffset=AValue) then exit;
  FleftImageOffset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypecoverTypecoverInfo.SettopImageOffset(AIndex : Integer; AValue : integer); 

begin
  If (FtopImageOffset=AValue) then exit;
  FtopImageOffset:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPersonTypecoverTypecoverPhoto
  --------------------------------------------------------------------}


Procedure TPersonTypecoverTypecoverPhoto.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypecoverTypecoverPhoto.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypecoverTypecoverPhoto.Setwidth(AIndex : Integer; AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPersonTypecover
  --------------------------------------------------------------------}


Procedure TPersonTypecover.SetcoverInfo(AIndex : Integer; AValue : TPersonTypecoverTypecoverInfo); 

begin
  If (FcoverInfo=AValue) then exit;
  FcoverInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypecover.SetcoverPhoto(AIndex : Integer; AValue : TPersonTypecoverTypecoverPhoto); 

begin
  If (FcoverPhoto=AValue) then exit;
  FcoverPhoto:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypecover.Setlayout(AIndex : Integer; AValue : String); 

begin
  If (Flayout=AValue) then exit;
  Flayout:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPersonTypeemailsItem
  --------------------------------------------------------------------}


Procedure TPersonTypeemailsItem.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypeemailsItem.Setvalue(AIndex : Integer; AValue : String); 

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


Procedure TPersonTypeimage.SetisDefault(AIndex : Integer; AValue : boolean); 

begin
  If (FisDefault=AValue) then exit;
  FisDefault:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypeimage.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPersonTypename
  --------------------------------------------------------------------}


Procedure TPersonTypename.SetfamilyName(AIndex : Integer; AValue : String); 

begin
  If (FfamilyName=AValue) then exit;
  FfamilyName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypename.Setformatted(AIndex : Integer; AValue : String); 

begin
  If (Fformatted=AValue) then exit;
  Fformatted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypename.SetgivenName(AIndex : Integer; AValue : String); 

begin
  If (FgivenName=AValue) then exit;
  FgivenName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypename.SethonorificPrefix(AIndex : Integer; AValue : String); 

begin
  If (FhonorificPrefix=AValue) then exit;
  FhonorificPrefix:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypename.SethonorificSuffix(AIndex : Integer; AValue : String); 

begin
  If (FhonorificSuffix=AValue) then exit;
  FhonorificSuffix:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypename.SetmiddleName(AIndex : Integer; AValue : String); 

begin
  If (FmiddleName=AValue) then exit;
  FmiddleName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPersonTypeorganizationsItem
  --------------------------------------------------------------------}


Procedure TPersonTypeorganizationsItem.Setdepartment(AIndex : Integer; AValue : String); 

begin
  If (Fdepartment=AValue) then exit;
  Fdepartment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypeorganizationsItem.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypeorganizationsItem.SetendDate(AIndex : Integer; AValue : String); 

begin
  If (FendDate=AValue) then exit;
  FendDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypeorganizationsItem.Setlocation(AIndex : Integer; AValue : String); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypeorganizationsItem.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypeorganizationsItem.Setprimary(AIndex : Integer; AValue : boolean); 

begin
  If (Fprimary=AValue) then exit;
  Fprimary:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypeorganizationsItem.SetstartDate(AIndex : Integer; AValue : String); 

begin
  If (FstartDate=AValue) then exit;
  FstartDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypeorganizationsItem.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypeorganizationsItem.Set_type(AIndex : Integer; AValue : String); 

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


Procedure TPersonTypeplacesLivedItem.Setprimary(AIndex : Integer; AValue : boolean); 

begin
  If (Fprimary=AValue) then exit;
  Fprimary:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypeplacesLivedItem.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPersonTypeurlsItem
  --------------------------------------------------------------------}


Procedure TPersonTypeurlsItem.Set_label(AIndex : Integer; AValue : String); 

begin
  If (F_label=AValue) then exit;
  F_label:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypeurlsItem.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonTypeurlsItem.Setvalue(AIndex : Integer; AValue : String); 

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


Procedure TPerson.SetaboutMe(AIndex : Integer; AValue : String); 

begin
  If (FaboutMe=AValue) then exit;
  FaboutMe:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetageRange(AIndex : Integer; AValue : TPersonTypeageRange); 

begin
  If (FageRange=AValue) then exit;
  FageRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setbirthday(AIndex : Integer; AValue : String); 

begin
  If (Fbirthday=AValue) then exit;
  Fbirthday:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetbraggingRights(AIndex : Integer; AValue : String); 

begin
  If (FbraggingRights=AValue) then exit;
  FbraggingRights:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetcircledByCount(AIndex : Integer; AValue : integer); 

begin
  If (FcircledByCount=AValue) then exit;
  FcircledByCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setcover(AIndex : Integer; AValue : TPersonTypecover); 

begin
  If (Fcover=AValue) then exit;
  Fcover:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetcurrentLocation(AIndex : Integer; AValue : String); 

begin
  If (FcurrentLocation=AValue) then exit;
  FcurrentLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetdisplayName(AIndex : Integer; AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setdomain(AIndex : Integer; AValue : String); 

begin
  If (Fdomain=AValue) then exit;
  Fdomain:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setemails(AIndex : Integer; AValue : TPersonTypeemailsArray); 

begin
  If (Femails=AValue) then exit;
  Femails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setgender(AIndex : Integer; AValue : String); 

begin
  If (Fgender=AValue) then exit;
  Fgender:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setimage(AIndex : Integer; AValue : TPersonTypeimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetisPlusUser(AIndex : Integer; AValue : boolean); 

begin
  If (FisPlusUser=AValue) then exit;
  FisPlusUser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setlanguage(AIndex : Integer; AValue : String); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setname(AIndex : Integer; AValue : TPersonTypename); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setnickname(AIndex : Integer; AValue : String); 

begin
  If (Fnickname=AValue) then exit;
  Fnickname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetobjectType(AIndex : Integer; AValue : String); 

begin
  If (FobjectType=AValue) then exit;
  FobjectType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setoccupation(AIndex : Integer; AValue : String); 

begin
  If (Foccupation=AValue) then exit;
  Foccupation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setorganizations(AIndex : Integer; AValue : TPersonTypeorganizationsArray); 

begin
  If (Forganizations=AValue) then exit;
  Forganizations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetplacesLived(AIndex : Integer; AValue : TPersonTypeplacesLivedArray); 

begin
  If (FplacesLived=AValue) then exit;
  FplacesLived:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetplusOneCount(AIndex : Integer; AValue : integer); 

begin
  If (FplusOneCount=AValue) then exit;
  FplusOneCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetrelationshipStatus(AIndex : Integer; AValue : String); 

begin
  If (FrelationshipStatus=AValue) then exit;
  FrelationshipStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setskills(AIndex : Integer; AValue : String); 

begin
  If (Fskills=AValue) then exit;
  Fskills:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Settagline(AIndex : Integer; AValue : String); 

begin
  If (Ftagline=AValue) then exit;
  Ftagline:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Seturls(AIndex : Integer; AValue : TPersonTypeurlsArray); 

begin
  If (Furls=AValue) then exit;
  Furls:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setverified(AIndex : Integer; AValue : boolean); 

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


Procedure TPlaceTypeaddress.Setformatted(AIndex : Integer; AValue : String); 

begin
  If (Fformatted=AValue) then exit;
  Fformatted:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlaceTypeposition
  --------------------------------------------------------------------}


Procedure TPlaceTypeposition.Setlatitude(AIndex : Integer; AValue : double); 

begin
  If (Flatitude=AValue) then exit;
  Flatitude:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaceTypeposition.Setlongitude(AIndex : Integer; AValue : double); 

begin
  If (Flongitude=AValue) then exit;
  Flongitude:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlace
  --------------------------------------------------------------------}


Procedure TPlace.Setaddress(AIndex : Integer; AValue : TPlaceTypeaddress); 

begin
  If (Faddress=AValue) then exit;
  Faddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlace.SetdisplayName(AIndex : Integer; AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlace.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlace.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlace.Setposition(AIndex : Integer; AValue : TPlaceTypeposition); 

begin
  If (Fposition=AValue) then exit;
  Fposition:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlusAclentryResource
  --------------------------------------------------------------------}


Procedure TPlusAclentryResource.SetdisplayName(AIndex : Integer; AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlusAclentryResource.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlusAclentryResource.Set_type(AIndex : Integer; AValue : String); 

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
  TMomentsResource
  --------------------------------------------------------------------}


Class Function TMomentsResource.ResourceName : String;

begin
  Result:='moments';
end;

Class Function TMomentsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TplusAPI;
end;

Function TMomentsResource.Insert(collection: string; userId: string; aMoment : TMoment; AQuery : string = '') : TMoment;

Const
  _HTTPMethod = 'POST';
  _Path       = 'people/{userId}/moments/{collection}';
  _Methodid   = 'plus.moments.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['collection',collection,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aMoment,TMoment) as TMoment;
end;


Function TMomentsResource.Insert(collection: string; userId: string; aMoment : TMoment; AQuery : TMomentsinsertOptions) : TMoment;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'debug',AQuery.debug);
  Result:=Insert(collection,userId,aMoment,_Q);
end;

Function TMomentsResource.List(collection: string; userId: string; AQuery : string = '') : TMomentsFeed;

Const
  _HTTPMethod = 'GET';
  _Path       = 'people/{userId}/moments/{collection}';
  _Methodid   = 'plus.moments.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['collection',collection,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TMomentsFeed) as TMomentsFeed;
end;


Function TMomentsResource.List(collection: string; userId: string; AQuery : TMomentslistOptions) : TMomentsFeed;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'targetUrl',AQuery.targetUrl);
  AddToQuery(_Q,'type',AQuery._type);
  Result:=List(collection,userId,_Q);
end;

Procedure TMomentsResource.Remove(id: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'moments/{id}';
  _Methodid   = 'plus.moments.remove';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
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
  Result:='20150302';
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
  Result:='The Google+ API enables developers to build on top of the Google+ platform.';
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
  Result:='https://www.googleapis.com:443/';
end;

Class Function TPlusAPI.APIbasePath : string;

begin
  Result:='/plus/v1/';
end;

Class Function TPlusAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/plus/v1/';
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
  Result[0].Description:='Know your basic profile info and list of people in your circles.';
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
  TActivityTypeactorTypeimage.RegisterObject;
  TActivityTypeactorTypename.RegisterObject;
  TActivityTypeactor.RegisterObject;
  TActivityTypeobjectTypeactorTypeimage.RegisterObject;
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
  TCommentTypeactorTypeimage.RegisterObject;
  TCommentTypeactor.RegisterObject;
  TCommentTypeinReplyToItem.RegisterObject;
  TCommentTypeobject.RegisterObject;
  TCommentTypeplusoners.RegisterObject;
  TComment.RegisterObject;
  TCommentFeed.RegisterObject;
  TItemScope.RegisterObject;
  TMoment.RegisterObject;
  TMomentsFeed.RegisterObject;
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



Function TPlusAPI.GetMomentsInstance : TMomentsResource;

begin
  if (FMomentsInstance=Nil) then
    FMomentsInstance:=CreateMomentsResource;
  Result:=FMomentsInstance;
end;

Function TPlusAPI.CreateMomentsResource : TMomentsResource;

begin
  Result:=CreateMomentsResource(Self);
end;


Function TPlusAPI.CreateMomentsResource(AOwner : TComponent) : TMomentsResource;

begin
  Result:=TMomentsResource.Create(AOwner);
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
