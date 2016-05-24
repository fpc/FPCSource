unit googleplus;
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
  TAcl = class;
  TAclArray = Array of TAcl;
  TAclitems = class;
  TAclitemsArray = Array of TAclitems;
  TActivity = class;
  TActivityArray = Array of TActivity;
  TActivityactor = class;
  TActivityactorArray = Array of TActivityactor;
  TActivityactorimage = class;
  TActivityactorimageArray = Array of TActivityactorimage;
  TActivityactorname = class;
  TActivityactornameArray = Array of TActivityactorname;
  TActivityobject = class;
  TActivityobjectArray = Array of TActivityobject;
  TActivityobjectactor = class;
  TActivityobjectactorArray = Array of TActivityobjectactor;
  TActivityobjectactorimage = class;
  TActivityobjectactorimageArray = Array of TActivityobjectactorimage;
  TActivityobjectattachments = class;
  TActivityobjectattachmentsArray = Array of TActivityobjectattachments;
  TActivityobjectattachmentsembed = class;
  TActivityobjectattachmentsembedArray = Array of TActivityobjectattachmentsembed;
  TActivityobjectattachmentsfullImage = class;
  TActivityobjectattachmentsfullImageArray = Array of TActivityobjectattachmentsfullImage;
  TActivityobjectattachmentsimage = class;
  TActivityobjectattachmentsimageArray = Array of TActivityobjectattachmentsimage;
  TActivityobjectattachmentsthumbnails = class;
  TActivityobjectattachmentsthumbnailsArray = Array of TActivityobjectattachmentsthumbnails;
  TActivityobjectattachmentsthumbnailsimage = class;
  TActivityobjectattachmentsthumbnailsimageArray = Array of TActivityobjectattachmentsthumbnailsimage;
  TActivityobjectplusoners = class;
  TActivityobjectplusonersArray = Array of TActivityobjectplusoners;
  TActivityobjectreplies = class;
  TActivityobjectrepliesArray = Array of TActivityobjectreplies;
  TActivityobjectresharers = class;
  TActivityobjectresharersArray = Array of TActivityobjectresharers;
  TActivityprovider = class;
  TActivityproviderArray = Array of TActivityprovider;
  TActivityFeed = class;
  TActivityFeedArray = Array of TActivityFeed;
  TActivityFeeditems = class;
  TActivityFeeditemsArray = Array of TActivityFeeditems;
  TComment = class;
  TCommentArray = Array of TComment;
  TCommentactor = class;
  TCommentactorArray = Array of TCommentactor;
  TCommentactorimage = class;
  TCommentactorimageArray = Array of TCommentactorimage;
  TCommentinReplyTo = class;
  TCommentinReplyToArray = Array of TCommentinReplyTo;
  TCommentobject = class;
  TCommentobjectArray = Array of TCommentobject;
  TCommentplusoners = class;
  TCommentplusonersArray = Array of TCommentplusoners;
  TCommentFeed = class;
  TCommentFeedArray = Array of TCommentFeed;
  TCommentFeeditems = class;
  TCommentFeeditemsArray = Array of TCommentFeeditems;
  TItemScope = class;
  TItemScopeArray = Array of TItemScope;
  TItemScopeadditionalName = class;
  TItemScopeadditionalNameArray = Array of TItemScopeadditionalName;
  TItemScopeassociated_media = class;
  TItemScopeassociated_mediaArray = Array of TItemScopeassociated_media;
  TItemScopeattendees = class;
  TItemScopeattendeesArray = Array of TItemScopeattendees;
  TItemScopeauthor = class;
  TItemScopeauthorArray = Array of TItemScopeauthor;
  TItemScopecontributor = class;
  TItemScopecontributorArray = Array of TItemScopecontributor;
  TItemScopeperformers = class;
  TItemScopeperformersArray = Array of TItemScopeperformers;
  TMoment = class;
  TMomentArray = Array of TMoment;
  TMomentsFeed = class;
  TMomentsFeedArray = Array of TMomentsFeed;
  TMomentsFeeditems = class;
  TMomentsFeeditemsArray = Array of TMomentsFeeditems;
  TPeopleFeed = class;
  TPeopleFeedArray = Array of TPeopleFeed;
  TPeopleFeeditems = class;
  TPeopleFeeditemsArray = Array of TPeopleFeeditems;
  TPerson = class;
  TPersonArray = Array of TPerson;
  TPersonageRange = class;
  TPersonageRangeArray = Array of TPersonageRange;
  TPersoncover = class;
  TPersoncoverArray = Array of TPersoncover;
  TPersoncovercoverInfo = class;
  TPersoncovercoverInfoArray = Array of TPersoncovercoverInfo;
  TPersoncovercoverPhoto = class;
  TPersoncovercoverPhotoArray = Array of TPersoncovercoverPhoto;
  TPersonemails = class;
  TPersonemailsArray = Array of TPersonemails;
  TPersonimage = class;
  TPersonimageArray = Array of TPersonimage;
  TPersonname = class;
  TPersonnameArray = Array of TPersonname;
  TPersonorganizations = class;
  TPersonorganizationsArray = Array of TPersonorganizations;
  TPersonplacesLived = class;
  TPersonplacesLivedArray = Array of TPersonplacesLived;
  TPersonurls = class;
  TPersonurlsArray = Array of TPersonurls;
  TPlace = class;
  TPlaceArray = Array of TPlace;
  TPlaceaddress = class;
  TPlaceaddressArray = Array of TPlaceaddress;
  TPlaceposition = class;
  TPlacepositionArray = Array of TPlaceposition;
  TPlusAclentryResource = class;
  TPlusAclentryResourceArray = Array of TPlusAclentryResource;
  
  { --------------------------------------------------------------------
    TAcl
    --------------------------------------------------------------------}
  
  TAcl = Class(TGoogleBaseObject)
  Private
    Fdescription : string;
    Fitems : TAclitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TAclitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property description : string Index 0 Read Fdescription Write Setdescription;
    Property items : TAclitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
  end;
  TAclClass = Class of TAcl;
  
  { --------------------------------------------------------------------
    TAclitems
    --------------------------------------------------------------------}
  
  TAclitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAclitemsClass = Class of TAclitems;
  
  { --------------------------------------------------------------------
    TActivity
    --------------------------------------------------------------------}
  
  TActivity = Class(TGoogleBaseObject)
  Private
    Faccess : TAcl;
    Factor : TActivityactor;
    Faddress : string;
    Fannotation : string;
    FcrosspostSource : string;
    Fetag : string;
    Fgeocode : string;
    Fid : string;
    Fkind : string;
    Flocation : TPlace;
    F_object : TActivityobject;
    FplaceId : string;
    FplaceName : string;
    Fprovider : TActivityprovider;
    F_published : TDatetime;
    Fradius : string;
    Ftitle : string;
    Fupdated : TDatetime;
    Furl : string;
    Fverb : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setaccess(AIndex : Integer; AValue : TAcl); virtual;
    Procedure Setactor(AIndex : Integer; AValue : TActivityactor); virtual;
    Procedure Setaddress(AIndex : Integer; AValue : string); virtual;
    Procedure Setannotation(AIndex : Integer; AValue : string); virtual;
    Procedure SetcrosspostSource(AIndex : Integer; AValue : string); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setgeocode(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : TPlace); virtual;
    Procedure Set_object(AIndex : Integer; AValue : TActivityobject); virtual;
    Procedure SetplaceId(AIndex : Integer; AValue : string); virtual;
    Procedure SetplaceName(AIndex : Integer; AValue : string); virtual;
    Procedure Setprovider(AIndex : Integer; AValue : TActivityprovider); virtual;
    Procedure Set_published(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setradius(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
    Procedure Setverb(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property access : TAcl Index 0 Read Faccess Write Setaccess;
    Property actor : TActivityactor Index 8 Read Factor Write Setactor;
    Property address : string Index 16 Read Faddress Write Setaddress;
    Property annotation : string Index 24 Read Fannotation Write Setannotation;
    Property crosspostSource : string Index 32 Read FcrosspostSource Write SetcrosspostSource;
    Property etag : string Index 40 Read Fetag Write Setetag;
    Property geocode : string Index 48 Read Fgeocode Write Setgeocode;
    Property id : string Index 56 Read Fid Write Setid;
    Property kind : string Index 64 Read Fkind Write Setkind;
    Property location : TPlace Index 72 Read Flocation Write Setlocation;
    Property _object : TActivityobject Index 80 Read F_object Write Set_object;
    Property placeId : string Index 88 Read FplaceId Write SetplaceId;
    Property placeName : string Index 96 Read FplaceName Write SetplaceName;
    Property provider : TActivityprovider Index 104 Read Fprovider Write Setprovider;
    Property _published : TDatetime Index 112 Read F_published Write Set_published;
    Property radius : string Index 120 Read Fradius Write Setradius;
    Property title : string Index 128 Read Ftitle Write Settitle;
    Property updated : TDatetime Index 136 Read Fupdated Write Setupdated;
    Property url : string Index 144 Read Furl Write Seturl;
    Property verb : string Index 152 Read Fverb Write Setverb;
  end;
  TActivityClass = Class of TActivity;
  
  { --------------------------------------------------------------------
    TActivityactor
    --------------------------------------------------------------------}
  
  TActivityactor = Class(TGoogleBaseObject)
  Private
    FdisplayName : string;
    Fid : string;
    Fimage : TActivityactorimage;
    Fname : TActivityactorname;
    Furl : string;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setimage(AIndex : Integer; AValue : TActivityactorimage); virtual;
    Procedure Setname(AIndex : Integer; AValue : TActivityactorname); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property displayName : string Index 0 Read FdisplayName Write SetdisplayName;
    Property id : string Index 8 Read Fid Write Setid;
    Property image : TActivityactorimage Index 16 Read Fimage Write Setimage;
    Property name : TActivityactorname Index 24 Read Fname Write Setname;
    Property url : string Index 32 Read Furl Write Seturl;
  end;
  TActivityactorClass = Class of TActivityactor;
  
  { --------------------------------------------------------------------
    TActivityactorimage
    --------------------------------------------------------------------}
  
  TActivityactorimage = Class(TGoogleBaseObject)
  Private
    Furl : string;
  Protected
    //Property setters
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property url : string Index 0 Read Furl Write Seturl;
  end;
  TActivityactorimageClass = Class of TActivityactorimage;
  
  { --------------------------------------------------------------------
    TActivityactorname
    --------------------------------------------------------------------}
  
  TActivityactorname = Class(TGoogleBaseObject)
  Private
    FfamilyName : string;
    FgivenName : string;
  Protected
    //Property setters
    Procedure SetfamilyName(AIndex : Integer; AValue : string); virtual;
    Procedure SetgivenName(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property familyName : string Index 0 Read FfamilyName Write SetfamilyName;
    Property givenName : string Index 8 Read FgivenName Write SetgivenName;
  end;
  TActivityactornameClass = Class of TActivityactorname;
  
  { --------------------------------------------------------------------
    TActivityobject
    --------------------------------------------------------------------}
  
  TActivityobject = Class(TGoogleBaseObject)
  Private
    Factor : TActivityobjectactor;
    Fattachments : TActivityobjectattachments;
    Fcontent : string;
    Fid : string;
    FobjectType : string;
    ForiginalContent : string;
    Fplusoners : TActivityobjectplusoners;
    Freplies : TActivityobjectreplies;
    Fresharers : TActivityobjectresharers;
    Furl : string;
  Protected
    //Property setters
    Procedure Setactor(AIndex : Integer; AValue : TActivityobjectactor); virtual;
    Procedure Setattachments(AIndex : Integer; AValue : TActivityobjectattachments); virtual;
    Procedure Setcontent(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetobjectType(AIndex : Integer; AValue : string); virtual;
    Procedure SetoriginalContent(AIndex : Integer; AValue : string); virtual;
    Procedure Setplusoners(AIndex : Integer; AValue : TActivityobjectplusoners); virtual;
    Procedure Setreplies(AIndex : Integer; AValue : TActivityobjectreplies); virtual;
    Procedure Setresharers(AIndex : Integer; AValue : TActivityobjectresharers); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property actor : TActivityobjectactor Index 0 Read Factor Write Setactor;
    Property attachments : TActivityobjectattachments Index 8 Read Fattachments Write Setattachments;
    Property content : string Index 16 Read Fcontent Write Setcontent;
    Property id : string Index 24 Read Fid Write Setid;
    Property objectType : string Index 32 Read FobjectType Write SetobjectType;
    Property originalContent : string Index 40 Read ForiginalContent Write SetoriginalContent;
    Property plusoners : TActivityobjectplusoners Index 48 Read Fplusoners Write Setplusoners;
    Property replies : TActivityobjectreplies Index 56 Read Freplies Write Setreplies;
    Property resharers : TActivityobjectresharers Index 64 Read Fresharers Write Setresharers;
    Property url : string Index 72 Read Furl Write Seturl;
  end;
  TActivityobjectClass = Class of TActivityobject;
  
  { --------------------------------------------------------------------
    TActivityobjectactor
    --------------------------------------------------------------------}
  
  TActivityobjectactor = Class(TGoogleBaseObject)
  Private
    FdisplayName : string;
    Fid : string;
    Fimage : TActivityobjectactorimage;
    Furl : string;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setimage(AIndex : Integer; AValue : TActivityobjectactorimage); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property displayName : string Index 0 Read FdisplayName Write SetdisplayName;
    Property id : string Index 8 Read Fid Write Setid;
    Property image : TActivityobjectactorimage Index 16 Read Fimage Write Setimage;
    Property url : string Index 24 Read Furl Write Seturl;
  end;
  TActivityobjectactorClass = Class of TActivityobjectactor;
  
  { --------------------------------------------------------------------
    TActivityobjectactorimage
    --------------------------------------------------------------------}
  
  TActivityobjectactorimage = Class(TGoogleBaseObject)
  Private
    Furl : string;
  Protected
    //Property setters
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property url : string Index 0 Read Furl Write Seturl;
  end;
  TActivityobjectactorimageClass = Class of TActivityobjectactorimage;
  
  { --------------------------------------------------------------------
    TActivityobjectattachments
    --------------------------------------------------------------------}
  
  TActivityobjectattachments = Class(TGoogleBaseObject)
  Private
    Fcontent : string;
    FdisplayName : string;
    Fembed : TActivityobjectattachmentsembed;
    FfullImage : TActivityobjectattachmentsfullImage;
    Fid : string;
    Fimage : TActivityobjectattachmentsimage;
    FobjectType : string;
    Fthumbnails : TActivityobjectattachmentsthumbnails;
    Furl : string;
  Protected
    //Property setters
    Procedure Setcontent(AIndex : Integer; AValue : string); virtual;
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure Setembed(AIndex : Integer; AValue : TActivityobjectattachmentsembed); virtual;
    Procedure SetfullImage(AIndex : Integer; AValue : TActivityobjectattachmentsfullImage); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setimage(AIndex : Integer; AValue : TActivityobjectattachmentsimage); virtual;
    Procedure SetobjectType(AIndex : Integer; AValue : string); virtual;
    Procedure Setthumbnails(AIndex : Integer; AValue : TActivityobjectattachmentsthumbnails); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property content : string Index 0 Read Fcontent Write Setcontent;
    Property displayName : string Index 8 Read FdisplayName Write SetdisplayName;
    Property embed : TActivityobjectattachmentsembed Index 16 Read Fembed Write Setembed;
    Property fullImage : TActivityobjectattachmentsfullImage Index 24 Read FfullImage Write SetfullImage;
    Property id : string Index 32 Read Fid Write Setid;
    Property image : TActivityobjectattachmentsimage Index 40 Read Fimage Write Setimage;
    Property objectType : string Index 48 Read FobjectType Write SetobjectType;
    Property thumbnails : TActivityobjectattachmentsthumbnails Index 56 Read Fthumbnails Write Setthumbnails;
    Property url : string Index 64 Read Furl Write Seturl;
  end;
  TActivityobjectattachmentsClass = Class of TActivityobjectattachments;
  
  { --------------------------------------------------------------------
    TActivityobjectattachmentsembed
    --------------------------------------------------------------------}
  
  TActivityobjectattachmentsembed = Class(TGoogleBaseObject)
  Private
    F_type : string;
    Furl : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property _type : string Index 0 Read F_type Write Set_type;
    Property url : string Index 8 Read Furl Write Seturl;
  end;
  TActivityobjectattachmentsembedClass = Class of TActivityobjectattachmentsembed;
  
  { --------------------------------------------------------------------
    TActivityobjectattachmentsfullImage
    --------------------------------------------------------------------}
  
  TActivityobjectattachmentsfullImage = Class(TGoogleBaseObject)
  Private
    Fheight : integer;
    F_type : string;
    Furl : string;
    Fwidth : integer;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setheight(AIndex : Integer; AValue : integer); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property height : integer Index 0 Read Fheight Write Setheight;
    Property _type : string Index 8 Read F_type Write Set_type;
    Property url : string Index 16 Read Furl Write Seturl;
    Property width : integer Index 24 Read Fwidth Write Setwidth;
  end;
  TActivityobjectattachmentsfullImageClass = Class of TActivityobjectattachmentsfullImage;
  
  { --------------------------------------------------------------------
    TActivityobjectattachmentsimage
    --------------------------------------------------------------------}
  
  TActivityobjectattachmentsimage = Class(TGoogleBaseObject)
  Private
    Fheight : integer;
    F_type : string;
    Furl : string;
    Fwidth : integer;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setheight(AIndex : Integer; AValue : integer); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property height : integer Index 0 Read Fheight Write Setheight;
    Property _type : string Index 8 Read F_type Write Set_type;
    Property url : string Index 16 Read Furl Write Seturl;
    Property width : integer Index 24 Read Fwidth Write Setwidth;
  end;
  TActivityobjectattachmentsimageClass = Class of TActivityobjectattachmentsimage;
  
  { --------------------------------------------------------------------
    TActivityobjectattachmentsthumbnails
    --------------------------------------------------------------------}
  
  TActivityobjectattachmentsthumbnails = Class(TGoogleBaseObject)
  Private
    Fdescription : string;
    Fimage : TActivityobjectattachmentsthumbnailsimage;
    Furl : string;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setimage(AIndex : Integer; AValue : TActivityobjectattachmentsthumbnailsimage); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property description : string Index 0 Read Fdescription Write Setdescription;
    Property image : TActivityobjectattachmentsthumbnailsimage Index 8 Read Fimage Write Setimage;
    Property url : string Index 16 Read Furl Write Seturl;
  end;
  TActivityobjectattachmentsthumbnailsClass = Class of TActivityobjectattachmentsthumbnails;
  
  { --------------------------------------------------------------------
    TActivityobjectattachmentsthumbnailsimage
    --------------------------------------------------------------------}
  
  TActivityobjectattachmentsthumbnailsimage = Class(TGoogleBaseObject)
  Private
    Fheight : integer;
    F_type : string;
    Furl : string;
    Fwidth : integer;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setheight(AIndex : Integer; AValue : integer); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property height : integer Index 0 Read Fheight Write Setheight;
    Property _type : string Index 8 Read F_type Write Set_type;
    Property url : string Index 16 Read Furl Write Seturl;
    Property width : integer Index 24 Read Fwidth Write Setwidth;
  end;
  TActivityobjectattachmentsthumbnailsimageClass = Class of TActivityobjectattachmentsthumbnailsimage;
  
  { --------------------------------------------------------------------
    TActivityobjectplusoners
    --------------------------------------------------------------------}
  
  TActivityobjectplusoners = Class(TGoogleBaseObject)
  Private
    FselfLink : string;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property selfLink : string Index 0 Read FselfLink Write SetselfLink;
    Property totalItems : integer Index 8 Read FtotalItems Write SettotalItems;
  end;
  TActivityobjectplusonersClass = Class of TActivityobjectplusoners;
  
  { --------------------------------------------------------------------
    TActivityobjectreplies
    --------------------------------------------------------------------}
  
  TActivityobjectreplies = Class(TGoogleBaseObject)
  Private
    FselfLink : string;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property selfLink : string Index 0 Read FselfLink Write SetselfLink;
    Property totalItems : integer Index 8 Read FtotalItems Write SettotalItems;
  end;
  TActivityobjectrepliesClass = Class of TActivityobjectreplies;
  
  { --------------------------------------------------------------------
    TActivityobjectresharers
    --------------------------------------------------------------------}
  
  TActivityobjectresharers = Class(TGoogleBaseObject)
  Private
    FselfLink : string;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property selfLink : string Index 0 Read FselfLink Write SetselfLink;
    Property totalItems : integer Index 8 Read FtotalItems Write SettotalItems;
  end;
  TActivityobjectresharersClass = Class of TActivityobjectresharers;
  
  { --------------------------------------------------------------------
    TActivityprovider
    --------------------------------------------------------------------}
  
  TActivityprovider = Class(TGoogleBaseObject)
  Private
    Ftitle : string;
  Protected
    //Property setters
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property title : string Index 0 Read Ftitle Write Settitle;
  end;
  TActivityproviderClass = Class of TActivityprovider;
  
  { --------------------------------------------------------------------
    TActivityFeed
    --------------------------------------------------------------------}
  
  TActivityFeed = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fid : string;
    Fitems : TActivityFeeditems;
    Fkind : string;
    FnextLink : string;
    FnextPageToken : string;
    FselfLink : string;
    Ftitle : string;
    Fupdated : TDatetime;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TActivityFeeditems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property id : string Index 8 Read Fid Write Setid;
    Property items : TActivityFeeditems Index 16 Read Fitems Write Setitems;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property nextLink : string Index 32 Read FnextLink Write SetnextLink;
    Property nextPageToken : string Index 40 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 48 Read FselfLink Write SetselfLink;
    Property title : string Index 56 Read Ftitle Write Settitle;
    Property updated : TDatetime Index 64 Read Fupdated Write Setupdated;
  end;
  TActivityFeedClass = Class of TActivityFeed;
  
  { --------------------------------------------------------------------
    TActivityFeeditems
    --------------------------------------------------------------------}
  
  TActivityFeeditems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TActivityFeeditemsClass = Class of TActivityFeeditems;
  
  { --------------------------------------------------------------------
    TComment
    --------------------------------------------------------------------}
  
  TComment = Class(TGoogleBaseObject)
  Private
    Factor : TCommentactor;
    Fetag : string;
    Fid : string;
    FinReplyTo : TCommentinReplyTo;
    Fkind : string;
    F_object : TCommentobject;
    Fplusoners : TCommentplusoners;
    F_published : TDatetime;
    FselfLink : string;
    Fupdated : TDatetime;
    Fverb : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setactor(AIndex : Integer; AValue : TCommentactor); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetinReplyTo(AIndex : Integer; AValue : TCommentinReplyTo); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Set_object(AIndex : Integer; AValue : TCommentobject); virtual;
    Procedure Setplusoners(AIndex : Integer; AValue : TCommentplusoners); virtual;
    Procedure Set_published(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setverb(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property actor : TCommentactor Index 0 Read Factor Write Setactor;
    Property etag : string Index 8 Read Fetag Write Setetag;
    Property id : string Index 16 Read Fid Write Setid;
    Property inReplyTo : TCommentinReplyTo Index 24 Read FinReplyTo Write SetinReplyTo;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property _object : TCommentobject Index 40 Read F_object Write Set_object;
    Property plusoners : TCommentplusoners Index 48 Read Fplusoners Write Setplusoners;
    Property _published : TDatetime Index 56 Read F_published Write Set_published;
    Property selfLink : string Index 64 Read FselfLink Write SetselfLink;
    Property updated : TDatetime Index 72 Read Fupdated Write Setupdated;
    Property verb : string Index 80 Read Fverb Write Setverb;
  end;
  TCommentClass = Class of TComment;
  
  { --------------------------------------------------------------------
    TCommentactor
    --------------------------------------------------------------------}
  
  TCommentactor = Class(TGoogleBaseObject)
  Private
    FdisplayName : string;
    Fid : string;
    Fimage : TCommentactorimage;
    Furl : string;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setimage(AIndex : Integer; AValue : TCommentactorimage); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property displayName : string Index 0 Read FdisplayName Write SetdisplayName;
    Property id : string Index 8 Read Fid Write Setid;
    Property image : TCommentactorimage Index 16 Read Fimage Write Setimage;
    Property url : string Index 24 Read Furl Write Seturl;
  end;
  TCommentactorClass = Class of TCommentactor;
  
  { --------------------------------------------------------------------
    TCommentactorimage
    --------------------------------------------------------------------}
  
  TCommentactorimage = Class(TGoogleBaseObject)
  Private
    Furl : string;
  Protected
    //Property setters
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property url : string Index 0 Read Furl Write Seturl;
  end;
  TCommentactorimageClass = Class of TCommentactorimage;
  
  { --------------------------------------------------------------------
    TCommentinReplyTo
    --------------------------------------------------------------------}
  
  TCommentinReplyTo = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Furl : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property url : string Index 8 Read Furl Write Seturl;
  end;
  TCommentinReplyToClass = Class of TCommentinReplyTo;
  
  { --------------------------------------------------------------------
    TCommentobject
    --------------------------------------------------------------------}
  
  TCommentobject = Class(TGoogleBaseObject)
  Private
    Fcontent : string;
    FobjectType : string;
    ForiginalContent : string;
  Protected
    //Property setters
    Procedure Setcontent(AIndex : Integer; AValue : string); virtual;
    Procedure SetobjectType(AIndex : Integer; AValue : string); virtual;
    Procedure SetoriginalContent(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property content : string Index 0 Read Fcontent Write Setcontent;
    Property objectType : string Index 8 Read FobjectType Write SetobjectType;
    Property originalContent : string Index 16 Read ForiginalContent Write SetoriginalContent;
  end;
  TCommentobjectClass = Class of TCommentobject;
  
  { --------------------------------------------------------------------
    TCommentplusoners
    --------------------------------------------------------------------}
  
  TCommentplusoners = Class(TGoogleBaseObject)
  Private
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property totalItems : integer Index 0 Read FtotalItems Write SettotalItems;
  end;
  TCommentplusonersClass = Class of TCommentplusoners;
  
  { --------------------------------------------------------------------
    TCommentFeed
    --------------------------------------------------------------------}
  
  TCommentFeed = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fid : string;
    Fitems : TCommentFeeditems;
    Fkind : string;
    FnextLink : string;
    FnextPageToken : string;
    Ftitle : string;
    Fupdated : TDatetime;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TCommentFeeditems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property id : string Index 8 Read Fid Write Setid;
    Property items : TCommentFeeditems Index 16 Read Fitems Write Setitems;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property nextLink : string Index 32 Read FnextLink Write SetnextLink;
    Property nextPageToken : string Index 40 Read FnextPageToken Write SetnextPageToken;
    Property title : string Index 48 Read Ftitle Write Settitle;
    Property updated : TDatetime Index 56 Read Fupdated Write Setupdated;
  end;
  TCommentFeedClass = Class of TCommentFeed;
  
  { --------------------------------------------------------------------
    TCommentFeeditems
    --------------------------------------------------------------------}
  
  TCommentFeeditems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCommentFeeditemsClass = Class of TCommentFeeditems;
  
  { --------------------------------------------------------------------
    TItemScope
    --------------------------------------------------------------------}
  
  TItemScope = Class(TGoogleBaseObject)
  Private
    Fabout : TItemScope;
    FadditionalName : TItemScopeadditionalName;
    Faddress : TItemScope;
    FaddressCountry : string;
    FaddressLocality : string;
    FaddressRegion : string;
    Fassociated_media : TItemScopeassociated_media;
    FattendeeCount : integer;
    Fattendees : TItemScopeattendees;
    Faudio : TItemScope;
    Fauthor : TItemScopeauthor;
    FbestRating : string;
    FbirthDate : string;
    FbyArtist : TItemScope;
    Fcaption : string;
    FcontentSize : string;
    FcontentUrl : string;
    Fcontributor : TItemScopecontributor;
    FdateCreated : string;
    FdateModified : string;
    FdatePublished : string;
    Fdescription : string;
    Fduration : string;
    FembedUrl : string;
    FendDate : string;
    FfamilyName : string;
    Fgender : string;
    Fgeo : TItemScope;
    FgivenName : string;
    Fheight : string;
    Fid : string;
    Fimage : string;
    FinAlbum : TItemScope;
    Fkind : string;
    Flatitude : double;
    Flocation : TItemScope;
    Flongitude : double;
    Fname : string;
    FpartOfTVSeries : TItemScope;
    Fperformers : TItemScopeperformers;
    FplayerType : string;
    FpostOfficeBoxNumber : string;
    FpostalCode : string;
    FratingValue : string;
    FreviewRating : TItemScope;
    FstartDate : string;
    FstreetAddress : string;
    Ftext : string;
    Fthumbnail : TItemScope;
    FthumbnailUrl : string;
    FtickerSymbol : string;
    F_type : string;
    Furl : string;
    Fwidth : string;
    FworstRating : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setabout(AIndex : Integer; AValue : TItemScope); virtual;
    Procedure SetadditionalName(AIndex : Integer; AValue : TItemScopeadditionalName); virtual;
    Procedure Setaddress(AIndex : Integer; AValue : TItemScope); virtual;
    Procedure SetaddressCountry(AIndex : Integer; AValue : string); virtual;
    Procedure SetaddressLocality(AIndex : Integer; AValue : string); virtual;
    Procedure SetaddressRegion(AIndex : Integer; AValue : string); virtual;
    Procedure Setassociated_media(AIndex : Integer; AValue : TItemScopeassociated_media); virtual;
    Procedure SetattendeeCount(AIndex : Integer; AValue : integer); virtual;
    Procedure Setattendees(AIndex : Integer; AValue : TItemScopeattendees); virtual;
    Procedure Setaudio(AIndex : Integer; AValue : TItemScope); virtual;
    Procedure Setauthor(AIndex : Integer; AValue : TItemScopeauthor); virtual;
    Procedure SetbestRating(AIndex : Integer; AValue : string); virtual;
    Procedure SetbirthDate(AIndex : Integer; AValue : string); virtual;
    Procedure SetbyArtist(AIndex : Integer; AValue : TItemScope); virtual;
    Procedure Setcaption(AIndex : Integer; AValue : string); virtual;
    Procedure SetcontentSize(AIndex : Integer; AValue : string); virtual;
    Procedure SetcontentUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Setcontributor(AIndex : Integer; AValue : TItemScopecontributor); virtual;
    Procedure SetdateCreated(AIndex : Integer; AValue : string); virtual;
    Procedure SetdateModified(AIndex : Integer; AValue : string); virtual;
    Procedure SetdatePublished(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setduration(AIndex : Integer; AValue : string); virtual;
    Procedure SetembedUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SetendDate(AIndex : Integer; AValue : string); virtual;
    Procedure SetfamilyName(AIndex : Integer; AValue : string); virtual;
    Procedure Setgender(AIndex : Integer; AValue : string); virtual;
    Procedure Setgeo(AIndex : Integer; AValue : TItemScope); virtual;
    Procedure SetgivenName(AIndex : Integer; AValue : string); virtual;
    Procedure Setheight(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setimage(AIndex : Integer; AValue : string); virtual;
    Procedure SetinAlbum(AIndex : Integer; AValue : TItemScope); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlatitude(AIndex : Integer; AValue : double); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : TItemScope); virtual;
    Procedure Setlongitude(AIndex : Integer; AValue : double); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetpartOfTVSeries(AIndex : Integer; AValue : TItemScope); virtual;
    Procedure Setperformers(AIndex : Integer; AValue : TItemScopeperformers); virtual;
    Procedure SetplayerType(AIndex : Integer; AValue : string); virtual;
    Procedure SetpostOfficeBoxNumber(AIndex : Integer; AValue : string); virtual;
    Procedure SetpostalCode(AIndex : Integer; AValue : string); virtual;
    Procedure SetratingValue(AIndex : Integer; AValue : string); virtual;
    Procedure SetreviewRating(AIndex : Integer; AValue : TItemScope); virtual;
    Procedure SetstartDate(AIndex : Integer; AValue : string); virtual;
    Procedure SetstreetAddress(AIndex : Integer; AValue : string); virtual;
    Procedure Settext(AIndex : Integer; AValue : string); virtual;
    Procedure Setthumbnail(AIndex : Integer; AValue : TItemScope); virtual;
    Procedure SetthumbnailUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SettickerSymbol(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : string); virtual;
    Procedure SetworstRating(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property about : TItemScope Index 0 Read Fabout Write Setabout;
    Property additionalName : TItemScopeadditionalName Index 8 Read FadditionalName Write SetadditionalName;
    Property address : TItemScope Index 16 Read Faddress Write Setaddress;
    Property addressCountry : string Index 24 Read FaddressCountry Write SetaddressCountry;
    Property addressLocality : string Index 32 Read FaddressLocality Write SetaddressLocality;
    Property addressRegion : string Index 40 Read FaddressRegion Write SetaddressRegion;
    Property associated_media : TItemScopeassociated_media Index 48 Read Fassociated_media Write Setassociated_media;
    Property attendeeCount : integer Index 56 Read FattendeeCount Write SetattendeeCount;
    Property attendees : TItemScopeattendees Index 64 Read Fattendees Write Setattendees;
    Property audio : TItemScope Index 72 Read Faudio Write Setaudio;
    Property author : TItemScopeauthor Index 80 Read Fauthor Write Setauthor;
    Property bestRating : string Index 88 Read FbestRating Write SetbestRating;
    Property birthDate : string Index 96 Read FbirthDate Write SetbirthDate;
    Property byArtist : TItemScope Index 104 Read FbyArtist Write SetbyArtist;
    Property caption : string Index 112 Read Fcaption Write Setcaption;
    Property contentSize : string Index 120 Read FcontentSize Write SetcontentSize;
    Property contentUrl : string Index 128 Read FcontentUrl Write SetcontentUrl;
    Property contributor : TItemScopecontributor Index 136 Read Fcontributor Write Setcontributor;
    Property dateCreated : string Index 144 Read FdateCreated Write SetdateCreated;
    Property dateModified : string Index 152 Read FdateModified Write SetdateModified;
    Property datePublished : string Index 160 Read FdatePublished Write SetdatePublished;
    Property description : string Index 168 Read Fdescription Write Setdescription;
    Property duration : string Index 176 Read Fduration Write Setduration;
    Property embedUrl : string Index 184 Read FembedUrl Write SetembedUrl;
    Property endDate : string Index 192 Read FendDate Write SetendDate;
    Property familyName : string Index 200 Read FfamilyName Write SetfamilyName;
    Property gender : string Index 208 Read Fgender Write Setgender;
    Property geo : TItemScope Index 216 Read Fgeo Write Setgeo;
    Property givenName : string Index 224 Read FgivenName Write SetgivenName;
    Property height : string Index 232 Read Fheight Write Setheight;
    Property id : string Index 240 Read Fid Write Setid;
    Property image : string Index 248 Read Fimage Write Setimage;
    Property inAlbum : TItemScope Index 256 Read FinAlbum Write SetinAlbum;
    Property kind : string Index 264 Read Fkind Write Setkind;
    Property latitude : double Index 272 Read Flatitude Write Setlatitude;
    Property location : TItemScope Index 280 Read Flocation Write Setlocation;
    Property longitude : double Index 288 Read Flongitude Write Setlongitude;
    Property name : string Index 296 Read Fname Write Setname;
    Property partOfTVSeries : TItemScope Index 304 Read FpartOfTVSeries Write SetpartOfTVSeries;
    Property performers : TItemScopeperformers Index 312 Read Fperformers Write Setperformers;
    Property playerType : string Index 320 Read FplayerType Write SetplayerType;
    Property postOfficeBoxNumber : string Index 328 Read FpostOfficeBoxNumber Write SetpostOfficeBoxNumber;
    Property postalCode : string Index 336 Read FpostalCode Write SetpostalCode;
    Property ratingValue : string Index 344 Read FratingValue Write SetratingValue;
    Property reviewRating : TItemScope Index 352 Read FreviewRating Write SetreviewRating;
    Property startDate : string Index 360 Read FstartDate Write SetstartDate;
    Property streetAddress : string Index 368 Read FstreetAddress Write SetstreetAddress;
    Property text : string Index 376 Read Ftext Write Settext;
    Property thumbnail : TItemScope Index 384 Read Fthumbnail Write Setthumbnail;
    Property thumbnailUrl : string Index 392 Read FthumbnailUrl Write SetthumbnailUrl;
    Property tickerSymbol : string Index 400 Read FtickerSymbol Write SettickerSymbol;
    Property _type : string Index 408 Read F_type Write Set_type;
    Property url : string Index 416 Read Furl Write Seturl;
    Property width : string Index 424 Read Fwidth Write Setwidth;
    Property worstRating : string Index 432 Read FworstRating Write SetworstRating;
  end;
  TItemScopeClass = Class of TItemScope;
  
  { --------------------------------------------------------------------
    TItemScopeadditionalName
    --------------------------------------------------------------------}
  
  TItemScopeadditionalName = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TItemScopeadditionalNameClass = Class of TItemScopeadditionalName;
  
  { --------------------------------------------------------------------
    TItemScopeassociated_media
    --------------------------------------------------------------------}
  
  TItemScopeassociated_media = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TItemScopeassociated_mediaClass = Class of TItemScopeassociated_media;
  
  { --------------------------------------------------------------------
    TItemScopeattendees
    --------------------------------------------------------------------}
  
  TItemScopeattendees = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TItemScopeattendeesClass = Class of TItemScopeattendees;
  
  { --------------------------------------------------------------------
    TItemScopeauthor
    --------------------------------------------------------------------}
  
  TItemScopeauthor = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TItemScopeauthorClass = Class of TItemScopeauthor;
  
  { --------------------------------------------------------------------
    TItemScopecontributor
    --------------------------------------------------------------------}
  
  TItemScopecontributor = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TItemScopecontributorClass = Class of TItemScopecontributor;
  
  { --------------------------------------------------------------------
    TItemScopeperformers
    --------------------------------------------------------------------}
  
  TItemScopeperformers = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TItemScopeperformersClass = Class of TItemScopeperformers;
  
  { --------------------------------------------------------------------
    TMoment
    --------------------------------------------------------------------}
  
  TMoment = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fkind : string;
    F_object : TItemScope;
    Fresult : TItemScope;
    FstartDate : TDatetime;
    Ftarget : TItemScope;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Set_object(AIndex : Integer; AValue : TItemScope); virtual;
    Procedure Setresult(AIndex : Integer; AValue : TItemScope); virtual;
    Procedure SetstartDate(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Settarget(AIndex : Integer; AValue : TItemScope); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property _object : TItemScope Index 16 Read F_object Write Set_object;
    Property result : TItemScope Index 24 Read Fresult Write Setresult;
    Property startDate : TDatetime Index 32 Read FstartDate Write SetstartDate;
    Property target : TItemScope Index 40 Read Ftarget Write Settarget;
    Property _type : string Index 48 Read F_type Write Set_type;
  end;
  TMomentClass = Class of TMoment;
  
  { --------------------------------------------------------------------
    TMomentsFeed
    --------------------------------------------------------------------}
  
  TMomentsFeed = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TMomentsFeeditems;
    Fkind : string;
    FnextLink : string;
    FnextPageToken : string;
    FselfLink : string;
    Ftitle : string;
    Fupdated : TDatetime;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TMomentsFeeditems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TMomentsFeeditems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextLink : string Index 24 Read FnextLink Write SetnextLink;
    Property nextPageToken : string Index 32 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 40 Read FselfLink Write SetselfLink;
    Property title : string Index 48 Read Ftitle Write Settitle;
    Property updated : TDatetime Index 56 Read Fupdated Write Setupdated;
  end;
  TMomentsFeedClass = Class of TMomentsFeed;
  
  { --------------------------------------------------------------------
    TMomentsFeeditems
    --------------------------------------------------------------------}
  
  TMomentsFeeditems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMomentsFeeditemsClass = Class of TMomentsFeeditems;
  
  { --------------------------------------------------------------------
    TPeopleFeed
    --------------------------------------------------------------------}
  
  TPeopleFeed = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TPeopleFeeditems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
    Ftitle : string;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TPeopleFeeditems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TPeopleFeeditems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
    Property title : string Index 40 Read Ftitle Write Settitle;
    Property totalItems : integer Index 48 Read FtotalItems Write SettotalItems;
  end;
  TPeopleFeedClass = Class of TPeopleFeed;
  
  { --------------------------------------------------------------------
    TPeopleFeeditems
    --------------------------------------------------------------------}
  
  TPeopleFeeditems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPeopleFeeditemsClass = Class of TPeopleFeeditems;
  
  { --------------------------------------------------------------------
    TPerson
    --------------------------------------------------------------------}
  
  TPerson = Class(TGoogleBaseObject)
  Private
    FaboutMe : string;
    FageRange : TPersonageRange;
    Fbirthday : string;
    FbraggingRights : string;
    FcircledByCount : integer;
    Fcover : TPersoncover;
    FcurrentLocation : string;
    FdisplayName : string;
    Fdomain : string;
    Femails : TPersonemails;
    Fetag : string;
    Fgender : string;
    Fid : string;
    Fimage : TPersonimage;
    FisPlusUser : boolean;
    Fkind : string;
    Flanguage : string;
    Fname : TPersonname;
    Fnickname : string;
    FobjectType : string;
    Foccupation : string;
    Forganizations : TPersonorganizations;
    FplacesLived : TPersonplacesLived;
    FplusOneCount : integer;
    FrelationshipStatus : string;
    Fskills : string;
    Ftagline : string;
    Furl : string;
    Furls : TPersonurls;
    Fverified : boolean;
  Protected
    //Property setters
    Procedure SetaboutMe(AIndex : Integer; AValue : string); virtual;
    Procedure SetageRange(AIndex : Integer; AValue : TPersonageRange); virtual;
    Procedure Setbirthday(AIndex : Integer; AValue : string); virtual;
    Procedure SetbraggingRights(AIndex : Integer; AValue : string); virtual;
    Procedure SetcircledByCount(AIndex : Integer; AValue : integer); virtual;
    Procedure Setcover(AIndex : Integer; AValue : TPersoncover); virtual;
    Procedure SetcurrentLocation(AIndex : Integer; AValue : string); virtual;
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure Setdomain(AIndex : Integer; AValue : string); virtual;
    Procedure Setemails(AIndex : Integer; AValue : TPersonemails); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setgender(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setimage(AIndex : Integer; AValue : TPersonimage); virtual;
    Procedure SetisPlusUser(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlanguage(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : TPersonname); virtual;
    Procedure Setnickname(AIndex : Integer; AValue : string); virtual;
    Procedure SetobjectType(AIndex : Integer; AValue : string); virtual;
    Procedure Setoccupation(AIndex : Integer; AValue : string); virtual;
    Procedure Setorganizations(AIndex : Integer; AValue : TPersonorganizations); virtual;
    Procedure SetplacesLived(AIndex : Integer; AValue : TPersonplacesLived); virtual;
    Procedure SetplusOneCount(AIndex : Integer; AValue : integer); virtual;
    Procedure SetrelationshipStatus(AIndex : Integer; AValue : string); virtual;
    Procedure Setskills(AIndex : Integer; AValue : string); virtual;
    Procedure Settagline(AIndex : Integer; AValue : string); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
    Procedure Seturls(AIndex : Integer; AValue : TPersonurls); virtual;
    Procedure Setverified(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property aboutMe : string Index 0 Read FaboutMe Write SetaboutMe;
    Property ageRange : TPersonageRange Index 8 Read FageRange Write SetageRange;
    Property birthday : string Index 16 Read Fbirthday Write Setbirthday;
    Property braggingRights : string Index 24 Read FbraggingRights Write SetbraggingRights;
    Property circledByCount : integer Index 32 Read FcircledByCount Write SetcircledByCount;
    Property cover : TPersoncover Index 40 Read Fcover Write Setcover;
    Property currentLocation : string Index 48 Read FcurrentLocation Write SetcurrentLocation;
    Property displayName : string Index 56 Read FdisplayName Write SetdisplayName;
    Property domain : string Index 64 Read Fdomain Write Setdomain;
    Property emails : TPersonemails Index 72 Read Femails Write Setemails;
    Property etag : string Index 80 Read Fetag Write Setetag;
    Property gender : string Index 88 Read Fgender Write Setgender;
    Property id : string Index 96 Read Fid Write Setid;
    Property image : TPersonimage Index 104 Read Fimage Write Setimage;
    Property isPlusUser : boolean Index 112 Read FisPlusUser Write SetisPlusUser;
    Property kind : string Index 120 Read Fkind Write Setkind;
    Property language : string Index 128 Read Flanguage Write Setlanguage;
    Property name : TPersonname Index 136 Read Fname Write Setname;
    Property nickname : string Index 144 Read Fnickname Write Setnickname;
    Property objectType : string Index 152 Read FobjectType Write SetobjectType;
    Property occupation : string Index 160 Read Foccupation Write Setoccupation;
    Property organizations : TPersonorganizations Index 168 Read Forganizations Write Setorganizations;
    Property placesLived : TPersonplacesLived Index 176 Read FplacesLived Write SetplacesLived;
    Property plusOneCount : integer Index 184 Read FplusOneCount Write SetplusOneCount;
    Property relationshipStatus : string Index 192 Read FrelationshipStatus Write SetrelationshipStatus;
    Property skills : string Index 200 Read Fskills Write Setskills;
    Property tagline : string Index 208 Read Ftagline Write Settagline;
    Property url : string Index 216 Read Furl Write Seturl;
    Property urls : TPersonurls Index 224 Read Furls Write Seturls;
    Property verified : boolean Index 232 Read Fverified Write Setverified;
  end;
  TPersonClass = Class of TPerson;
  
  { --------------------------------------------------------------------
    TPersonageRange
    --------------------------------------------------------------------}
  
  TPersonageRange = Class(TGoogleBaseObject)
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
  TPersonageRangeClass = Class of TPersonageRange;
  
  { --------------------------------------------------------------------
    TPersoncover
    --------------------------------------------------------------------}
  
  TPersoncover = Class(TGoogleBaseObject)
  Private
    FcoverInfo : TPersoncovercoverInfo;
    FcoverPhoto : TPersoncovercoverPhoto;
    Flayout : string;
  Protected
    //Property setters
    Procedure SetcoverInfo(AIndex : Integer; AValue : TPersoncovercoverInfo); virtual;
    Procedure SetcoverPhoto(AIndex : Integer; AValue : TPersoncovercoverPhoto); virtual;
    Procedure Setlayout(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property coverInfo : TPersoncovercoverInfo Index 0 Read FcoverInfo Write SetcoverInfo;
    Property coverPhoto : TPersoncovercoverPhoto Index 8 Read FcoverPhoto Write SetcoverPhoto;
    Property layout : string Index 16 Read Flayout Write Setlayout;
  end;
  TPersoncoverClass = Class of TPersoncover;
  
  { --------------------------------------------------------------------
    TPersoncovercoverInfo
    --------------------------------------------------------------------}
  
  TPersoncovercoverInfo = Class(TGoogleBaseObject)
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
  TPersoncovercoverInfoClass = Class of TPersoncovercoverInfo;
  
  { --------------------------------------------------------------------
    TPersoncovercoverPhoto
    --------------------------------------------------------------------}
  
  TPersoncovercoverPhoto = Class(TGoogleBaseObject)
  Private
    Fheight : integer;
    Furl : string;
    Fwidth : integer;
  Protected
    //Property setters
    Procedure Setheight(AIndex : Integer; AValue : integer); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property height : integer Index 0 Read Fheight Write Setheight;
    Property url : string Index 8 Read Furl Write Seturl;
    Property width : integer Index 16 Read Fwidth Write Setwidth;
  end;
  TPersoncovercoverPhotoClass = Class of TPersoncovercoverPhoto;
  
  { --------------------------------------------------------------------
    TPersonemails
    --------------------------------------------------------------------}
  
  TPersonemails = Class(TGoogleBaseObject)
  Private
    F_type : string;
    Fvalue : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property _type : string Index 0 Read F_type Write Set_type;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TPersonemailsClass = Class of TPersonemails;
  
  { --------------------------------------------------------------------
    TPersonimage
    --------------------------------------------------------------------}
  
  TPersonimage = Class(TGoogleBaseObject)
  Private
    FisDefault : boolean;
    Furl : string;
  Protected
    //Property setters
    Procedure SetisDefault(AIndex : Integer; AValue : boolean); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property isDefault : boolean Index 0 Read FisDefault Write SetisDefault;
    Property url : string Index 8 Read Furl Write Seturl;
  end;
  TPersonimageClass = Class of TPersonimage;
  
  { --------------------------------------------------------------------
    TPersonname
    --------------------------------------------------------------------}
  
  TPersonname = Class(TGoogleBaseObject)
  Private
    FfamilyName : string;
    Fformatted : string;
    FgivenName : string;
    FhonorificPrefix : string;
    FhonorificSuffix : string;
    FmiddleName : string;
  Protected
    //Property setters
    Procedure SetfamilyName(AIndex : Integer; AValue : string); virtual;
    Procedure Setformatted(AIndex : Integer; AValue : string); virtual;
    Procedure SetgivenName(AIndex : Integer; AValue : string); virtual;
    Procedure SethonorificPrefix(AIndex : Integer; AValue : string); virtual;
    Procedure SethonorificSuffix(AIndex : Integer; AValue : string); virtual;
    Procedure SetmiddleName(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property familyName : string Index 0 Read FfamilyName Write SetfamilyName;
    Property formatted : string Index 8 Read Fformatted Write Setformatted;
    Property givenName : string Index 16 Read FgivenName Write SetgivenName;
    Property honorificPrefix : string Index 24 Read FhonorificPrefix Write SethonorificPrefix;
    Property honorificSuffix : string Index 32 Read FhonorificSuffix Write SethonorificSuffix;
    Property middleName : string Index 40 Read FmiddleName Write SetmiddleName;
  end;
  TPersonnameClass = Class of TPersonname;
  
  { --------------------------------------------------------------------
    TPersonorganizations
    --------------------------------------------------------------------}
  
  TPersonorganizations = Class(TGoogleBaseObject)
  Private
    Fdepartment : string;
    Fdescription : string;
    FendDate : string;
    Flocation : string;
    Fname : string;
    Fprimary : boolean;
    FstartDate : string;
    Ftitle : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setdepartment(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetendDate(AIndex : Integer; AValue : string); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setprimary(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetstartDate(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property department : string Index 0 Read Fdepartment Write Setdepartment;
    Property description : string Index 8 Read Fdescription Write Setdescription;
    Property endDate : string Index 16 Read FendDate Write SetendDate;
    Property location : string Index 24 Read Flocation Write Setlocation;
    Property name : string Index 32 Read Fname Write Setname;
    Property primary : boolean Index 40 Read Fprimary Write Setprimary;
    Property startDate : string Index 48 Read FstartDate Write SetstartDate;
    Property title : string Index 56 Read Ftitle Write Settitle;
    Property _type : string Index 64 Read F_type Write Set_type;
  end;
  TPersonorganizationsClass = Class of TPersonorganizations;
  
  { --------------------------------------------------------------------
    TPersonplacesLived
    --------------------------------------------------------------------}
  
  TPersonplacesLived = Class(TGoogleBaseObject)
  Private
    Fprimary : boolean;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setprimary(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property primary : boolean Index 0 Read Fprimary Write Setprimary;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TPersonplacesLivedClass = Class of TPersonplacesLived;
  
  { --------------------------------------------------------------------
    TPersonurls
    --------------------------------------------------------------------}
  
  TPersonurls = Class(TGoogleBaseObject)
  Private
    F_label : string;
    F_type : string;
    Fvalue : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_label(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property _label : string Index 0 Read F_label Write Set_label;
    Property _type : string Index 8 Read F_type Write Set_type;
    Property value : string Index 16 Read Fvalue Write Setvalue;
  end;
  TPersonurlsClass = Class of TPersonurls;
  
  { --------------------------------------------------------------------
    TPlace
    --------------------------------------------------------------------}
  
  TPlace = Class(TGoogleBaseObject)
  Private
    Faddress : TPlaceaddress;
    FdisplayName : string;
    Fid : string;
    Fkind : string;
    Fposition : TPlaceposition;
  Protected
    //Property setters
    Procedure Setaddress(AIndex : Integer; AValue : TPlaceaddress); virtual;
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setposition(AIndex : Integer; AValue : TPlaceposition); virtual;
  Public
  Published
    Property address : TPlaceaddress Index 0 Read Faddress Write Setaddress;
    Property displayName : string Index 8 Read FdisplayName Write SetdisplayName;
    Property id : string Index 16 Read Fid Write Setid;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property position : TPlaceposition Index 32 Read Fposition Write Setposition;
  end;
  TPlaceClass = Class of TPlace;
  
  { --------------------------------------------------------------------
    TPlaceaddress
    --------------------------------------------------------------------}
  
  TPlaceaddress = Class(TGoogleBaseObject)
  Private
    Fformatted : string;
  Protected
    //Property setters
    Procedure Setformatted(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property formatted : string Index 0 Read Fformatted Write Setformatted;
  end;
  TPlaceaddressClass = Class of TPlaceaddress;
  
  { --------------------------------------------------------------------
    TPlaceposition
    --------------------------------------------------------------------}
  
  TPlaceposition = Class(TGoogleBaseObject)
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
  TPlacepositionClass = Class of TPlaceposition;
  
  { --------------------------------------------------------------------
    TPlusAclentryResource
    --------------------------------------------------------------------}
  
  TPlusAclentryResource = Class(TGoogleBaseObject)
  Private
    FdisplayName : string;
    Fid : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property displayName : string Index 0 Read FdisplayName Write SetdisplayName;
    Property id : string Index 8 Read Fid Write Setid;
    Property _type : string Index 16 Read F_type Write Set_type;
  end;
  TPlusAclentryResourceClass = Class of TPlusAclentryResource;
  
  { --------------------------------------------------------------------
    TActivitiesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TActivitiesResource, method List
  
  TActivitiesListOptions = Record
    maxResults : integer;
    pageToken : string;
  end;
  
  
  //Optional query Options for TActivitiesResource, method Search
  
  TActivitiesSearchOptions = Record
    language : string;
    maxResults : integer;
    orderBy : string;
    pageToken : string;
    query : string;
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
    pageToken : string;
    sortOrder : string;
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
    pageToken : string;
    targetUrl : string;
    _type : string;
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
    orderBy : string;
    pageToken : string;
  end;
  
  
  //Optional query Options for TPeopleResource, method ListByActivity
  
  TPeopleListByActivityOptions = Record
    maxResults : integer;
    pageToken : string;
  end;
  
  
  //Optional query Options for TPeopleResource, method Search
  
  TPeopleSearchOptions = Record
    language : string;
    maxResults : integer;
    pageToken : string;
    query : string;
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


Procedure TAcl.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAcl.Setitems(AIndex : Integer; AValue : TAclitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAcl.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAclitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TActivity
  --------------------------------------------------------------------}


Procedure TActivity.Setaccess(AIndex : Integer; AValue : TAcl); 

begin
  If (Faccess=AValue) then exit;
  Faccess:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setactor(AIndex : Integer; AValue : TActivityactor); 

begin
  If (Factor=AValue) then exit;
  Factor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setaddress(AIndex : Integer; AValue : string); 

begin
  If (Faddress=AValue) then exit;
  Faddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setannotation(AIndex : Integer; AValue : string); 

begin
  If (Fannotation=AValue) then exit;
  Fannotation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.SetcrosspostSource(AIndex : Integer; AValue : string); 

begin
  If (FcrosspostSource=AValue) then exit;
  FcrosspostSource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setgeocode(AIndex : Integer; AValue : string); 

begin
  If (Fgeocode=AValue) then exit;
  Fgeocode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setkind(AIndex : Integer; AValue : string); 

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



Procedure TActivity.Set_object(AIndex : Integer; AValue : TActivityobject); 

begin
  If (F_object=AValue) then exit;
  F_object:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.SetplaceId(AIndex : Integer; AValue : string); 

begin
  If (FplaceId=AValue) then exit;
  FplaceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.SetplaceName(AIndex : Integer; AValue : string); 

begin
  If (FplaceName=AValue) then exit;
  FplaceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setprovider(AIndex : Integer; AValue : TActivityprovider); 

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



Procedure TActivity.Setradius(AIndex : Integer; AValue : string); 

begin
  If (Fradius=AValue) then exit;
  Fradius:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Settitle(AIndex : Integer; AValue : string); 

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



Procedure TActivity.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setverb(AIndex : Integer; AValue : string); 

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
  TActivityactor
  --------------------------------------------------------------------}


Procedure TActivityactor.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityactor.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityactor.Setimage(AIndex : Integer; AValue : TActivityactorimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityactor.Setname(AIndex : Integer; AValue : TActivityactorname); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityactor.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityactorimage
  --------------------------------------------------------------------}


Procedure TActivityactorimage.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityactorname
  --------------------------------------------------------------------}


Procedure TActivityactorname.SetfamilyName(AIndex : Integer; AValue : string); 

begin
  If (FfamilyName=AValue) then exit;
  FfamilyName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityactorname.SetgivenName(AIndex : Integer; AValue : string); 

begin
  If (FgivenName=AValue) then exit;
  FgivenName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityobject
  --------------------------------------------------------------------}


Procedure TActivityobject.Setactor(AIndex : Integer; AValue : TActivityobjectactor); 

begin
  If (Factor=AValue) then exit;
  Factor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobject.Setattachments(AIndex : Integer; AValue : TActivityobjectattachments); 

begin
  If (Fattachments=AValue) then exit;
  Fattachments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobject.Setcontent(AIndex : Integer; AValue : string); 

begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobject.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobject.SetobjectType(AIndex : Integer; AValue : string); 

begin
  If (FobjectType=AValue) then exit;
  FobjectType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobject.SetoriginalContent(AIndex : Integer; AValue : string); 

begin
  If (ForiginalContent=AValue) then exit;
  ForiginalContent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobject.Setplusoners(AIndex : Integer; AValue : TActivityobjectplusoners); 

begin
  If (Fplusoners=AValue) then exit;
  Fplusoners:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobject.Setreplies(AIndex : Integer; AValue : TActivityobjectreplies); 

begin
  If (Freplies=AValue) then exit;
  Freplies:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobject.Setresharers(AIndex : Integer; AValue : TActivityobjectresharers); 

begin
  If (Fresharers=AValue) then exit;
  Fresharers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobject.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityobjectactor
  --------------------------------------------------------------------}


Procedure TActivityobjectactor.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectactor.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectactor.Setimage(AIndex : Integer; AValue : TActivityobjectactorimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectactor.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityobjectactorimage
  --------------------------------------------------------------------}


Procedure TActivityobjectactorimage.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityobjectattachments
  --------------------------------------------------------------------}


Procedure TActivityobjectattachments.Setcontent(AIndex : Integer; AValue : string); 

begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectattachments.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectattachments.Setembed(AIndex : Integer; AValue : TActivityobjectattachmentsembed); 

begin
  If (Fembed=AValue) then exit;
  Fembed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectattachments.SetfullImage(AIndex : Integer; AValue : TActivityobjectattachmentsfullImage); 

begin
  If (FfullImage=AValue) then exit;
  FfullImage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectattachments.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectattachments.Setimage(AIndex : Integer; AValue : TActivityobjectattachmentsimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectattachments.SetobjectType(AIndex : Integer; AValue : string); 

begin
  If (FobjectType=AValue) then exit;
  FobjectType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectattachments.Setthumbnails(AIndex : Integer; AValue : TActivityobjectattachmentsthumbnails); 

begin
  If (Fthumbnails=AValue) then exit;
  Fthumbnails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectattachments.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityobjectattachmentsembed
  --------------------------------------------------------------------}


Procedure TActivityobjectattachmentsembed.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectattachmentsembed.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TActivityobjectattachmentsembed.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TActivityobjectattachmentsfullImage
  --------------------------------------------------------------------}


Procedure TActivityobjectattachmentsfullImage.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectattachmentsfullImage.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectattachmentsfullImage.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectattachmentsfullImage.Setwidth(AIndex : Integer; AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TActivityobjectattachmentsfullImage.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TActivityobjectattachmentsimage
  --------------------------------------------------------------------}


Procedure TActivityobjectattachmentsimage.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectattachmentsimage.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectattachmentsimage.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectattachmentsimage.Setwidth(AIndex : Integer; AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TActivityobjectattachmentsimage.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TActivityobjectattachmentsthumbnails
  --------------------------------------------------------------------}


Procedure TActivityobjectattachmentsthumbnails.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectattachmentsthumbnails.Setimage(AIndex : Integer; AValue : TActivityobjectattachmentsthumbnailsimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectattachmentsthumbnails.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityobjectattachmentsthumbnailsimage
  --------------------------------------------------------------------}


Procedure TActivityobjectattachmentsthumbnailsimage.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectattachmentsthumbnailsimage.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectattachmentsthumbnailsimage.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectattachmentsthumbnailsimage.Setwidth(AIndex : Integer; AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TActivityobjectattachmentsthumbnailsimage.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TActivityobjectplusoners
  --------------------------------------------------------------------}


Procedure TActivityobjectplusoners.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectplusoners.SettotalItems(AIndex : Integer; AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityobjectreplies
  --------------------------------------------------------------------}


Procedure TActivityobjectreplies.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectreplies.SettotalItems(AIndex : Integer; AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityobjectresharers
  --------------------------------------------------------------------}


Procedure TActivityobjectresharers.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectresharers.SettotalItems(AIndex : Integer; AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityprovider
  --------------------------------------------------------------------}


Procedure TActivityprovider.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityFeed
  --------------------------------------------------------------------}


Procedure TActivityFeed.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityFeed.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityFeed.Setitems(AIndex : Integer; AValue : TActivityFeeditems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityFeed.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityFeed.SetnextLink(AIndex : Integer; AValue : string); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityFeed.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityFeed.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityFeed.Settitle(AIndex : Integer; AValue : string); 

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





{ --------------------------------------------------------------------
  TActivityFeeditems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TComment
  --------------------------------------------------------------------}


Procedure TComment.Setactor(AIndex : Integer; AValue : TCommentactor); 

begin
  If (Factor=AValue) then exit;
  Factor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.SetinReplyTo(AIndex : Integer; AValue : TCommentinReplyTo); 

begin
  If (FinReplyTo=AValue) then exit;
  FinReplyTo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Set_object(AIndex : Integer; AValue : TCommentobject); 

begin
  If (F_object=AValue) then exit;
  F_object:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setplusoners(AIndex : Integer; AValue : TCommentplusoners); 

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



Procedure TComment.SetselfLink(AIndex : Integer; AValue : string); 

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



Procedure TComment.Setverb(AIndex : Integer; AValue : string); 

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




{ --------------------------------------------------------------------
  TCommentactor
  --------------------------------------------------------------------}


Procedure TCommentactor.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentactor.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentactor.Setimage(AIndex : Integer; AValue : TCommentactorimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentactor.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentactorimage
  --------------------------------------------------------------------}


Procedure TCommentactorimage.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentinReplyTo
  --------------------------------------------------------------------}


Procedure TCommentinReplyTo.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentinReplyTo.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentobject
  --------------------------------------------------------------------}


Procedure TCommentobject.Setcontent(AIndex : Integer; AValue : string); 

begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentobject.SetobjectType(AIndex : Integer; AValue : string); 

begin
  If (FobjectType=AValue) then exit;
  FobjectType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentobject.SetoriginalContent(AIndex : Integer; AValue : string); 

begin
  If (ForiginalContent=AValue) then exit;
  ForiginalContent:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentplusoners
  --------------------------------------------------------------------}


Procedure TCommentplusoners.SettotalItems(AIndex : Integer; AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentFeed
  --------------------------------------------------------------------}


Procedure TCommentFeed.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentFeed.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentFeed.Setitems(AIndex : Integer; AValue : TCommentFeeditems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentFeed.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentFeed.SetnextLink(AIndex : Integer; AValue : string); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentFeed.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentFeed.Settitle(AIndex : Integer; AValue : string); 

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





{ --------------------------------------------------------------------
  TCommentFeeditems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TItemScope
  --------------------------------------------------------------------}


Procedure TItemScope.Setabout(AIndex : Integer; AValue : TItemScope); 

begin
  If (Fabout=AValue) then exit;
  Fabout:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetadditionalName(AIndex : Integer; AValue : TItemScopeadditionalName); 

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



Procedure TItemScope.SetaddressCountry(AIndex : Integer; AValue : string); 

begin
  If (FaddressCountry=AValue) then exit;
  FaddressCountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetaddressLocality(AIndex : Integer; AValue : string); 

begin
  If (FaddressLocality=AValue) then exit;
  FaddressLocality:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetaddressRegion(AIndex : Integer; AValue : string); 

begin
  If (FaddressRegion=AValue) then exit;
  FaddressRegion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setassociated_media(AIndex : Integer; AValue : TItemScopeassociated_media); 

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



Procedure TItemScope.Setattendees(AIndex : Integer; AValue : TItemScopeattendees); 

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



Procedure TItemScope.Setauthor(AIndex : Integer; AValue : TItemScopeauthor); 

begin
  If (Fauthor=AValue) then exit;
  Fauthor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetbestRating(AIndex : Integer; AValue : string); 

begin
  If (FbestRating=AValue) then exit;
  FbestRating:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetbirthDate(AIndex : Integer; AValue : string); 

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



Procedure TItemScope.Setcaption(AIndex : Integer; AValue : string); 

begin
  If (Fcaption=AValue) then exit;
  Fcaption:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetcontentSize(AIndex : Integer; AValue : string); 

begin
  If (FcontentSize=AValue) then exit;
  FcontentSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetcontentUrl(AIndex : Integer; AValue : string); 

begin
  If (FcontentUrl=AValue) then exit;
  FcontentUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setcontributor(AIndex : Integer; AValue : TItemScopecontributor); 

begin
  If (Fcontributor=AValue) then exit;
  Fcontributor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetdateCreated(AIndex : Integer; AValue : string); 

begin
  If (FdateCreated=AValue) then exit;
  FdateCreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetdateModified(AIndex : Integer; AValue : string); 

begin
  If (FdateModified=AValue) then exit;
  FdateModified:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetdatePublished(AIndex : Integer; AValue : string); 

begin
  If (FdatePublished=AValue) then exit;
  FdatePublished:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setduration(AIndex : Integer; AValue : string); 

begin
  If (Fduration=AValue) then exit;
  Fduration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetembedUrl(AIndex : Integer; AValue : string); 

begin
  If (FembedUrl=AValue) then exit;
  FembedUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetendDate(AIndex : Integer; AValue : string); 

begin
  If (FendDate=AValue) then exit;
  FendDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetfamilyName(AIndex : Integer; AValue : string); 

begin
  If (FfamilyName=AValue) then exit;
  FfamilyName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setgender(AIndex : Integer; AValue : string); 

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



Procedure TItemScope.SetgivenName(AIndex : Integer; AValue : string); 

begin
  If (FgivenName=AValue) then exit;
  FgivenName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setheight(AIndex : Integer; AValue : string); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setimage(AIndex : Integer; AValue : string); 

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



Procedure TItemScope.Setkind(AIndex : Integer; AValue : string); 

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



Procedure TItemScope.Setname(AIndex : Integer; AValue : string); 

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



Procedure TItemScope.Setperformers(AIndex : Integer; AValue : TItemScopeperformers); 

begin
  If (Fperformers=AValue) then exit;
  Fperformers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetplayerType(AIndex : Integer; AValue : string); 

begin
  If (FplayerType=AValue) then exit;
  FplayerType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetpostOfficeBoxNumber(AIndex : Integer; AValue : string); 

begin
  If (FpostOfficeBoxNumber=AValue) then exit;
  FpostOfficeBoxNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetpostalCode(AIndex : Integer; AValue : string); 

begin
  If (FpostalCode=AValue) then exit;
  FpostalCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetratingValue(AIndex : Integer; AValue : string); 

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



Procedure TItemScope.SetstartDate(AIndex : Integer; AValue : string); 

begin
  If (FstartDate=AValue) then exit;
  FstartDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetstreetAddress(AIndex : Integer; AValue : string); 

begin
  If (FstreetAddress=AValue) then exit;
  FstreetAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Settext(AIndex : Integer; AValue : string); 

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



Procedure TItemScope.SetthumbnailUrl(AIndex : Integer; AValue : string); 

begin
  If (FthumbnailUrl=AValue) then exit;
  FthumbnailUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SettickerSymbol(AIndex : Integer; AValue : string); 

begin
  If (FtickerSymbol=AValue) then exit;
  FtickerSymbol:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.Setwidth(AIndex : Integer; AValue : string); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TItemScope.SetworstRating(AIndex : Integer; AValue : string); 

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




{ --------------------------------------------------------------------
  TItemScopeadditionalName
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TItemScopeassociated_media
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TItemScopeattendees
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TItemScopeauthor
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TItemScopecontributor
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TItemScopeperformers
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMoment
  --------------------------------------------------------------------}


Procedure TMoment.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMoment.Setkind(AIndex : Integer; AValue : string); 

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



Procedure TMoment.Set_type(AIndex : Integer; AValue : string); 

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


Procedure TMomentsFeed.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMomentsFeed.Setitems(AIndex : Integer; AValue : TMomentsFeeditems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMomentsFeed.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMomentsFeed.SetnextLink(AIndex : Integer; AValue : string); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMomentsFeed.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMomentsFeed.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMomentsFeed.Settitle(AIndex : Integer; AValue : string); 

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





{ --------------------------------------------------------------------
  TMomentsFeeditems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPeopleFeed
  --------------------------------------------------------------------}


Procedure TPeopleFeed.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPeopleFeed.Setitems(AIndex : Integer; AValue : TPeopleFeeditems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPeopleFeed.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPeopleFeed.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPeopleFeed.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPeopleFeed.Settitle(AIndex : Integer; AValue : string); 

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





{ --------------------------------------------------------------------
  TPeopleFeeditems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPerson
  --------------------------------------------------------------------}


Procedure TPerson.SetaboutMe(AIndex : Integer; AValue : string); 

begin
  If (FaboutMe=AValue) then exit;
  FaboutMe:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetageRange(AIndex : Integer; AValue : TPersonageRange); 

begin
  If (FageRange=AValue) then exit;
  FageRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setbirthday(AIndex : Integer; AValue : string); 

begin
  If (Fbirthday=AValue) then exit;
  Fbirthday:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetbraggingRights(AIndex : Integer; AValue : string); 

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



Procedure TPerson.Setcover(AIndex : Integer; AValue : TPersoncover); 

begin
  If (Fcover=AValue) then exit;
  Fcover:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetcurrentLocation(AIndex : Integer; AValue : string); 

begin
  If (FcurrentLocation=AValue) then exit;
  FcurrentLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setdomain(AIndex : Integer; AValue : string); 

begin
  If (Fdomain=AValue) then exit;
  Fdomain:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setemails(AIndex : Integer; AValue : TPersonemails); 

begin
  If (Femails=AValue) then exit;
  Femails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setgender(AIndex : Integer; AValue : string); 

begin
  If (Fgender=AValue) then exit;
  Fgender:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setimage(AIndex : Integer; AValue : TPersonimage); 

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



Procedure TPerson.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setlanguage(AIndex : Integer; AValue : string); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setname(AIndex : Integer; AValue : TPersonname); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setnickname(AIndex : Integer; AValue : string); 

begin
  If (Fnickname=AValue) then exit;
  Fnickname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetobjectType(AIndex : Integer; AValue : string); 

begin
  If (FobjectType=AValue) then exit;
  FobjectType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setoccupation(AIndex : Integer; AValue : string); 

begin
  If (Foccupation=AValue) then exit;
  Foccupation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setorganizations(AIndex : Integer; AValue : TPersonorganizations); 

begin
  If (Forganizations=AValue) then exit;
  Forganizations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetplacesLived(AIndex : Integer; AValue : TPersonplacesLived); 

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



Procedure TPerson.SetrelationshipStatus(AIndex : Integer; AValue : string); 

begin
  If (FrelationshipStatus=AValue) then exit;
  FrelationshipStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setskills(AIndex : Integer; AValue : string); 

begin
  If (Fskills=AValue) then exit;
  Fskills:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Settagline(AIndex : Integer; AValue : string); 

begin
  If (Ftagline=AValue) then exit;
  Ftagline:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Seturls(AIndex : Integer; AValue : TPersonurls); 

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





{ --------------------------------------------------------------------
  TPersonageRange
  --------------------------------------------------------------------}


Procedure TPersonageRange.Setmax(AIndex : Integer; AValue : integer); 

begin
  If (Fmax=AValue) then exit;
  Fmax:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonageRange.Setmin(AIndex : Integer; AValue : integer); 

begin
  If (Fmin=AValue) then exit;
  Fmin:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPersoncover
  --------------------------------------------------------------------}


Procedure TPersoncover.SetcoverInfo(AIndex : Integer; AValue : TPersoncovercoverInfo); 

begin
  If (FcoverInfo=AValue) then exit;
  FcoverInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersoncover.SetcoverPhoto(AIndex : Integer; AValue : TPersoncovercoverPhoto); 

begin
  If (FcoverPhoto=AValue) then exit;
  FcoverPhoto:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersoncover.Setlayout(AIndex : Integer; AValue : string); 

begin
  If (Flayout=AValue) then exit;
  Flayout:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPersoncovercoverInfo
  --------------------------------------------------------------------}


Procedure TPersoncovercoverInfo.SetleftImageOffset(AIndex : Integer; AValue : integer); 

begin
  If (FleftImageOffset=AValue) then exit;
  FleftImageOffset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersoncovercoverInfo.SettopImageOffset(AIndex : Integer; AValue : integer); 

begin
  If (FtopImageOffset=AValue) then exit;
  FtopImageOffset:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPersoncovercoverPhoto
  --------------------------------------------------------------------}


Procedure TPersoncovercoverPhoto.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersoncovercoverPhoto.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersoncovercoverPhoto.Setwidth(AIndex : Integer; AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPersonemails
  --------------------------------------------------------------------}


Procedure TPersonemails.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonemails.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPersonemails.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPersonimage
  --------------------------------------------------------------------}


Procedure TPersonimage.SetisDefault(AIndex : Integer; AValue : boolean); 

begin
  If (FisDefault=AValue) then exit;
  FisDefault:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonimage.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPersonname
  --------------------------------------------------------------------}


Procedure TPersonname.SetfamilyName(AIndex : Integer; AValue : string); 

begin
  If (FfamilyName=AValue) then exit;
  FfamilyName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonname.Setformatted(AIndex : Integer; AValue : string); 

begin
  If (Fformatted=AValue) then exit;
  Fformatted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonname.SetgivenName(AIndex : Integer; AValue : string); 

begin
  If (FgivenName=AValue) then exit;
  FgivenName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonname.SethonorificPrefix(AIndex : Integer; AValue : string); 

begin
  If (FhonorificPrefix=AValue) then exit;
  FhonorificPrefix:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonname.SethonorificSuffix(AIndex : Integer; AValue : string); 

begin
  If (FhonorificSuffix=AValue) then exit;
  FhonorificSuffix:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonname.SetmiddleName(AIndex : Integer; AValue : string); 

begin
  If (FmiddleName=AValue) then exit;
  FmiddleName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPersonorganizations
  --------------------------------------------------------------------}


Procedure TPersonorganizations.Setdepartment(AIndex : Integer; AValue : string); 

begin
  If (Fdepartment=AValue) then exit;
  Fdepartment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonorganizations.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonorganizations.SetendDate(AIndex : Integer; AValue : string); 

begin
  If (FendDate=AValue) then exit;
  FendDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonorganizations.Setlocation(AIndex : Integer; AValue : string); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonorganizations.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonorganizations.Setprimary(AIndex : Integer; AValue : boolean); 

begin
  If (Fprimary=AValue) then exit;
  Fprimary:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonorganizations.SetstartDate(AIndex : Integer; AValue : string); 

begin
  If (FstartDate=AValue) then exit;
  FstartDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonorganizations.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonorganizations.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPersonorganizations.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPersonplacesLived
  --------------------------------------------------------------------}


Procedure TPersonplacesLived.Setprimary(AIndex : Integer; AValue : boolean); 

begin
  If (Fprimary=AValue) then exit;
  Fprimary:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonplacesLived.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPersonurls
  --------------------------------------------------------------------}


Procedure TPersonurls.Set_label(AIndex : Integer; AValue : string); 

begin
  If (F_label=AValue) then exit;
  F_label:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonurls.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonurls.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPersonurls.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_label' : Result:='label';
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPlace
  --------------------------------------------------------------------}


Procedure TPlace.Setaddress(AIndex : Integer; AValue : TPlaceaddress); 

begin
  If (Faddress=AValue) then exit;
  Faddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlace.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlace.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlace.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlace.Setposition(AIndex : Integer; AValue : TPlaceposition); 

begin
  If (Fposition=AValue) then exit;
  Fposition:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlaceaddress
  --------------------------------------------------------------------}


Procedure TPlaceaddress.Setformatted(AIndex : Integer; AValue : string); 

begin
  If (Fformatted=AValue) then exit;
  Fformatted:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlaceposition
  --------------------------------------------------------------------}


Procedure TPlaceposition.Setlatitude(AIndex : Integer; AValue : double); 

begin
  If (Flatitude=AValue) then exit;
  Flatitude:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlaceposition.Setlongitude(AIndex : Integer; AValue : double); 

begin
  If (Flongitude=AValue) then exit;
  Flongitude:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPlusAclentryResource
  --------------------------------------------------------------------}


Procedure TPlusAclentryResource.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlusAclentryResource.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlusAclentryResource.Set_type(AIndex : Integer; AValue : string); 

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
  Result:='20150401';
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
  TAclitems.RegisterObject;
  TActivity.RegisterObject;
  TActivityactor.RegisterObject;
  TActivityactorimage.RegisterObject;
  TActivityactorname.RegisterObject;
  TActivityobject.RegisterObject;
  TActivityobjectactor.RegisterObject;
  TActivityobjectactorimage.RegisterObject;
  TActivityobjectattachments.RegisterObject;
  TActivityobjectattachmentsembed.RegisterObject;
  TActivityobjectattachmentsfullImage.RegisterObject;
  TActivityobjectattachmentsimage.RegisterObject;
  TActivityobjectattachmentsthumbnails.RegisterObject;
  TActivityobjectattachmentsthumbnailsimage.RegisterObject;
  TActivityobjectplusoners.RegisterObject;
  TActivityobjectreplies.RegisterObject;
  TActivityobjectresharers.RegisterObject;
  TActivityprovider.RegisterObject;
  TActivityFeed.RegisterObject;
  TActivityFeeditems.RegisterObject;
  TComment.RegisterObject;
  TCommentactor.RegisterObject;
  TCommentactorimage.RegisterObject;
  TCommentinReplyTo.RegisterObject;
  TCommentobject.RegisterObject;
  TCommentplusoners.RegisterObject;
  TCommentFeed.RegisterObject;
  TCommentFeeditems.RegisterObject;
  TItemScope.RegisterObject;
  TItemScopeadditionalName.RegisterObject;
  TItemScopeassociated_media.RegisterObject;
  TItemScopeattendees.RegisterObject;
  TItemScopeauthor.RegisterObject;
  TItemScopecontributor.RegisterObject;
  TItemScopeperformers.RegisterObject;
  TMoment.RegisterObject;
  TMomentsFeed.RegisterObject;
  TMomentsFeeditems.RegisterObject;
  TPeopleFeed.RegisterObject;
  TPeopleFeeditems.RegisterObject;
  TPerson.RegisterObject;
  TPersonageRange.RegisterObject;
  TPersoncover.RegisterObject;
  TPersoncovercoverInfo.RegisterObject;
  TPersoncovercoverPhoto.RegisterObject;
  TPersonemails.RegisterObject;
  TPersonimage.RegisterObject;
  TPersonname.RegisterObject;
  TPersonorganizations.RegisterObject;
  TPersonplacesLived.RegisterObject;
  TPersonurls.RegisterObject;
  TPlace.RegisterObject;
  TPlaceaddress.RegisterObject;
  TPlaceposition.RegisterObject;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
end;



initialization
  TPlusAPI.RegisterAPI;
end.
