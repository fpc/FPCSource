unit googleplusDomains;
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
  TActivityobjectattachmentspreviewThumbnails = class;
  TActivityobjectattachmentspreviewThumbnailsArray = Array of TActivityobjectattachmentspreviewThumbnails;
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
  TActivityobjectstatusForViewer = class;
  TActivityobjectstatusForViewerArray = Array of TActivityobjectstatusForViewer;
  TActivityprovider = class;
  TActivityproviderArray = Array of TActivityprovider;
  TActivityFeed = class;
  TActivityFeedArray = Array of TActivityFeed;
  TActivityFeeditems = class;
  TActivityFeeditemsArray = Array of TActivityFeeditems;
  TAudience = class;
  TAudienceArray = Array of TAudience;
  TAudiencesFeed = class;
  TAudiencesFeedArray = Array of TAudiencesFeed;
  TAudiencesFeeditems = class;
  TAudiencesFeeditemsArray = Array of TAudiencesFeeditems;
  TCircle = class;
  TCircleArray = Array of TCircle;
  TCirclepeople = class;
  TCirclepeopleArray = Array of TCirclepeople;
  TCircleFeed = class;
  TCircleFeedArray = Array of TCircleFeed;
  TCircleFeeditems = class;
  TCircleFeeditemsArray = Array of TCircleFeeditems;
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
  TMedia = class;
  TMediaArray = Array of TMedia;
  TMediaauthor = class;
  TMediaauthorArray = Array of TMediaauthor;
  TMediaauthorimage = class;
  TMediaauthorimageArray = Array of TMediaauthorimage;
  TMediaexif = class;
  TMediaexifArray = Array of TMediaexif;
  TMediastreams = class;
  TMediastreamsArray = Array of TMediastreams;
  TPeopleFeed = class;
  TPeopleFeedArray = Array of TPeopleFeed;
  TPeopleFeeditems = class;
  TPeopleFeeditemsArray = Array of TPeopleFeeditems;
  TPerson = class;
  TPersonArray = Array of TPerson;
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
  TPlusDomainsAclentryResource = class;
  TPlusDomainsAclentryResourceArray = Array of TPlusDomainsAclentryResource;
  TVideostream = class;
  TVideostreamArray = Array of TVideostream;
  
  { --------------------------------------------------------------------
    TAcl
    --------------------------------------------------------------------}
  
  TAcl = Class(TGoogleBaseObject)
  Private
    Fdescription : string;
    FdomainRestricted : boolean;
    Fitems : TAclitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetdomainRestricted(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TAclitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property description : string Index 0 Read Fdescription Write Setdescription;
    Property domainRestricted : boolean Index 8 Read FdomainRestricted Write SetdomainRestricted;
    Property items : TAclitems Index 16 Read Fitems Write Setitems;
    Property kind : string Index 24 Read Fkind Write Setkind;
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
    FstatusForViewer : TActivityobjectstatusForViewer;
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
    Procedure SetstatusForViewer(AIndex : Integer; AValue : TActivityobjectstatusForViewer); virtual;
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
    Property statusForViewer : TActivityobjectstatusForViewer Index 72 Read FstatusForViewer Write SetstatusForViewer;
    Property url : string Index 80 Read Furl Write Seturl;
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
    FpreviewThumbnails : TActivityobjectattachmentspreviewThumbnails;
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
    Procedure SetpreviewThumbnails(AIndex : Integer; AValue : TActivityobjectattachmentspreviewThumbnails); virtual;
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
    Property previewThumbnails : TActivityobjectattachmentspreviewThumbnails Index 56 Read FpreviewThumbnails Write SetpreviewThumbnails;
    Property thumbnails : TActivityobjectattachmentsthumbnails Index 64 Read Fthumbnails Write Setthumbnails;
    Property url : string Index 72 Read Furl Write Seturl;
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
    TActivityobjectattachmentspreviewThumbnails
    --------------------------------------------------------------------}
  
  TActivityobjectattachmentspreviewThumbnails = Class(TGoogleBaseObject)
  Private
    Furl : string;
  Protected
    //Property setters
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property url : string Index 0 Read Furl Write Seturl;
  end;
  TActivityobjectattachmentspreviewThumbnailsClass = Class of TActivityobjectattachmentspreviewThumbnails;
  
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
    TActivityobjectstatusForViewer
    --------------------------------------------------------------------}
  
  TActivityobjectstatusForViewer = Class(TGoogleBaseObject)
  Private
    FcanComment : boolean;
    FcanPlusone : boolean;
    FcanUpdate : boolean;
    FisPlusOned : boolean;
    FresharingDisabled : boolean;
  Protected
    //Property setters
    Procedure SetcanComment(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetcanPlusone(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetcanUpdate(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetisPlusOned(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetresharingDisabled(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property canComment : boolean Index 0 Read FcanComment Write SetcanComment;
    Property canPlusone : boolean Index 8 Read FcanPlusone Write SetcanPlusone;
    Property canUpdate : boolean Index 16 Read FcanUpdate Write SetcanUpdate;
    Property isPlusOned : boolean Index 24 Read FisPlusOned Write SetisPlusOned;
    Property resharingDisabled : boolean Index 32 Read FresharingDisabled Write SetresharingDisabled;
  end;
  TActivityobjectstatusForViewerClass = Class of TActivityobjectstatusForViewer;
  
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
    TAudience
    --------------------------------------------------------------------}
  
  TAudience = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitem : TPlusDomainsAclentryResource;
    Fkind : string;
    FmemberCount : integer;
    Fvisibility : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitem(AIndex : Integer; AValue : TPlusDomainsAclentryResource); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetmemberCount(AIndex : Integer; AValue : integer); virtual;
    Procedure Setvisibility(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property item : TPlusDomainsAclentryResource Index 8 Read Fitem Write Setitem;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property memberCount : integer Index 24 Read FmemberCount Write SetmemberCount;
    Property visibility : string Index 32 Read Fvisibility Write Setvisibility;
  end;
  TAudienceClass = Class of TAudience;
  
  { --------------------------------------------------------------------
    TAudiencesFeed
    --------------------------------------------------------------------}
  
  TAudiencesFeed = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TAudiencesFeeditems;
    Fkind : string;
    FnextPageToken : string;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TAudiencesFeeditems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TAudiencesFeeditems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property totalItems : integer Index 32 Read FtotalItems Write SettotalItems;
  end;
  TAudiencesFeedClass = Class of TAudiencesFeed;
  
  { --------------------------------------------------------------------
    TAudiencesFeeditems
    --------------------------------------------------------------------}
  
  TAudiencesFeeditems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAudiencesFeeditemsClass = Class of TAudiencesFeeditems;
  
  { --------------------------------------------------------------------
    TCircle
    --------------------------------------------------------------------}
  
  TCircle = Class(TGoogleBaseObject)
  Private
    Fdescription : string;
    FdisplayName : string;
    Fetag : string;
    Fid : string;
    Fkind : string;
    Fpeople : TCirclepeople;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setpeople(AIndex : Integer; AValue : TCirclepeople); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property description : string Index 0 Read Fdescription Write Setdescription;
    Property displayName : string Index 8 Read FdisplayName Write SetdisplayName;
    Property etag : string Index 16 Read Fetag Write Setetag;
    Property id : string Index 24 Read Fid Write Setid;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property people : TCirclepeople Index 40 Read Fpeople Write Setpeople;
    Property selfLink : string Index 48 Read FselfLink Write SetselfLink;
  end;
  TCircleClass = Class of TCircle;
  
  { --------------------------------------------------------------------
    TCirclepeople
    --------------------------------------------------------------------}
  
  TCirclepeople = Class(TGoogleBaseObject)
  Private
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property totalItems : integer Index 0 Read FtotalItems Write SettotalItems;
  end;
  TCirclepeopleClass = Class of TCirclepeople;
  
  { --------------------------------------------------------------------
    TCircleFeed
    --------------------------------------------------------------------}
  
  TCircleFeed = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TCircleFeeditems;
    Fkind : string;
    FnextLink : string;
    FnextPageToken : string;
    FselfLink : string;
    Ftitle : string;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TCircleFeeditems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TCircleFeeditems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextLink : string Index 24 Read FnextLink Write SetnextLink;
    Property nextPageToken : string Index 32 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 40 Read FselfLink Write SetselfLink;
    Property title : string Index 48 Read Ftitle Write Settitle;
    Property totalItems : integer Index 56 Read FtotalItems Write SettotalItems;
  end;
  TCircleFeedClass = Class of TCircleFeed;
  
  { --------------------------------------------------------------------
    TCircleFeeditems
    --------------------------------------------------------------------}
  
  TCircleFeeditems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCircleFeeditemsClass = Class of TCircleFeeditems;
  
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
    TMedia
    --------------------------------------------------------------------}
  
  TMedia = Class(TGoogleBaseObject)
  Private
    Fauthor : TMediaauthor;
    FdisplayName : string;
    Fetag : string;
    Fexif : TMediaexif;
    Fheight : integer;
    Fid : string;
    Fkind : string;
    FmediaCreatedTime : TDatetime;
    FmediaUrl : string;
    F_published : TDatetime;
    FsizeBytes : string;
    Fstreams : TMediastreams;
    Fsummary : string;
    Fupdated : TDatetime;
    Furl : string;
    FvideoDuration : string;
    FvideoStatus : string;
    Fwidth : integer;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setauthor(AIndex : Integer; AValue : TMediaauthor); virtual;
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setexif(AIndex : Integer; AValue : TMediaexif); virtual;
    Procedure Setheight(AIndex : Integer; AValue : integer); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetmediaCreatedTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetmediaUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Set_published(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetsizeBytes(AIndex : Integer; AValue : string); virtual;
    Procedure Setstreams(AIndex : Integer; AValue : TMediastreams); virtual;
    Procedure Setsummary(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
    Procedure SetvideoDuration(AIndex : Integer; AValue : string); virtual;
    Procedure SetvideoStatus(AIndex : Integer; AValue : string); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property author : TMediaauthor Index 0 Read Fauthor Write Setauthor;
    Property displayName : string Index 8 Read FdisplayName Write SetdisplayName;
    Property etag : string Index 16 Read Fetag Write Setetag;
    Property exif : TMediaexif Index 24 Read Fexif Write Setexif;
    Property height : integer Index 32 Read Fheight Write Setheight;
    Property id : string Index 40 Read Fid Write Setid;
    Property kind : string Index 48 Read Fkind Write Setkind;
    Property mediaCreatedTime : TDatetime Index 56 Read FmediaCreatedTime Write SetmediaCreatedTime;
    Property mediaUrl : string Index 64 Read FmediaUrl Write SetmediaUrl;
    Property _published : TDatetime Index 72 Read F_published Write Set_published;
    Property sizeBytes : string Index 80 Read FsizeBytes Write SetsizeBytes;
    Property streams : TMediastreams Index 88 Read Fstreams Write Setstreams;
    Property summary : string Index 96 Read Fsummary Write Setsummary;
    Property updated : TDatetime Index 104 Read Fupdated Write Setupdated;
    Property url : string Index 112 Read Furl Write Seturl;
    Property videoDuration : string Index 120 Read FvideoDuration Write SetvideoDuration;
    Property videoStatus : string Index 128 Read FvideoStatus Write SetvideoStatus;
    Property width : integer Index 136 Read Fwidth Write Setwidth;
  end;
  TMediaClass = Class of TMedia;
  
  { --------------------------------------------------------------------
    TMediaauthor
    --------------------------------------------------------------------}
  
  TMediaauthor = Class(TGoogleBaseObject)
  Private
    FdisplayName : string;
    Fid : string;
    Fimage : TMediaauthorimage;
    Furl : string;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setimage(AIndex : Integer; AValue : TMediaauthorimage); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property displayName : string Index 0 Read FdisplayName Write SetdisplayName;
    Property id : string Index 8 Read Fid Write Setid;
    Property image : TMediaauthorimage Index 16 Read Fimage Write Setimage;
    Property url : string Index 24 Read Furl Write Seturl;
  end;
  TMediaauthorClass = Class of TMediaauthor;
  
  { --------------------------------------------------------------------
    TMediaauthorimage
    --------------------------------------------------------------------}
  
  TMediaauthorimage = Class(TGoogleBaseObject)
  Private
    Furl : string;
  Protected
    //Property setters
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property url : string Index 0 Read Furl Write Seturl;
  end;
  TMediaauthorimageClass = Class of TMediaauthorimage;
  
  { --------------------------------------------------------------------
    TMediaexif
    --------------------------------------------------------------------}
  
  TMediaexif = Class(TGoogleBaseObject)
  Private
    Ftime : TDatetime;
  Protected
    //Property setters
    Procedure Settime(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property time : TDatetime Index 0 Read Ftime Write Settime;
  end;
  TMediaexifClass = Class of TMediaexif;
  
  { --------------------------------------------------------------------
    TMediastreams
    --------------------------------------------------------------------}
  
  TMediastreams = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMediastreamsClass = Class of TMediastreams;
  
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
    Property birthday : string Index 8 Read Fbirthday Write Setbirthday;
    Property braggingRights : string Index 16 Read FbraggingRights Write SetbraggingRights;
    Property circledByCount : integer Index 24 Read FcircledByCount Write SetcircledByCount;
    Property cover : TPersoncover Index 32 Read Fcover Write Setcover;
    Property currentLocation : string Index 40 Read FcurrentLocation Write SetcurrentLocation;
    Property displayName : string Index 48 Read FdisplayName Write SetdisplayName;
    Property domain : string Index 56 Read Fdomain Write Setdomain;
    Property emails : TPersonemails Index 64 Read Femails Write Setemails;
    Property etag : string Index 72 Read Fetag Write Setetag;
    Property gender : string Index 80 Read Fgender Write Setgender;
    Property id : string Index 88 Read Fid Write Setid;
    Property image : TPersonimage Index 96 Read Fimage Write Setimage;
    Property isPlusUser : boolean Index 104 Read FisPlusUser Write SetisPlusUser;
    Property kind : string Index 112 Read Fkind Write Setkind;
    Property name : TPersonname Index 120 Read Fname Write Setname;
    Property nickname : string Index 128 Read Fnickname Write Setnickname;
    Property objectType : string Index 136 Read FobjectType Write SetobjectType;
    Property occupation : string Index 144 Read Foccupation Write Setoccupation;
    Property organizations : TPersonorganizations Index 152 Read Forganizations Write Setorganizations;
    Property placesLived : TPersonplacesLived Index 160 Read FplacesLived Write SetplacesLived;
    Property plusOneCount : integer Index 168 Read FplusOneCount Write SetplusOneCount;
    Property relationshipStatus : string Index 176 Read FrelationshipStatus Write SetrelationshipStatus;
    Property skills : string Index 184 Read Fskills Write Setskills;
    Property tagline : string Index 192 Read Ftagline Write Settagline;
    Property url : string Index 200 Read Furl Write Seturl;
    Property urls : TPersonurls Index 208 Read Furls Write Seturls;
    Property verified : boolean Index 216 Read Fverified Write Setverified;
  end;
  TPersonClass = Class of TPerson;
  
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
    TPlusDomainsAclentryResource
    --------------------------------------------------------------------}
  
  TPlusDomainsAclentryResource = Class(TGoogleBaseObject)
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
  TPlusDomainsAclentryResourceClass = Class of TPlusDomainsAclentryResource;
  
  { --------------------------------------------------------------------
    TVideostream
    --------------------------------------------------------------------}
  
  TVideostream = Class(TGoogleBaseObject)
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
  TVideostreamClass = Class of TVideostream;
  
  { --------------------------------------------------------------------
    TActivitiesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TActivitiesResource, method Insert
  
  TActivitiesInsertOptions = Record
    preview : boolean;
  end;
  
  
  //Optional query Options for TActivitiesResource, method List
  
  TActivitiesListOptions = Record
    maxResults : integer;
    pageToken : string;
  end;
  
  TActivitiesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(activityId: string) : TActivity;
    Function Insert(userId: string; aActivity : TActivity; AQuery : string  = '') : TActivity;
    Function Insert(userId: string; aActivity : TActivity; AQuery : TActivitiesinsertOptions) : TActivity;
    Function List(collection: string; userId: string; AQuery : string  = '') : TActivityFeed;
    Function List(collection: string; userId: string; AQuery : TActivitieslistOptions) : TActivityFeed;
  end;
  
  
  { --------------------------------------------------------------------
    TAudiencesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAudiencesResource, method List
  
  TAudiencesListOptions = Record
    maxResults : integer;
    pageToken : string;
  end;
  
  TAudiencesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(userId: string; AQuery : string  = '') : TAudiencesFeed;
    Function List(userId: string; AQuery : TAudienceslistOptions) : TAudiencesFeed;
  end;
  
  
  { --------------------------------------------------------------------
    TCirclesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCirclesResource, method AddPeople
  
  TCirclesAddPeopleOptions = Record
    email : string;
    userId : string;
  end;
  
  
  //Optional query Options for TCirclesResource, method List
  
  TCirclesListOptions = Record
    maxResults : integer;
    pageToken : string;
  end;
  
  
  //Optional query Options for TCirclesResource, method RemovePeople
  
  TCirclesRemovePeopleOptions = Record
    email : string;
    userId : string;
  end;
  
  TCirclesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function AddPeople(circleId: string; AQuery : string  = '') : TCircle;
    Function AddPeople(circleId: string; AQuery : TCirclesaddPeopleOptions) : TCircle;
    Function Get(circleId: string) : TCircle;
    Function Insert(userId: string; aCircle : TCircle) : TCircle;
    Function List(userId: string; AQuery : string  = '') : TCircleFeed;
    Function List(userId: string; AQuery : TCircleslistOptions) : TCircleFeed;
    Function Patch(circleId: string; aCircle : TCircle) : TCircle;
    Procedure Remove(circleId: string);
    Procedure RemovePeople(circleId: string; AQuery : string  = '');
    Procedure RemovePeople(circleId: string; AQuery : TCirclesremovePeopleOptions);
    Function Update(circleId: string; aCircle : TCircle) : TCircle;
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
    Function Insert(activityId: string; aComment : TComment) : TComment;
    Function List(activityId: string; AQuery : string  = '') : TCommentFeed;
    Function List(activityId: string; AQuery : TCommentslistOptions) : TCommentFeed;
  end;
  
  
  { --------------------------------------------------------------------
    TMediaResource
    --------------------------------------------------------------------}
  
  TMediaResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Insert(collection: string; userId: string; aMedia : TMedia) : TMedia;
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
  
  
  //Optional query Options for TPeopleResource, method ListByCircle
  
  TPeopleListByCircleOptions = Record
    maxResults : integer;
    pageToken : string;
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
    Function ListByCircle(circleId: string; AQuery : string  = '') : TPeopleFeed;
    Function ListByCircle(circleId: string; AQuery : TPeoplelistByCircleOptions) : TPeopleFeed;
  end;
  
  
  { --------------------------------------------------------------------
    TPlusDomainsAPI
    --------------------------------------------------------------------}
  
  TPlusDomainsAPI = Class(TGoogleAPI)
  Private
    FActivitiesInstance : TActivitiesResource;
    FAudiencesInstance : TAudiencesResource;
    FCirclesInstance : TCirclesResource;
    FCommentsInstance : TCommentsResource;
    FMediaInstance : TMediaResource;
    FPeopleInstance : TPeopleResource;
    Function GetActivitiesInstance : TActivitiesResource;virtual;
    Function GetAudiencesInstance : TAudiencesResource;virtual;
    Function GetCirclesInstance : TCirclesResource;virtual;
    Function GetCommentsInstance : TCommentsResource;virtual;
    Function GetMediaInstance : TMediaResource;virtual;
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
    Function CreateAudiencesResource(AOwner : TComponent) : TAudiencesResource;virtual;overload;
    Function CreateAudiencesResource : TAudiencesResource;virtual;overload;
    Function CreateCirclesResource(AOwner : TComponent) : TCirclesResource;virtual;overload;
    Function CreateCirclesResource : TCirclesResource;virtual;overload;
    Function CreateCommentsResource(AOwner : TComponent) : TCommentsResource;virtual;overload;
    Function CreateCommentsResource : TCommentsResource;virtual;overload;
    Function CreateMediaResource(AOwner : TComponent) : TMediaResource;virtual;overload;
    Function CreateMediaResource : TMediaResource;virtual;overload;
    Function CreatePeopleResource(AOwner : TComponent) : TPeopleResource;virtual;overload;
    Function CreatePeopleResource : TPeopleResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ActivitiesResource : TActivitiesResource Read GetActivitiesInstance;
    Property AudiencesResource : TAudiencesResource Read GetAudiencesInstance;
    Property CirclesResource : TCirclesResource Read GetCirclesInstance;
    Property CommentsResource : TCommentsResource Read GetCommentsInstance;
    Property MediaResource : TMediaResource Read GetMediaInstance;
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



Procedure TAcl.SetdomainRestricted(AIndex : Integer; AValue : boolean); 

begin
  If (FdomainRestricted=AValue) then exit;
  FdomainRestricted:=AValue;
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



Procedure TActivityobject.SetstatusForViewer(AIndex : Integer; AValue : TActivityobjectstatusForViewer); 

begin
  If (FstatusForViewer=AValue) then exit;
  FstatusForViewer:=AValue;
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



Procedure TActivityobjectattachments.SetpreviewThumbnails(AIndex : Integer; AValue : TActivityobjectattachmentspreviewThumbnails); 

begin
  If (FpreviewThumbnails=AValue) then exit;
  FpreviewThumbnails:=AValue;
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
  TActivityobjectattachmentspreviewThumbnails
  --------------------------------------------------------------------}


Procedure TActivityobjectattachmentspreviewThumbnails.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
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
  TActivityobjectstatusForViewer
  --------------------------------------------------------------------}


Procedure TActivityobjectstatusForViewer.SetcanComment(AIndex : Integer; AValue : boolean); 

begin
  If (FcanComment=AValue) then exit;
  FcanComment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectstatusForViewer.SetcanPlusone(AIndex : Integer; AValue : boolean); 

begin
  If (FcanPlusone=AValue) then exit;
  FcanPlusone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectstatusForViewer.SetcanUpdate(AIndex : Integer; AValue : boolean); 

begin
  If (FcanUpdate=AValue) then exit;
  FcanUpdate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectstatusForViewer.SetisPlusOned(AIndex : Integer; AValue : boolean); 

begin
  If (FisPlusOned=AValue) then exit;
  FisPlusOned:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityobjectstatusForViewer.SetresharingDisabled(AIndex : Integer; AValue : boolean); 

begin
  If (FresharingDisabled=AValue) then exit;
  FresharingDisabled:=AValue;
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
  TAudience
  --------------------------------------------------------------------}


Procedure TAudience.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAudience.Setitem(AIndex : Integer; AValue : TPlusDomainsAclentryResource); 

begin
  If (Fitem=AValue) then exit;
  Fitem:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAudience.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAudience.SetmemberCount(AIndex : Integer; AValue : integer); 

begin
  If (FmemberCount=AValue) then exit;
  FmemberCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAudience.Setvisibility(AIndex : Integer; AValue : string); 

begin
  If (Fvisibility=AValue) then exit;
  Fvisibility:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAudiencesFeed
  --------------------------------------------------------------------}


Procedure TAudiencesFeed.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAudiencesFeed.Setitems(AIndex : Integer; AValue : TAudiencesFeeditems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAudiencesFeed.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAudiencesFeed.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAudiencesFeed.SettotalItems(AIndex : Integer; AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAudiencesFeeditems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCircle
  --------------------------------------------------------------------}


Procedure TCircle.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCircle.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCircle.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCircle.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCircle.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCircle.Setpeople(AIndex : Integer; AValue : TCirclepeople); 

begin
  If (Fpeople=AValue) then exit;
  Fpeople:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCircle.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCirclepeople
  --------------------------------------------------------------------}


Procedure TCirclepeople.SettotalItems(AIndex : Integer; AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCircleFeed
  --------------------------------------------------------------------}


Procedure TCircleFeed.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCircleFeed.Setitems(AIndex : Integer; AValue : TCircleFeeditems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCircleFeed.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCircleFeed.SetnextLink(AIndex : Integer; AValue : string); 

begin
  If (FnextLink=AValue) then exit;
  FnextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCircleFeed.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCircleFeed.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCircleFeed.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCircleFeed.SettotalItems(AIndex : Integer; AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCircleFeeditems
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
  TMedia
  --------------------------------------------------------------------}


Procedure TMedia.Setauthor(AIndex : Integer; AValue : TMediaauthor); 

begin
  If (Fauthor=AValue) then exit;
  Fauthor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMedia.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMedia.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMedia.Setexif(AIndex : Integer; AValue : TMediaexif); 

begin
  If (Fexif=AValue) then exit;
  Fexif:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMedia.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMedia.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMedia.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMedia.SetmediaCreatedTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FmediaCreatedTime=AValue) then exit;
  FmediaCreatedTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMedia.SetmediaUrl(AIndex : Integer; AValue : string); 

begin
  If (FmediaUrl=AValue) then exit;
  FmediaUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMedia.Set_published(AIndex : Integer; AValue : TDatetime); 

begin
  If (F_published=AValue) then exit;
  F_published:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMedia.SetsizeBytes(AIndex : Integer; AValue : string); 

begin
  If (FsizeBytes=AValue) then exit;
  FsizeBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMedia.Setstreams(AIndex : Integer; AValue : TMediastreams); 

begin
  If (Fstreams=AValue) then exit;
  Fstreams:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMedia.Setsummary(AIndex : Integer; AValue : string); 

begin
  If (Fsummary=AValue) then exit;
  Fsummary:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMedia.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMedia.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMedia.SetvideoDuration(AIndex : Integer; AValue : string); 

begin
  If (FvideoDuration=AValue) then exit;
  FvideoDuration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMedia.SetvideoStatus(AIndex : Integer; AValue : string); 

begin
  If (FvideoStatus=AValue) then exit;
  FvideoStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMedia.Setwidth(AIndex : Integer; AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TMedia.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_published' : Result:='published';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TMediaauthor
  --------------------------------------------------------------------}


Procedure TMediaauthor.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMediaauthor.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMediaauthor.Setimage(AIndex : Integer; AValue : TMediaauthorimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMediaauthor.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMediaauthorimage
  --------------------------------------------------------------------}


Procedure TMediaauthorimage.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMediaexif
  --------------------------------------------------------------------}


Procedure TMediaexif.Settime(AIndex : Integer; AValue : TDatetime); 

begin
  If (Ftime=AValue) then exit;
  Ftime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMediastreams
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
  TPlusDomainsAclentryResource
  --------------------------------------------------------------------}


Procedure TPlusDomainsAclentryResource.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlusDomainsAclentryResource.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPlusDomainsAclentryResource.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPlusDomainsAclentryResource.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TVideostream
  --------------------------------------------------------------------}


Procedure TVideostream.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideostream.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideostream.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVideostream.Setwidth(AIndex : Integer; AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TVideostream.ExportPropertyName(Const AName : String) :String;

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
  Result:=TplusDomainsAPI;
end;

Function TActivitiesResource.Get(activityId: string) : TActivity;

Const
  _HTTPMethod = 'GET';
  _Path       = 'activities/{activityId}';
  _Methodid   = 'plusDomains.activities.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['activityId',activityId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TActivity) as TActivity;
end;

Function TActivitiesResource.Insert(userId: string; aActivity : TActivity; AQuery : string = '') : TActivity;

Const
  _HTTPMethod = 'POST';
  _Path       = 'people/{userId}/activities';
  _Methodid   = 'plusDomains.activities.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aActivity,TActivity) as TActivity;
end;


Function TActivitiesResource.Insert(userId: string; aActivity : TActivity; AQuery : TActivitiesinsertOptions) : TActivity;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'preview',AQuery.preview);
  Result:=Insert(userId,aActivity,_Q);
end;

Function TActivitiesResource.List(collection: string; userId: string; AQuery : string = '') : TActivityFeed;

Const
  _HTTPMethod = 'GET';
  _Path       = 'people/{userId}/activities/{collection}';
  _Methodid   = 'plusDomains.activities.list';

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



{ --------------------------------------------------------------------
  TAudiencesResource
  --------------------------------------------------------------------}


Class Function TAudiencesResource.ResourceName : String;

begin
  Result:='audiences';
end;

Class Function TAudiencesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TplusDomainsAPI;
end;

Function TAudiencesResource.List(userId: string; AQuery : string = '') : TAudiencesFeed;

Const
  _HTTPMethod = 'GET';
  _Path       = 'people/{userId}/audiences';
  _Methodid   = 'plusDomains.audiences.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAudiencesFeed) as TAudiencesFeed;
end;


Function TAudiencesResource.List(userId: string; AQuery : TAudienceslistOptions) : TAudiencesFeed;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(userId,_Q);
end;



{ --------------------------------------------------------------------
  TCirclesResource
  --------------------------------------------------------------------}


Class Function TCirclesResource.ResourceName : String;

begin
  Result:='circles';
end;

Class Function TCirclesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TplusDomainsAPI;
end;

Function TCirclesResource.AddPeople(circleId: string; AQuery : string = '') : TCircle;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'circles/{circleId}/people';
  _Methodid   = 'plusDomains.circles.addPeople';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['circleId',circleId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TCircle) as TCircle;
end;


Function TCirclesResource.AddPeople(circleId: string; AQuery : TCirclesaddPeopleOptions) : TCircle;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'email',AQuery.email);
  AddToQuery(_Q,'userId',AQuery.userId);
  Result:=AddPeople(circleId,_Q);
end;

Function TCirclesResource.Get(circleId: string) : TCircle;

Const
  _HTTPMethod = 'GET';
  _Path       = 'circles/{circleId}';
  _Methodid   = 'plusDomains.circles.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['circleId',circleId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCircle) as TCircle;
end;

Function TCirclesResource.Insert(userId: string; aCircle : TCircle) : TCircle;

Const
  _HTTPMethod = 'POST';
  _Path       = 'people/{userId}/circles';
  _Methodid   = 'plusDomains.circles.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCircle,TCircle) as TCircle;
end;

Function TCirclesResource.List(userId: string; AQuery : string = '') : TCircleFeed;

Const
  _HTTPMethod = 'GET';
  _Path       = 'people/{userId}/circles';
  _Methodid   = 'plusDomains.circles.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TCircleFeed) as TCircleFeed;
end;


Function TCirclesResource.List(userId: string; AQuery : TCircleslistOptions) : TCircleFeed;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(userId,_Q);
end;

Function TCirclesResource.Patch(circleId: string; aCircle : TCircle) : TCircle;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'circles/{circleId}';
  _Methodid   = 'plusDomains.circles.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['circleId',circleId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCircle,TCircle) as TCircle;
end;

Procedure TCirclesResource.Remove(circleId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'circles/{circleId}';
  _Methodid   = 'plusDomains.circles.remove';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['circleId',circleId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Procedure TCirclesResource.RemovePeople(circleId: string; AQuery : string = '');

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'circles/{circleId}/people';
  _Methodid   = 'plusDomains.circles.removePeople';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['circleId',circleId]);
  ServiceCall(_HTTPMethod,_P,AQuery,Nil,Nil);
end;


Procedure TCirclesResource.RemovePeople(circleId: string; AQuery : TCirclesremovePeopleOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'email',AQuery.email);
  AddToQuery(_Q,'userId',AQuery.userId);
  RemovePeople(circleId,_Q);
end;

Function TCirclesResource.Update(circleId: string; aCircle : TCircle) : TCircle;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'circles/{circleId}';
  _Methodid   = 'plusDomains.circles.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['circleId',circleId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCircle,TCircle) as TCircle;
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
  Result:=TplusDomainsAPI;
end;

Function TCommentsResource.Get(commentId: string) : TComment;

Const
  _HTTPMethod = 'GET';
  _Path       = 'comments/{commentId}';
  _Methodid   = 'plusDomains.comments.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['commentId',commentId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TComment) as TComment;
end;

Function TCommentsResource.Insert(activityId: string; aComment : TComment) : TComment;

Const
  _HTTPMethod = 'POST';
  _Path       = 'activities/{activityId}/comments';
  _Methodid   = 'plusDomains.comments.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['activityId',activityId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aComment,TComment) as TComment;
end;

Function TCommentsResource.List(activityId: string; AQuery : string = '') : TCommentFeed;

Const
  _HTTPMethod = 'GET';
  _Path       = 'activities/{activityId}/comments';
  _Methodid   = 'plusDomains.comments.list';

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
  TMediaResource
  --------------------------------------------------------------------}


Class Function TMediaResource.ResourceName : String;

begin
  Result:='media';
end;

Class Function TMediaResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TplusDomainsAPI;
end;

Function TMediaResource.Insert(collection: string; userId: string; aMedia : TMedia) : TMedia;

Const
  _HTTPMethod = 'POST';
  _Path       = 'people/{userId}/media/{collection}';
  _Methodid   = 'plusDomains.media.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['collection',collection,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aMedia,TMedia) as TMedia;
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
  Result:=TplusDomainsAPI;
end;

Function TPeopleResource.Get(userId: string) : TPerson;

Const
  _HTTPMethod = 'GET';
  _Path       = 'people/{userId}';
  _Methodid   = 'plusDomains.people.get';

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
  _Methodid   = 'plusDomains.people.list';

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
  _Methodid   = 'plusDomains.people.listByActivity';

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

Function TPeopleResource.ListByCircle(circleId: string; AQuery : string = '') : TPeopleFeed;

Const
  _HTTPMethod = 'GET';
  _Path       = 'circles/{circleId}/people';
  _Methodid   = 'plusDomains.people.listByCircle';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['circleId',circleId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPeopleFeed) as TPeopleFeed;
end;


Function TPeopleResource.ListByCircle(circleId: string; AQuery : TPeoplelistByCircleOptions) : TPeopleFeed;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=ListByCircle(circleId,_Q);
end;



{ --------------------------------------------------------------------
  TPlusDomainsAPI
  --------------------------------------------------------------------}

Class Function TPlusDomainsAPI.APIName : String;

begin
  Result:='plusDomains';
end;

Class Function TPlusDomainsAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TPlusDomainsAPI.APIRevision : String;

begin
  Result:='20150401';
end;

Class Function TPlusDomainsAPI.APIID : String;

begin
  Result:='plusDomains:v1';
end;

Class Function TPlusDomainsAPI.APITitle : String;

begin
  Result:='Google+ Domains API';
end;

Class Function TPlusDomainsAPI.APIDescription : String;

begin
  Result:='The Google+ API enables developers to build on top of the Google+ platform.';
end;

Class Function TPlusDomainsAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TPlusDomainsAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TPlusDomainsAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/gplus-16.png';
end;

Class Function TPlusDomainsAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/gplus-32.png';
end;

Class Function TPlusDomainsAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/+/domains/';
end;

Class Function TPlusDomainsAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TPlusDomainsAPI.APIbasePath : string;

begin
  Result:='/plusDomains/v1/';
end;

Class Function TPlusDomainsAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/plusDomains/v1/';
end;

Class Function TPlusDomainsAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TPlusDomainsAPI.APIservicePath : string;

begin
  Result:='plusDomains/v1/';
end;

Class Function TPlusDomainsAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TPlusDomainsAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,10);
  Result[0].Name:='https://www.googleapis.com/auth/plus.circles.read';
  Result[0].Description:='View your circles and the people and pages in them';
  Result[1].Name:='https://www.googleapis.com/auth/plus.circles.write';
  Result[1].Description:='Manage your circles and add people and pages. People and pages you add to your circles will be notified. Others may see this information publicly. People you add to circles can use Hangouts with you.';
  Result[2].Name:='https://www.googleapis.com/auth/plus.login';
  Result[2].Description:='Know your basic profile info and list of people in your circles.';
  Result[3].Name:='https://www.googleapis.com/auth/plus.me';
  Result[3].Description:='Know who you are on Google';
  Result[4].Name:='https://www.googleapis.com/auth/plus.media.upload';
  Result[4].Description:='Send your photos and videos to Google+';
  Result[5].Name:='https://www.googleapis.com/auth/plus.profiles.read';
  Result[5].Description:='View your own Google+ profile and profiles visible to you';
  Result[6].Name:='https://www.googleapis.com/auth/plus.stream.read';
  Result[6].Description:='View your Google+ posts, comments, and stream';
  Result[7].Name:='https://www.googleapis.com/auth/plus.stream.write';
  Result[7].Description:='Manage your Google+ posts, comments, and stream';
  Result[8].Name:='https://www.googleapis.com/auth/userinfo.email';
  Result[8].Description:='View your email address';
  Result[9].Name:='https://www.googleapis.com/auth/userinfo.profile';
  Result[9].Description:='View your basic profile info';
  
end;

Class Function TPlusDomainsAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TPlusDomainsAPI.RegisterAPIResources;

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
  TActivityobjectattachmentspreviewThumbnails.RegisterObject;
  TActivityobjectattachmentsthumbnails.RegisterObject;
  TActivityobjectattachmentsthumbnailsimage.RegisterObject;
  TActivityobjectplusoners.RegisterObject;
  TActivityobjectreplies.RegisterObject;
  TActivityobjectresharers.RegisterObject;
  TActivityobjectstatusForViewer.RegisterObject;
  TActivityprovider.RegisterObject;
  TActivityFeed.RegisterObject;
  TActivityFeeditems.RegisterObject;
  TAudience.RegisterObject;
  TAudiencesFeed.RegisterObject;
  TAudiencesFeeditems.RegisterObject;
  TCircle.RegisterObject;
  TCirclepeople.RegisterObject;
  TCircleFeed.RegisterObject;
  TCircleFeeditems.RegisterObject;
  TComment.RegisterObject;
  TCommentactor.RegisterObject;
  TCommentactorimage.RegisterObject;
  TCommentinReplyTo.RegisterObject;
  TCommentobject.RegisterObject;
  TCommentplusoners.RegisterObject;
  TCommentFeed.RegisterObject;
  TCommentFeeditems.RegisterObject;
  TMedia.RegisterObject;
  TMediaauthor.RegisterObject;
  TMediaauthorimage.RegisterObject;
  TMediaexif.RegisterObject;
  TMediastreams.RegisterObject;
  TPeopleFeed.RegisterObject;
  TPeopleFeeditems.RegisterObject;
  TPerson.RegisterObject;
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
  TPlusDomainsAclentryResource.RegisterObject;
  TVideostream.RegisterObject;
end;


Function TPlusDomainsAPI.GetActivitiesInstance : TActivitiesResource;

begin
  if (FActivitiesInstance=Nil) then
    FActivitiesInstance:=CreateActivitiesResource;
  Result:=FActivitiesInstance;
end;

Function TPlusDomainsAPI.CreateActivitiesResource : TActivitiesResource;

begin
  Result:=CreateActivitiesResource(Self);
end;


Function TPlusDomainsAPI.CreateActivitiesResource(AOwner : TComponent) : TActivitiesResource;

begin
  Result:=TActivitiesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TPlusDomainsAPI.GetAudiencesInstance : TAudiencesResource;

begin
  if (FAudiencesInstance=Nil) then
    FAudiencesInstance:=CreateAudiencesResource;
  Result:=FAudiencesInstance;
end;

Function TPlusDomainsAPI.CreateAudiencesResource : TAudiencesResource;

begin
  Result:=CreateAudiencesResource(Self);
end;


Function TPlusDomainsAPI.CreateAudiencesResource(AOwner : TComponent) : TAudiencesResource;

begin
  Result:=TAudiencesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TPlusDomainsAPI.GetCirclesInstance : TCirclesResource;

begin
  if (FCirclesInstance=Nil) then
    FCirclesInstance:=CreateCirclesResource;
  Result:=FCirclesInstance;
end;

Function TPlusDomainsAPI.CreateCirclesResource : TCirclesResource;

begin
  Result:=CreateCirclesResource(Self);
end;


Function TPlusDomainsAPI.CreateCirclesResource(AOwner : TComponent) : TCirclesResource;

begin
  Result:=TCirclesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TPlusDomainsAPI.GetCommentsInstance : TCommentsResource;

begin
  if (FCommentsInstance=Nil) then
    FCommentsInstance:=CreateCommentsResource;
  Result:=FCommentsInstance;
end;

Function TPlusDomainsAPI.CreateCommentsResource : TCommentsResource;

begin
  Result:=CreateCommentsResource(Self);
end;


Function TPlusDomainsAPI.CreateCommentsResource(AOwner : TComponent) : TCommentsResource;

begin
  Result:=TCommentsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TPlusDomainsAPI.GetMediaInstance : TMediaResource;

begin
  if (FMediaInstance=Nil) then
    FMediaInstance:=CreateMediaResource;
  Result:=FMediaInstance;
end;

Function TPlusDomainsAPI.CreateMediaResource : TMediaResource;

begin
  Result:=CreateMediaResource(Self);
end;


Function TPlusDomainsAPI.CreateMediaResource(AOwner : TComponent) : TMediaResource;

begin
  Result:=TMediaResource.Create(AOwner);
  Result.API:=Self;
end;



Function TPlusDomainsAPI.GetPeopleInstance : TPeopleResource;

begin
  if (FPeopleInstance=Nil) then
    FPeopleInstance:=CreatePeopleResource;
  Result:=FPeopleInstance;
end;

Function TPlusDomainsAPI.CreatePeopleResource : TPeopleResource;

begin
  Result:=CreatePeopleResource(Self);
end;


Function TPlusDomainsAPI.CreatePeopleResource(AOwner : TComponent) : TPeopleResource;

begin
  Result:=TPeopleResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TPlusDomainsAPI.RegisterAPI;
end.
