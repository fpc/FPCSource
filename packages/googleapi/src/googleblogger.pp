unit googleblogger;
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
  TBlog = class;
  TBlogArray = Array of TBlog;
  TBloglocale = class;
  TBloglocaleArray = Array of TBloglocale;
  TBlogpages = class;
  TBlogpagesArray = Array of TBlogpages;
  TBlogposts = class;
  TBlogpostsArray = Array of TBlogposts;
  TBlogpostsitems = class;
  TBlogpostsitemsArray = Array of TBlogpostsitems;
  TBlogList = class;
  TBlogListArray = Array of TBlogList;
  TBlogListblogUserInfos = class;
  TBlogListblogUserInfosArray = Array of TBlogListblogUserInfos;
  TBlogListitems = class;
  TBlogListitemsArray = Array of TBlogListitems;
  TBlogPerUserInfo = class;
  TBlogPerUserInfoArray = Array of TBlogPerUserInfo;
  TBlogUserInfo = class;
  TBlogUserInfoArray = Array of TBlogUserInfo;
  TComment = class;
  TCommentArray = Array of TComment;
  TCommentauthor = class;
  TCommentauthorArray = Array of TCommentauthor;
  TCommentauthorimage = class;
  TCommentauthorimageArray = Array of TCommentauthorimage;
  TCommentblog = class;
  TCommentblogArray = Array of TCommentblog;
  TCommentinReplyTo = class;
  TCommentinReplyToArray = Array of TCommentinReplyTo;
  TCommentpost = class;
  TCommentpostArray = Array of TCommentpost;
  TCommentList = class;
  TCommentListArray = Array of TCommentList;
  TCommentListitems = class;
  TCommentListitemsArray = Array of TCommentListitems;
  TPage = class;
  TPageArray = Array of TPage;
  TPageauthor = class;
  TPageauthorArray = Array of TPageauthor;
  TPageauthorimage = class;
  TPageauthorimageArray = Array of TPageauthorimage;
  TPageblog = class;
  TPageblogArray = Array of TPageblog;
  TPageList = class;
  TPageListArray = Array of TPageList;
  TPageListitems = class;
  TPageListitemsArray = Array of TPageListitems;
  TPageviews = class;
  TPageviewsArray = Array of TPageviews;
  TPageviewscounts = class;
  TPageviewscountsArray = Array of TPageviewscounts;
  TPost = class;
  TPostArray = Array of TPost;
  TPostauthor = class;
  TPostauthorArray = Array of TPostauthor;
  TPostauthorimage = class;
  TPostauthorimageArray = Array of TPostauthorimage;
  TPostblog = class;
  TPostblogArray = Array of TPostblog;
  TPostimages = class;
  TPostimagesArray = Array of TPostimages;
  TPostlabels = class;
  TPostlabelsArray = Array of TPostlabels;
  TPostlocation = class;
  TPostlocationArray = Array of TPostlocation;
  TPostreplies = class;
  TPostrepliesArray = Array of TPostreplies;
  TPostrepliesitems = class;
  TPostrepliesitemsArray = Array of TPostrepliesitems;
  TPostList = class;
  TPostListArray = Array of TPostList;
  TPostListitems = class;
  TPostListitemsArray = Array of TPostListitems;
  TPostPerUserInfo = class;
  TPostPerUserInfoArray = Array of TPostPerUserInfo;
  TPostUserInfo = class;
  TPostUserInfoArray = Array of TPostUserInfo;
  TPostUserInfosList = class;
  TPostUserInfosListArray = Array of TPostUserInfosList;
  TPostUserInfosListitems = class;
  TPostUserInfosListitemsArray = Array of TPostUserInfosListitems;
  TUser = class;
  TUserArray = Array of TUser;
  TUserblogs = class;
  TUserblogsArray = Array of TUserblogs;
  TUserlocale = class;
  TUserlocaleArray = Array of TUserlocale;
  
  { --------------------------------------------------------------------
    TBlog
    --------------------------------------------------------------------}
  
  TBlog = Class(TGoogleBaseObject)
  Private
    FcustomMetaData : string;
    Fdescription : string;
    Fid : string;
    Fkind : string;
    Flocale : TBloglocale;
    Fname : string;
    Fpages : TBlogpages;
    Fposts : TBlogposts;
    F_published : TDatetime;
    FselfLink : string;
    Fstatus : string;
    Fupdated : TDatetime;
    Furl : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetcustomMetaData(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlocale(AIndex : Integer; AValue : TBloglocale); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setpages(AIndex : Integer; AValue : TBlogpages); virtual;
    Procedure Setposts(AIndex : Integer; AValue : TBlogposts); virtual;
    Procedure Set_published(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property customMetaData : string Index 0 Read FcustomMetaData Write SetcustomMetaData;
    Property description : string Index 8 Read Fdescription Write Setdescription;
    Property id : string Index 16 Read Fid Write Setid;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property locale : TBloglocale Index 32 Read Flocale Write Setlocale;
    Property name : string Index 40 Read Fname Write Setname;
    Property pages : TBlogpages Index 48 Read Fpages Write Setpages;
    Property posts : TBlogposts Index 56 Read Fposts Write Setposts;
    Property _published : TDatetime Index 64 Read F_published Write Set_published;
    Property selfLink : string Index 72 Read FselfLink Write SetselfLink;
    Property status : string Index 80 Read Fstatus Write Setstatus;
    Property updated : TDatetime Index 88 Read Fupdated Write Setupdated;
    Property url : string Index 96 Read Furl Write Seturl;
  end;
  TBlogClass = Class of TBlog;
  
  { --------------------------------------------------------------------
    TBloglocale
    --------------------------------------------------------------------}
  
  TBloglocale = Class(TGoogleBaseObject)
  Private
    Fcountry : string;
    Flanguage : string;
    Fvariant : string;
  Protected
    //Property setters
    Procedure Setcountry(AIndex : Integer; AValue : string); virtual;
    Procedure Setlanguage(AIndex : Integer; AValue : string); virtual;
    Procedure Setvariant(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property country : string Index 0 Read Fcountry Write Setcountry;
    Property language : string Index 8 Read Flanguage Write Setlanguage;
    Property variant : string Index 16 Read Fvariant Write Setvariant;
  end;
  TBloglocaleClass = Class of TBloglocale;
  
  { --------------------------------------------------------------------
    TBlogpages
    --------------------------------------------------------------------}
  
  TBlogpages = Class(TGoogleBaseObject)
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
  TBlogpagesClass = Class of TBlogpages;
  
  { --------------------------------------------------------------------
    TBlogposts
    --------------------------------------------------------------------}
  
  TBlogposts = Class(TGoogleBaseObject)
  Private
    Fitems : TBlogpostsitems;
    FselfLink : string;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TBlogpostsitems); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property items : TBlogpostsitems Index 0 Read Fitems Write Setitems;
    Property selfLink : string Index 8 Read FselfLink Write SetselfLink;
    Property totalItems : integer Index 16 Read FtotalItems Write SettotalItems;
  end;
  TBlogpostsClass = Class of TBlogposts;
  
  { --------------------------------------------------------------------
    TBlogpostsitems
    --------------------------------------------------------------------}
  
  TBlogpostsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TBlogpostsitemsClass = Class of TBlogpostsitems;
  
  { --------------------------------------------------------------------
    TBlogList
    --------------------------------------------------------------------}
  
  TBlogList = Class(TGoogleBaseObject)
  Private
    FblogUserInfos : TBlogListblogUserInfos;
    Fitems : TBlogListitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure SetblogUserInfos(AIndex : Integer; AValue : TBlogListblogUserInfos); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TBlogListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property blogUserInfos : TBlogListblogUserInfos Index 0 Read FblogUserInfos Write SetblogUserInfos;
    Property items : TBlogListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
  end;
  TBlogListClass = Class of TBlogList;
  
  { --------------------------------------------------------------------
    TBlogListblogUserInfos
    --------------------------------------------------------------------}
  
  TBlogListblogUserInfos = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TBlogListblogUserInfosClass = Class of TBlogListblogUserInfos;
  
  { --------------------------------------------------------------------
    TBlogListitems
    --------------------------------------------------------------------}
  
  TBlogListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TBlogListitemsClass = Class of TBlogListitems;
  
  { --------------------------------------------------------------------
    TBlogPerUserInfo
    --------------------------------------------------------------------}
  
  TBlogPerUserInfo = Class(TGoogleBaseObject)
  Private
    FblogId : string;
    FhasAdminAccess : boolean;
    Fkind : string;
    FphotosAlbumKey : string;
    Frole : string;
    FuserId : string;
  Protected
    //Property setters
    Procedure SetblogId(AIndex : Integer; AValue : string); virtual;
    Procedure SethasAdminAccess(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetphotosAlbumKey(AIndex : Integer; AValue : string); virtual;
    Procedure Setrole(AIndex : Integer; AValue : string); virtual;
    Procedure SetuserId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property blogId : string Index 0 Read FblogId Write SetblogId;
    Property hasAdminAccess : boolean Index 8 Read FhasAdminAccess Write SethasAdminAccess;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property photosAlbumKey : string Index 24 Read FphotosAlbumKey Write SetphotosAlbumKey;
    Property role : string Index 32 Read Frole Write Setrole;
    Property userId : string Index 40 Read FuserId Write SetuserId;
  end;
  TBlogPerUserInfoClass = Class of TBlogPerUserInfo;
  
  { --------------------------------------------------------------------
    TBlogUserInfo
    --------------------------------------------------------------------}
  
  TBlogUserInfo = Class(TGoogleBaseObject)
  Private
    Fblog : TCommentblog;
    Fblog_user_info : TBlogPerUserInfo;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setblog(AIndex : Integer; AValue : TCommentblog); virtual;
    Procedure Setblog_user_info(AIndex : Integer; AValue : TBlogPerUserInfo); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property blog : TCommentblog Index 0 Read Fblog Write Setblog;
    Property blog_user_info : TBlogPerUserInfo Index 8 Read Fblog_user_info Write Setblog_user_info;
    Property kind : string Index 16 Read Fkind Write Setkind;
  end;
  TBlogUserInfoClass = Class of TBlogUserInfo;
  
  { --------------------------------------------------------------------
    TComment
    --------------------------------------------------------------------}
  
  TComment = Class(TGoogleBaseObject)
  Private
    Fauthor : TCommentauthor;
    Fblog : TCommentblog;
    Fcontent : string;
    Fid : string;
    FinReplyTo : TCommentinReplyTo;
    Fkind : string;
    Fpost : TCommentpost;
    F_published : TDatetime;
    FselfLink : string;
    Fstatus : string;
    Fupdated : TDatetime;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setauthor(AIndex : Integer; AValue : TCommentauthor); virtual;
    Procedure Setblog(AIndex : Integer; AValue : TCommentblog); virtual;
    Procedure Setcontent(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetinReplyTo(AIndex : Integer; AValue : TCommentinReplyTo); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setpost(AIndex : Integer; AValue : TCommentpost); virtual;
    Procedure Set_published(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property author : TCommentauthor Index 0 Read Fauthor Write Setauthor;
    Property blog : TCommentblog Index 8 Read Fblog Write Setblog;
    Property content : string Index 16 Read Fcontent Write Setcontent;
    Property id : string Index 24 Read Fid Write Setid;
    Property inReplyTo : TCommentinReplyTo Index 32 Read FinReplyTo Write SetinReplyTo;
    Property kind : string Index 40 Read Fkind Write Setkind;
    Property post : TCommentpost Index 48 Read Fpost Write Setpost;
    Property _published : TDatetime Index 56 Read F_published Write Set_published;
    Property selfLink : string Index 64 Read FselfLink Write SetselfLink;
    Property status : string Index 72 Read Fstatus Write Setstatus;
    Property updated : TDatetime Index 80 Read Fupdated Write Setupdated;
  end;
  TCommentClass = Class of TComment;
  
  { --------------------------------------------------------------------
    TCommentauthor
    --------------------------------------------------------------------}
  
  TCommentauthor = Class(TGoogleBaseObject)
  Private
    FdisplayName : string;
    Fid : string;
    Fimage : TCommentauthorimage;
    Furl : string;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setimage(AIndex : Integer; AValue : TCommentauthorimage); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property displayName : string Index 0 Read FdisplayName Write SetdisplayName;
    Property id : string Index 8 Read Fid Write Setid;
    Property image : TCommentauthorimage Index 16 Read Fimage Write Setimage;
    Property url : string Index 24 Read Furl Write Seturl;
  end;
  TCommentauthorClass = Class of TCommentauthor;
  
  { --------------------------------------------------------------------
    TCommentauthorimage
    --------------------------------------------------------------------}
  
  TCommentauthorimage = Class(TGoogleBaseObject)
  Private
    Furl : string;
  Protected
    //Property setters
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property url : string Index 0 Read Furl Write Seturl;
  end;
  TCommentauthorimageClass = Class of TCommentauthorimage;
  
  { --------------------------------------------------------------------
    TCommentblog
    --------------------------------------------------------------------}
  
  TCommentblog = Class(TGoogleBaseObject)
  Private
    Fid : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
  end;
  TCommentblogClass = Class of TCommentblog;
  
  { --------------------------------------------------------------------
    TCommentinReplyTo
    --------------------------------------------------------------------}
  
  TCommentinReplyTo = Class(TGoogleBaseObject)
  Private
    Fid : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
  end;
  TCommentinReplyToClass = Class of TCommentinReplyTo;
  
  { --------------------------------------------------------------------
    TCommentpost
    --------------------------------------------------------------------}
  
  TCommentpost = Class(TGoogleBaseObject)
  Private
    Fid : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
  end;
  TCommentpostClass = Class of TCommentpost;
  
  { --------------------------------------------------------------------
    TCommentList
    --------------------------------------------------------------------}
  
  TCommentList = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TCommentListitems;
    Fkind : string;
    FnextPageToken : string;
    FprevPageToken : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TCommentListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetprevPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TCommentListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property prevPageToken : string Index 32 Read FprevPageToken Write SetprevPageToken;
  end;
  TCommentListClass = Class of TCommentList;
  
  { --------------------------------------------------------------------
    TCommentListitems
    --------------------------------------------------------------------}
  
  TCommentListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCommentListitemsClass = Class of TCommentListitems;
  
  { --------------------------------------------------------------------
    TPage
    --------------------------------------------------------------------}
  
  TPage = Class(TGoogleBaseObject)
  Private
    Fauthor : TPageauthor;
    Fblog : TPageblog;
    Fcontent : string;
    Fetag : string;
    Fid : string;
    Fkind : string;
    F_published : TDatetime;
    FselfLink : string;
    Fstatus : string;
    Ftitle : string;
    Fupdated : TDatetime;
    Furl : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setauthor(AIndex : Integer; AValue : TPageauthor); virtual;
    Procedure Setblog(AIndex : Integer; AValue : TPageblog); virtual;
    Procedure Setcontent(AIndex : Integer; AValue : string); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Set_published(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property author : TPageauthor Index 0 Read Fauthor Write Setauthor;
    Property blog : TPageblog Index 8 Read Fblog Write Setblog;
    Property content : string Index 16 Read Fcontent Write Setcontent;
    Property etag : string Index 24 Read Fetag Write Setetag;
    Property id : string Index 32 Read Fid Write Setid;
    Property kind : string Index 40 Read Fkind Write Setkind;
    Property _published : TDatetime Index 48 Read F_published Write Set_published;
    Property selfLink : string Index 56 Read FselfLink Write SetselfLink;
    Property status : string Index 64 Read Fstatus Write Setstatus;
    Property title : string Index 72 Read Ftitle Write Settitle;
    Property updated : TDatetime Index 80 Read Fupdated Write Setupdated;
    Property url : string Index 88 Read Furl Write Seturl;
  end;
  TPageClass = Class of TPage;
  
  { --------------------------------------------------------------------
    TPageauthor
    --------------------------------------------------------------------}
  
  TPageauthor = Class(TGoogleBaseObject)
  Private
    FdisplayName : string;
    Fid : string;
    Fimage : TPageauthorimage;
    Furl : string;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setimage(AIndex : Integer; AValue : TPageauthorimage); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property displayName : string Index 0 Read FdisplayName Write SetdisplayName;
    Property id : string Index 8 Read Fid Write Setid;
    Property image : TPageauthorimage Index 16 Read Fimage Write Setimage;
    Property url : string Index 24 Read Furl Write Seturl;
  end;
  TPageauthorClass = Class of TPageauthor;
  
  { --------------------------------------------------------------------
    TPageauthorimage
    --------------------------------------------------------------------}
  
  TPageauthorimage = Class(TGoogleBaseObject)
  Private
    Furl : string;
  Protected
    //Property setters
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property url : string Index 0 Read Furl Write Seturl;
  end;
  TPageauthorimageClass = Class of TPageauthorimage;
  
  { --------------------------------------------------------------------
    TPageblog
    --------------------------------------------------------------------}
  
  TPageblog = Class(TGoogleBaseObject)
  Private
    Fid : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
  end;
  TPageblogClass = Class of TPageblog;
  
  { --------------------------------------------------------------------
    TPageList
    --------------------------------------------------------------------}
  
  TPageList = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TPageListitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TPageListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TPageListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TPageListClass = Class of TPageList;
  
  { --------------------------------------------------------------------
    TPageListitems
    --------------------------------------------------------------------}
  
  TPageListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPageListitemsClass = Class of TPageListitems;
  
  { --------------------------------------------------------------------
    TPageviews
    --------------------------------------------------------------------}
  
  TPageviews = Class(TGoogleBaseObject)
  Private
    FblogId : string;
    Fcounts : TPageviewscounts;
    Fkind : string;
  Protected
    //Property setters
    Procedure SetblogId(AIndex : Integer; AValue : string); virtual;
    Procedure Setcounts(AIndex : Integer; AValue : TPageviewscounts); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property blogId : string Index 0 Read FblogId Write SetblogId;
    Property counts : TPageviewscounts Index 8 Read Fcounts Write Setcounts;
    Property kind : string Index 16 Read Fkind Write Setkind;
  end;
  TPageviewsClass = Class of TPageviews;
  
  { --------------------------------------------------------------------
    TPageviewscounts
    --------------------------------------------------------------------}
  
  TPageviewscounts = Class(TGoogleBaseObject)
  Private
    Fcount : string;
    FtimeRange : string;
  Protected
    //Property setters
    Procedure Setcount(AIndex : Integer; AValue : string); virtual;
    Procedure SettimeRange(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property count : string Index 0 Read Fcount Write Setcount;
    Property timeRange : string Index 8 Read FtimeRange Write SettimeRange;
  end;
  TPageviewscountsClass = Class of TPageviewscounts;
  
  { --------------------------------------------------------------------
    TPost
    --------------------------------------------------------------------}
  
  TPost = Class(TGoogleBaseObject)
  Private
    Fauthor : TPostauthor;
    Fblog : TPostblog;
    Fcontent : string;
    FcustomMetaData : string;
    Fetag : string;
    Fid : string;
    Fimages : TPostimages;
    Fkind : string;
    Flabels : TPostlabels;
    Flocation : TPostlocation;
    F_published : TDatetime;
    FreaderComments : string;
    Freplies : TPostreplies;
    FselfLink : string;
    Fstatus : string;
    Ftitle : string;
    FtitleLink : string;
    Fupdated : TDatetime;
    Furl : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setauthor(AIndex : Integer; AValue : TPostauthor); virtual;
    Procedure Setblog(AIndex : Integer; AValue : TPostblog); virtual;
    Procedure Setcontent(AIndex : Integer; AValue : string); virtual;
    Procedure SetcustomMetaData(AIndex : Integer; AValue : string); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setimages(AIndex : Integer; AValue : TPostimages); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlabels(AIndex : Integer; AValue : TPostlabels); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : TPostlocation); virtual;
    Procedure Set_published(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetreaderComments(AIndex : Integer; AValue : string); virtual;
    Procedure Setreplies(AIndex : Integer; AValue : TPostreplies); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure SettitleLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property author : TPostauthor Index 0 Read Fauthor Write Setauthor;
    Property blog : TPostblog Index 8 Read Fblog Write Setblog;
    Property content : string Index 16 Read Fcontent Write Setcontent;
    Property customMetaData : string Index 24 Read FcustomMetaData Write SetcustomMetaData;
    Property etag : string Index 32 Read Fetag Write Setetag;
    Property id : string Index 40 Read Fid Write Setid;
    Property images : TPostimages Index 48 Read Fimages Write Setimages;
    Property kind : string Index 56 Read Fkind Write Setkind;
    Property labels : TPostlabels Index 64 Read Flabels Write Setlabels;
    Property location : TPostlocation Index 72 Read Flocation Write Setlocation;
    Property _published : TDatetime Index 80 Read F_published Write Set_published;
    Property readerComments : string Index 88 Read FreaderComments Write SetreaderComments;
    Property replies : TPostreplies Index 96 Read Freplies Write Setreplies;
    Property selfLink : string Index 104 Read FselfLink Write SetselfLink;
    Property status : string Index 112 Read Fstatus Write Setstatus;
    Property title : string Index 120 Read Ftitle Write Settitle;
    Property titleLink : string Index 128 Read FtitleLink Write SettitleLink;
    Property updated : TDatetime Index 136 Read Fupdated Write Setupdated;
    Property url : string Index 144 Read Furl Write Seturl;
  end;
  TPostClass = Class of TPost;
  
  { --------------------------------------------------------------------
    TPostauthor
    --------------------------------------------------------------------}
  
  TPostauthor = Class(TGoogleBaseObject)
  Private
    FdisplayName : string;
    Fid : string;
    Fimage : TPostauthorimage;
    Furl : string;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setimage(AIndex : Integer; AValue : TPostauthorimage); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property displayName : string Index 0 Read FdisplayName Write SetdisplayName;
    Property id : string Index 8 Read Fid Write Setid;
    Property image : TPostauthorimage Index 16 Read Fimage Write Setimage;
    Property url : string Index 24 Read Furl Write Seturl;
  end;
  TPostauthorClass = Class of TPostauthor;
  
  { --------------------------------------------------------------------
    TPostauthorimage
    --------------------------------------------------------------------}
  
  TPostauthorimage = Class(TGoogleBaseObject)
  Private
    Furl : string;
  Protected
    //Property setters
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property url : string Index 0 Read Furl Write Seturl;
  end;
  TPostauthorimageClass = Class of TPostauthorimage;
  
  { --------------------------------------------------------------------
    TPostblog
    --------------------------------------------------------------------}
  
  TPostblog = Class(TGoogleBaseObject)
  Private
    Fid : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
  end;
  TPostblogClass = Class of TPostblog;
  
  { --------------------------------------------------------------------
    TPostimages
    --------------------------------------------------------------------}
  
  TPostimages = Class(TGoogleBaseObject)
  Private
    Furl : string;
  Protected
    //Property setters
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property url : string Index 0 Read Furl Write Seturl;
  end;
  TPostimagesClass = Class of TPostimages;
  
  { --------------------------------------------------------------------
    TPostlabels
    --------------------------------------------------------------------}
  
  TPostlabels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPostlabelsClass = Class of TPostlabels;
  
  { --------------------------------------------------------------------
    TPostlocation
    --------------------------------------------------------------------}
  
  TPostlocation = Class(TGoogleBaseObject)
  Private
    Flat : double;
    Flng : double;
    Fname : string;
    Fspan : string;
  Protected
    //Property setters
    Procedure Setlat(AIndex : Integer; AValue : double); virtual;
    Procedure Setlng(AIndex : Integer; AValue : double); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setspan(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property lat : double Index 0 Read Flat Write Setlat;
    Property lng : double Index 8 Read Flng Write Setlng;
    Property name : string Index 16 Read Fname Write Setname;
    Property span : string Index 24 Read Fspan Write Setspan;
  end;
  TPostlocationClass = Class of TPostlocation;
  
  { --------------------------------------------------------------------
    TPostreplies
    --------------------------------------------------------------------}
  
  TPostreplies = Class(TGoogleBaseObject)
  Private
    Fitems : TPostrepliesitems;
    FselfLink : string;
    FtotalItems : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TPostrepliesitems); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TPostrepliesitems Index 0 Read Fitems Write Setitems;
    Property selfLink : string Index 8 Read FselfLink Write SetselfLink;
    Property totalItems : string Index 16 Read FtotalItems Write SettotalItems;
  end;
  TPostrepliesClass = Class of TPostreplies;
  
  { --------------------------------------------------------------------
    TPostrepliesitems
    --------------------------------------------------------------------}
  
  TPostrepliesitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPostrepliesitemsClass = Class of TPostrepliesitems;
  
  { --------------------------------------------------------------------
    TPostList
    --------------------------------------------------------------------}
  
  TPostList = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TPostListitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TPostListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TPostListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TPostListClass = Class of TPostList;
  
  { --------------------------------------------------------------------
    TPostListitems
    --------------------------------------------------------------------}
  
  TPostListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPostListitemsClass = Class of TPostListitems;
  
  { --------------------------------------------------------------------
    TPostPerUserInfo
    --------------------------------------------------------------------}
  
  TPostPerUserInfo = Class(TGoogleBaseObject)
  Private
    FblogId : string;
    FhasEditAccess : boolean;
    Fkind : string;
    FpostId : string;
    FuserId : string;
  Protected
    //Property setters
    Procedure SetblogId(AIndex : Integer; AValue : string); virtual;
    Procedure SethasEditAccess(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetpostId(AIndex : Integer; AValue : string); virtual;
    Procedure SetuserId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property blogId : string Index 0 Read FblogId Write SetblogId;
    Property hasEditAccess : boolean Index 8 Read FhasEditAccess Write SethasEditAccess;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property postId : string Index 24 Read FpostId Write SetpostId;
    Property userId : string Index 32 Read FuserId Write SetuserId;
  end;
  TPostPerUserInfoClass = Class of TPostPerUserInfo;
  
  { --------------------------------------------------------------------
    TPostUserInfo
    --------------------------------------------------------------------}
  
  TPostUserInfo = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    Fpost : TCommentpost;
    Fpost_user_info : TPostPerUserInfo;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setpost(AIndex : Integer; AValue : TCommentpost); virtual;
    Procedure Setpost_user_info(AIndex : Integer; AValue : TPostPerUserInfo); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property post : TCommentpost Index 8 Read Fpost Write Setpost;
    Property post_user_info : TPostPerUserInfo Index 16 Read Fpost_user_info Write Setpost_user_info;
  end;
  TPostUserInfoClass = Class of TPostUserInfo;
  
  { --------------------------------------------------------------------
    TPostUserInfosList
    --------------------------------------------------------------------}
  
  TPostUserInfosList = Class(TGoogleBaseObject)
  Private
    Fitems : TPostUserInfosListitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TPostUserInfosListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TPostUserInfosListitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TPostUserInfosListClass = Class of TPostUserInfosList;
  
  { --------------------------------------------------------------------
    TPostUserInfosListitems
    --------------------------------------------------------------------}
  
  TPostUserInfosListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPostUserInfosListitemsClass = Class of TPostUserInfosListitems;
  
  { --------------------------------------------------------------------
    TUser
    --------------------------------------------------------------------}
  
  TUser = Class(TGoogleBaseObject)
  Private
    Fabout : string;
    Fblogs : TUserblogs;
    Fcreated : TDatetime;
    FdisplayName : string;
    Fid : string;
    Fkind : string;
    Flocale : TUserlocale;
    FselfLink : string;
    Furl : string;
  Protected
    //Property setters
    Procedure Setabout(AIndex : Integer; AValue : string); virtual;
    Procedure Setblogs(AIndex : Integer; AValue : TUserblogs); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlocale(AIndex : Integer; AValue : TUserlocale); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property about : string Index 0 Read Fabout Write Setabout;
    Property blogs : TUserblogs Index 8 Read Fblogs Write Setblogs;
    Property created : TDatetime Index 16 Read Fcreated Write Setcreated;
    Property displayName : string Index 24 Read FdisplayName Write SetdisplayName;
    Property id : string Index 32 Read Fid Write Setid;
    Property kind : string Index 40 Read Fkind Write Setkind;
    Property locale : TUserlocale Index 48 Read Flocale Write Setlocale;
    Property selfLink : string Index 56 Read FselfLink Write SetselfLink;
    Property url : string Index 64 Read Furl Write Seturl;
  end;
  TUserClass = Class of TUser;
  
  { --------------------------------------------------------------------
    TUserblogs
    --------------------------------------------------------------------}
  
  TUserblogs = Class(TGoogleBaseObject)
  Private
    FselfLink : string;
  Protected
    //Property setters
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property selfLink : string Index 0 Read FselfLink Write SetselfLink;
  end;
  TUserblogsClass = Class of TUserblogs;
  
  { --------------------------------------------------------------------
    TUserlocale
    --------------------------------------------------------------------}
  
  TUserlocale = Class(TGoogleBaseObject)
  Private
    Fcountry : string;
    Flanguage : string;
    Fvariant : string;
  Protected
    //Property setters
    Procedure Setcountry(AIndex : Integer; AValue : string); virtual;
    Procedure Setlanguage(AIndex : Integer; AValue : string); virtual;
    Procedure Setvariant(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property country : string Index 0 Read Fcountry Write Setcountry;
    Property language : string Index 8 Read Flanguage Write Setlanguage;
    Property variant : string Index 16 Read Fvariant Write Setvariant;
  end;
  TUserlocaleClass = Class of TUserlocale;
  
  { --------------------------------------------------------------------
    TBlogUserInfosResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TBlogUserInfosResource, method Get
  
  TBlogUserInfosGetOptions = Record
    maxPosts : integer;
  end;
  
  TBlogUserInfosResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(blogId: string; userId: string; AQuery : string  = '') : TBlogUserInfo;
    Function Get(blogId: string; userId: string; AQuery : TBlogUserInfosgetOptions) : TBlogUserInfo;
  end;
  
  
  { --------------------------------------------------------------------
    TBlogsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TBlogsResource, method Get
  
  TBlogsGetOptions = Record
    maxPosts : integer;
    view : string;
  end;
  
  
  //Optional query Options for TBlogsResource, method GetByUrl
  
  TBlogsGetByUrlOptions = Record
    url : string;
    view : string;
  end;
  
  
  //Optional query Options for TBlogsResource, method ListByUser
  
  TBlogsListByUserOptions = Record
    fetchUserInfo : boolean;
    role : string;
    status : string;
    view : string;
  end;
  
  TBlogsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(blogId: string; AQuery : string  = '') : TBlog;
    Function Get(blogId: string; AQuery : TBlogsgetOptions) : TBlog;
    Function GetByUrl(AQuery : string  = '') : TBlog;
    Function GetByUrl(AQuery : TBlogsgetByUrlOptions) : TBlog;
    Function ListByUser(userId: string; AQuery : string  = '') : TBlogList;
    Function ListByUser(userId: string; AQuery : TBlogslistByUserOptions) : TBlogList;
  end;
  
  
  { --------------------------------------------------------------------
    TCommentsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCommentsResource, method Get
  
  TCommentsGetOptions = Record
    view : string;
  end;
  
  
  //Optional query Options for TCommentsResource, method List
  
  TCommentsListOptions = Record
    endDate : TDatetime;
    fetchBodies : boolean;
    maxResults : integer;
    pageToken : string;
    startDate : TDatetime;
    status : string;
    view : string;
  end;
  
  
  //Optional query Options for TCommentsResource, method ListByBlog
  
  TCommentsListByBlogOptions = Record
    endDate : TDatetime;
    fetchBodies : boolean;
    maxResults : integer;
    pageToken : string;
    startDate : TDatetime;
    status : string;
  end;
  
  TCommentsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Approve(blogId: string; commentId: string; postId: string) : TComment;
    Procedure Delete(blogId: string; commentId: string; postId: string);
    Function Get(blogId: string; commentId: string; postId: string; AQuery : string  = '') : TComment;
    Function Get(blogId: string; commentId: string; postId: string; AQuery : TCommentsgetOptions) : TComment;
    Function List(blogId: string; postId: string; AQuery : string  = '') : TCommentList;
    Function List(blogId: string; postId: string; AQuery : TCommentslistOptions) : TCommentList;
    Function ListByBlog(blogId: string; AQuery : string  = '') : TCommentList;
    Function ListByBlog(blogId: string; AQuery : TCommentslistByBlogOptions) : TCommentList;
    Function MarkAsSpam(blogId: string; commentId: string; postId: string) : TComment;
    Function RemoveContent(blogId: string; commentId: string; postId: string) : TComment;
  end;
  
  
  { --------------------------------------------------------------------
    TPageViewsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TPageViewsResource, method Get
  
  TPageViewsGetOptions = Record
    range : string;
  end;
  
  TPageViewsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(blogId: string; AQuery : string  = '') : TPageviews;
    Function Get(blogId: string; AQuery : TPageViewsgetOptions) : TPageviews;
  end;
  
  
  { --------------------------------------------------------------------
    TPagesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TPagesResource, method Get
  
  TPagesGetOptions = Record
    view : string;
  end;
  
  
  //Optional query Options for TPagesResource, method Insert
  
  TPagesInsertOptions = Record
    isDraft : boolean;
  end;
  
  
  //Optional query Options for TPagesResource, method List
  
  TPagesListOptions = Record
    fetchBodies : boolean;
    maxResults : integer;
    pageToken : string;
    status : string;
    view : string;
  end;
  
  
  //Optional query Options for TPagesResource, method Patch
  
  TPagesPatchOptions = Record
    publish : boolean;
    revert : boolean;
  end;
  
  
  //Optional query Options for TPagesResource, method Update
  
  TPagesUpdateOptions = Record
    publish : boolean;
    revert : boolean;
  end;
  
  TPagesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(blogId: string; pageId: string);
    Function Get(blogId: string; pageId: string; AQuery : string  = '') : TPage;
    Function Get(blogId: string; pageId: string; AQuery : TPagesgetOptions) : TPage;
    Function Insert(blogId: string; aPage : TPage; AQuery : string  = '') : TPage;
    Function Insert(blogId: string; aPage : TPage; AQuery : TPagesinsertOptions) : TPage;
    Function List(blogId: string; AQuery : string  = '') : TPageList;
    Function List(blogId: string; AQuery : TPageslistOptions) : TPageList;
    Function Patch(blogId: string; pageId: string; aPage : TPage; AQuery : string  = '') : TPage;
    Function Patch(blogId: string; pageId: string; aPage : TPage; AQuery : TPagespatchOptions) : TPage;
    Function Publish(blogId: string; pageId: string) : TPage;
    Function Revert(blogId: string; pageId: string) : TPage;
    Function Update(blogId: string; pageId: string; aPage : TPage; AQuery : string  = '') : TPage;
    Function Update(blogId: string; pageId: string; aPage : TPage; AQuery : TPagesupdateOptions) : TPage;
  end;
  
  
  { --------------------------------------------------------------------
    TPostUserInfosResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TPostUserInfosResource, method Get
  
  TPostUserInfosGetOptions = Record
    maxComments : integer;
  end;
  
  
  //Optional query Options for TPostUserInfosResource, method List
  
  TPostUserInfosListOptions = Record
    endDate : TDatetime;
    fetchBodies : boolean;
    labels : string;
    maxResults : integer;
    orderBy : string;
    pageToken : string;
    startDate : TDatetime;
    status : string;
    view : string;
  end;
  
  TPostUserInfosResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(blogId: string; postId: string; userId: string; AQuery : string  = '') : TPostUserInfo;
    Function Get(blogId: string; postId: string; userId: string; AQuery : TPostUserInfosgetOptions) : TPostUserInfo;
    Function List(blogId: string; userId: string; AQuery : string  = '') : TPostUserInfosList;
    Function List(blogId: string; userId: string; AQuery : TPostUserInfoslistOptions) : TPostUserInfosList;
  end;
  
  
  { --------------------------------------------------------------------
    TPostsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TPostsResource, method Get
  
  TPostsGetOptions = Record
    fetchBody : boolean;
    fetchImages : boolean;
    maxComments : integer;
    view : string;
  end;
  
  
  //Optional query Options for TPostsResource, method GetByPath
  
  TPostsGetByPathOptions = Record
    maxComments : integer;
    path : string;
    view : string;
  end;
  
  
  //Optional query Options for TPostsResource, method Insert
  
  TPostsInsertOptions = Record
    fetchBody : boolean;
    fetchImages : boolean;
    isDraft : boolean;
  end;
  
  
  //Optional query Options for TPostsResource, method List
  
  TPostsListOptions = Record
    endDate : TDatetime;
    fetchBodies : boolean;
    fetchImages : boolean;
    labels : string;
    maxResults : integer;
    orderBy : string;
    pageToken : string;
    startDate : TDatetime;
    status : string;
    view : string;
  end;
  
  
  //Optional query Options for TPostsResource, method Patch
  
  TPostsPatchOptions = Record
    fetchBody : boolean;
    fetchImages : boolean;
    maxComments : integer;
    publish : boolean;
    revert : boolean;
  end;
  
  
  //Optional query Options for TPostsResource, method Publish
  
  TPostsPublishOptions = Record
    publishDate : TDatetime;
  end;
  
  
  //Optional query Options for TPostsResource, method Search
  
  TPostsSearchOptions = Record
    fetchBodies : boolean;
    orderBy : string;
    q : string;
  end;
  
  
  //Optional query Options for TPostsResource, method Update
  
  TPostsUpdateOptions = Record
    fetchBody : boolean;
    fetchImages : boolean;
    maxComments : integer;
    publish : boolean;
    revert : boolean;
  end;
  
  TPostsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(blogId: string; postId: string);
    Function Get(blogId: string; postId: string; AQuery : string  = '') : TPost;
    Function Get(blogId: string; postId: string; AQuery : TPostsgetOptions) : TPost;
    Function GetByPath(blogId: string; AQuery : string  = '') : TPost;
    Function GetByPath(blogId: string; AQuery : TPostsgetByPathOptions) : TPost;
    Function Insert(blogId: string; aPost : TPost; AQuery : string  = '') : TPost;
    Function Insert(blogId: string; aPost : TPost; AQuery : TPostsinsertOptions) : TPost;
    Function List(blogId: string; AQuery : string  = '') : TPostList;
    Function List(blogId: string; AQuery : TPostslistOptions) : TPostList;
    Function Patch(blogId: string; postId: string; aPost : TPost; AQuery : string  = '') : TPost;
    Function Patch(blogId: string; postId: string; aPost : TPost; AQuery : TPostspatchOptions) : TPost;
    Function Publish(blogId: string; postId: string; AQuery : string  = '') : TPost;
    Function Publish(blogId: string; postId: string; AQuery : TPostspublishOptions) : TPost;
    Function Revert(blogId: string; postId: string) : TPost;
    Function Search(blogId: string; AQuery : string  = '') : TPostList;
    Function Search(blogId: string; AQuery : TPostssearchOptions) : TPostList;
    Function Update(blogId: string; postId: string; aPost : TPost; AQuery : string  = '') : TPost;
    Function Update(blogId: string; postId: string; aPost : TPost; AQuery : TPostsupdateOptions) : TPost;
  end;
  
  
  { --------------------------------------------------------------------
    TUsersResource
    --------------------------------------------------------------------}
  
  TUsersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(userId: string) : TUser;
  end;
  
  
  { --------------------------------------------------------------------
    TBloggerAPI
    --------------------------------------------------------------------}
  
  TBloggerAPI = Class(TGoogleAPI)
  Private
    FBlogUserInfosInstance : TBlogUserInfosResource;
    FBlogsInstance : TBlogsResource;
    FCommentsInstance : TCommentsResource;
    FPageViewsInstance : TPageViewsResource;
    FPagesInstance : TPagesResource;
    FPostUserInfosInstance : TPostUserInfosResource;
    FPostsInstance : TPostsResource;
    FUsersInstance : TUsersResource;
    Function GetBlogUserInfosInstance : TBlogUserInfosResource;virtual;
    Function GetBlogsInstance : TBlogsResource;virtual;
    Function GetCommentsInstance : TCommentsResource;virtual;
    Function GetPageViewsInstance : TPageViewsResource;virtual;
    Function GetPagesInstance : TPagesResource;virtual;
    Function GetPostUserInfosInstance : TPostUserInfosResource;virtual;
    Function GetPostsInstance : TPostsResource;virtual;
    Function GetUsersInstance : TUsersResource;virtual;
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
    Function CreateBlogUserInfosResource(AOwner : TComponent) : TBlogUserInfosResource;virtual;overload;
    Function CreateBlogUserInfosResource : TBlogUserInfosResource;virtual;overload;
    Function CreateBlogsResource(AOwner : TComponent) : TBlogsResource;virtual;overload;
    Function CreateBlogsResource : TBlogsResource;virtual;overload;
    Function CreateCommentsResource(AOwner : TComponent) : TCommentsResource;virtual;overload;
    Function CreateCommentsResource : TCommentsResource;virtual;overload;
    Function CreatePageViewsResource(AOwner : TComponent) : TPageViewsResource;virtual;overload;
    Function CreatePageViewsResource : TPageViewsResource;virtual;overload;
    Function CreatePagesResource(AOwner : TComponent) : TPagesResource;virtual;overload;
    Function CreatePagesResource : TPagesResource;virtual;overload;
    Function CreatePostUserInfosResource(AOwner : TComponent) : TPostUserInfosResource;virtual;overload;
    Function CreatePostUserInfosResource : TPostUserInfosResource;virtual;overload;
    Function CreatePostsResource(AOwner : TComponent) : TPostsResource;virtual;overload;
    Function CreatePostsResource : TPostsResource;virtual;overload;
    Function CreateUsersResource(AOwner : TComponent) : TUsersResource;virtual;overload;
    Function CreateUsersResource : TUsersResource;virtual;overload;
    //Add default on-demand instances for resources
    Property BlogUserInfosResource : TBlogUserInfosResource Read GetBlogUserInfosInstance;
    Property BlogsResource : TBlogsResource Read GetBlogsInstance;
    Property CommentsResource : TCommentsResource Read GetCommentsInstance;
    Property PageViewsResource : TPageViewsResource Read GetPageViewsInstance;
    Property PagesResource : TPagesResource Read GetPagesInstance;
    Property PostUserInfosResource : TPostUserInfosResource Read GetPostUserInfosInstance;
    Property PostsResource : TPostsResource Read GetPostsInstance;
    Property UsersResource : TUsersResource Read GetUsersInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TBlog
  --------------------------------------------------------------------}


Procedure TBlog.SetcustomMetaData(AIndex : Integer; AValue : string); 

begin
  If (FcustomMetaData=AValue) then exit;
  FcustomMetaData:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlog.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlog.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlog.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlog.Setlocale(AIndex : Integer; AValue : TBloglocale); 

begin
  If (Flocale=AValue) then exit;
  Flocale:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlog.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlog.Setpages(AIndex : Integer; AValue : TBlogpages); 

begin
  If (Fpages=AValue) then exit;
  Fpages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlog.Setposts(AIndex : Integer; AValue : TBlogposts); 

begin
  If (Fposts=AValue) then exit;
  Fposts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlog.Set_published(AIndex : Integer; AValue : TDatetime); 

begin
  If (F_published=AValue) then exit;
  F_published:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlog.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlog.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlog.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlog.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TBlog.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_published' : Result:='published';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TBloglocale
  --------------------------------------------------------------------}


Procedure TBloglocale.Setcountry(AIndex : Integer; AValue : string); 

begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBloglocale.Setlanguage(AIndex : Integer; AValue : string); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBloglocale.Setvariant(AIndex : Integer; AValue : string); 

begin
  If (Fvariant=AValue) then exit;
  Fvariant:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBlogpages
  --------------------------------------------------------------------}


Procedure TBlogpages.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlogpages.SettotalItems(AIndex : Integer; AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBlogposts
  --------------------------------------------------------------------}


Procedure TBlogposts.Setitems(AIndex : Integer; AValue : TBlogpostsitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlogposts.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlogposts.SettotalItems(AIndex : Integer; AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBlogpostsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TBlogList
  --------------------------------------------------------------------}


Procedure TBlogList.SetblogUserInfos(AIndex : Integer; AValue : TBlogListblogUserInfos); 

begin
  If (FblogUserInfos=AValue) then exit;
  FblogUserInfos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlogList.Setitems(AIndex : Integer; AValue : TBlogListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlogList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBlogListblogUserInfos
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TBlogListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TBlogPerUserInfo
  --------------------------------------------------------------------}


Procedure TBlogPerUserInfo.SetblogId(AIndex : Integer; AValue : string); 

begin
  If (FblogId=AValue) then exit;
  FblogId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlogPerUserInfo.SethasAdminAccess(AIndex : Integer; AValue : boolean); 

begin
  If (FhasAdminAccess=AValue) then exit;
  FhasAdminAccess:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlogPerUserInfo.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlogPerUserInfo.SetphotosAlbumKey(AIndex : Integer; AValue : string); 

begin
  If (FphotosAlbumKey=AValue) then exit;
  FphotosAlbumKey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlogPerUserInfo.Setrole(AIndex : Integer; AValue : string); 

begin
  If (Frole=AValue) then exit;
  Frole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlogPerUserInfo.SetuserId(AIndex : Integer; AValue : string); 

begin
  If (FuserId=AValue) then exit;
  FuserId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBlogUserInfo
  --------------------------------------------------------------------}


Procedure TBlogUserInfo.Setblog(AIndex : Integer; AValue : TCommentblog); 

begin
  If (Fblog=AValue) then exit;
  Fblog:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlogUserInfo.Setblog_user_info(AIndex : Integer; AValue : TBlogPerUserInfo); 

begin
  If (Fblog_user_info=AValue) then exit;
  Fblog_user_info:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlogUserInfo.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TComment
  --------------------------------------------------------------------}


Procedure TComment.Setauthor(AIndex : Integer; AValue : TCommentauthor); 

begin
  If (Fauthor=AValue) then exit;
  Fauthor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setblog(AIndex : Integer; AValue : TCommentblog); 

begin
  If (Fblog=AValue) then exit;
  Fblog:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setcontent(AIndex : Integer; AValue : string); 

begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
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



Procedure TComment.Setpost(AIndex : Integer; AValue : TCommentpost); 

begin
  If (Fpost=AValue) then exit;
  Fpost:=AValue;
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



Procedure TComment.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TComment.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_published' : Result:='published';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TCommentauthor
  --------------------------------------------------------------------}


Procedure TCommentauthor.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentauthor.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentauthor.Setimage(AIndex : Integer; AValue : TCommentauthorimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentauthor.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentauthorimage
  --------------------------------------------------------------------}


Procedure TCommentauthorimage.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentblog
  --------------------------------------------------------------------}


Procedure TCommentblog.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
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





{ --------------------------------------------------------------------
  TCommentpost
  --------------------------------------------------------------------}


Procedure TCommentpost.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentList
  --------------------------------------------------------------------}


Procedure TCommentList.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentList.Setitems(AIndex : Integer; AValue : TCommentListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentList.SetprevPageToken(AIndex : Integer; AValue : string); 

begin
  If (FprevPageToken=AValue) then exit;
  FprevPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPage
  --------------------------------------------------------------------}


Procedure TPage.Setauthor(AIndex : Integer; AValue : TPageauthor); 

begin
  If (Fauthor=AValue) then exit;
  Fauthor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPage.Setblog(AIndex : Integer; AValue : TPageblog); 

begin
  If (Fblog=AValue) then exit;
  Fblog:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPage.Setcontent(AIndex : Integer; AValue : string); 

begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPage.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPage.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPage.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPage.Set_published(AIndex : Integer; AValue : TDatetime); 

begin
  If (F_published=AValue) then exit;
  F_published:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPage.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPage.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPage.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPage.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPage.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPage.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_published' : Result:='published';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPageauthor
  --------------------------------------------------------------------}


Procedure TPageauthor.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPageauthor.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPageauthor.Setimage(AIndex : Integer; AValue : TPageauthorimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPageauthor.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPageauthorimage
  --------------------------------------------------------------------}


Procedure TPageauthorimage.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPageblog
  --------------------------------------------------------------------}


Procedure TPageblog.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPageList
  --------------------------------------------------------------------}


Procedure TPageList.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPageList.Setitems(AIndex : Integer; AValue : TPageListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPageList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPageList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPageListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPageviews
  --------------------------------------------------------------------}


Procedure TPageviews.SetblogId(AIndex : Integer; AValue : string); 

begin
  If (FblogId=AValue) then exit;
  FblogId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPageviews.Setcounts(AIndex : Integer; AValue : TPageviewscounts); 

begin
  If (Fcounts=AValue) then exit;
  Fcounts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPageviews.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPageviewscounts
  --------------------------------------------------------------------}


Procedure TPageviewscounts.Setcount(AIndex : Integer; AValue : string); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPageviewscounts.SettimeRange(AIndex : Integer; AValue : string); 

begin
  If (FtimeRange=AValue) then exit;
  FtimeRange:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPost
  --------------------------------------------------------------------}


Procedure TPost.Setauthor(AIndex : Integer; AValue : TPostauthor); 

begin
  If (Fauthor=AValue) then exit;
  Fauthor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.Setblog(AIndex : Integer; AValue : TPostblog); 

begin
  If (Fblog=AValue) then exit;
  Fblog:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.Setcontent(AIndex : Integer; AValue : string); 

begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.SetcustomMetaData(AIndex : Integer; AValue : string); 

begin
  If (FcustomMetaData=AValue) then exit;
  FcustomMetaData:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.Setimages(AIndex : Integer; AValue : TPostimages); 

begin
  If (Fimages=AValue) then exit;
  Fimages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.Setlabels(AIndex : Integer; AValue : TPostlabels); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.Setlocation(AIndex : Integer; AValue : TPostlocation); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.Set_published(AIndex : Integer; AValue : TDatetime); 

begin
  If (F_published=AValue) then exit;
  F_published:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.SetreaderComments(AIndex : Integer; AValue : string); 

begin
  If (FreaderComments=AValue) then exit;
  FreaderComments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.Setreplies(AIndex : Integer; AValue : TPostreplies); 

begin
  If (Freplies=AValue) then exit;
  Freplies:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.SettitleLink(AIndex : Integer; AValue : string); 

begin
  If (FtitleLink=AValue) then exit;
  FtitleLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPost.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_published' : Result:='published';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPostauthor
  --------------------------------------------------------------------}


Procedure TPostauthor.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostauthor.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostauthor.Setimage(AIndex : Integer; AValue : TPostauthorimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostauthor.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPostauthorimage
  --------------------------------------------------------------------}


Procedure TPostauthorimage.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPostblog
  --------------------------------------------------------------------}


Procedure TPostblog.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPostimages
  --------------------------------------------------------------------}


Procedure TPostimages.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPostlabels
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPostlocation
  --------------------------------------------------------------------}


Procedure TPostlocation.Setlat(AIndex : Integer; AValue : double); 

begin
  If (Flat=AValue) then exit;
  Flat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostlocation.Setlng(AIndex : Integer; AValue : double); 

begin
  If (Flng=AValue) then exit;
  Flng:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostlocation.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostlocation.Setspan(AIndex : Integer; AValue : string); 

begin
  If (Fspan=AValue) then exit;
  Fspan:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPostreplies
  --------------------------------------------------------------------}


Procedure TPostreplies.Setitems(AIndex : Integer; AValue : TPostrepliesitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostreplies.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostreplies.SettotalItems(AIndex : Integer; AValue : string); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPostrepliesitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPostList
  --------------------------------------------------------------------}


Procedure TPostList.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostList.Setitems(AIndex : Integer; AValue : TPostListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPostListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPostPerUserInfo
  --------------------------------------------------------------------}


Procedure TPostPerUserInfo.SetblogId(AIndex : Integer; AValue : string); 

begin
  If (FblogId=AValue) then exit;
  FblogId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostPerUserInfo.SethasEditAccess(AIndex : Integer; AValue : boolean); 

begin
  If (FhasEditAccess=AValue) then exit;
  FhasEditAccess:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostPerUserInfo.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostPerUserInfo.SetpostId(AIndex : Integer; AValue : string); 

begin
  If (FpostId=AValue) then exit;
  FpostId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostPerUserInfo.SetuserId(AIndex : Integer; AValue : string); 

begin
  If (FuserId=AValue) then exit;
  FuserId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPostUserInfo
  --------------------------------------------------------------------}


Procedure TPostUserInfo.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostUserInfo.Setpost(AIndex : Integer; AValue : TCommentpost); 

begin
  If (Fpost=AValue) then exit;
  Fpost:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostUserInfo.Setpost_user_info(AIndex : Integer; AValue : TPostPerUserInfo); 

begin
  If (Fpost_user_info=AValue) then exit;
  Fpost_user_info:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPostUserInfosList
  --------------------------------------------------------------------}


Procedure TPostUserInfosList.Setitems(AIndex : Integer; AValue : TPostUserInfosListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostUserInfosList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostUserInfosList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPostUserInfosListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TUser
  --------------------------------------------------------------------}


Procedure TUser.Setabout(AIndex : Integer; AValue : string); 

begin
  If (Fabout=AValue) then exit;
  Fabout:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setblogs(AIndex : Integer; AValue : TUserblogs); 

begin
  If (Fblogs=AValue) then exit;
  Fblogs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setcreated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setlocale(AIndex : Integer; AValue : TUserlocale); 

begin
  If (Flocale=AValue) then exit;
  Flocale:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUserblogs
  --------------------------------------------------------------------}


Procedure TUserblogs.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUserlocale
  --------------------------------------------------------------------}


Procedure TUserlocale.Setcountry(AIndex : Integer; AValue : string); 

begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserlocale.Setlanguage(AIndex : Integer; AValue : string); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserlocale.Setvariant(AIndex : Integer; AValue : string); 

begin
  If (Fvariant=AValue) then exit;
  Fvariant:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBlogUserInfosResource
  --------------------------------------------------------------------}


Class Function TBlogUserInfosResource.ResourceName : String;

begin
  Result:='blogUserInfos';
end;

Class Function TBlogUserInfosResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbloggerAPI;
end;

Function TBlogUserInfosResource.Get(blogId: string; userId: string; AQuery : string = '') : TBlogUserInfo;

Const
  _HTTPMethod = 'GET';
  _Path       = 'users/{userId}/blogs/{blogId}';
  _Methodid   = 'blogger.blogUserInfos.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TBlogUserInfo) as TBlogUserInfo;
end;


Function TBlogUserInfosResource.Get(blogId: string; userId: string; AQuery : TBlogUserInfosgetOptions) : TBlogUserInfo;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxPosts',AQuery.maxPosts);
  Result:=Get(blogId,userId,_Q);
end;



{ --------------------------------------------------------------------
  TBlogsResource
  --------------------------------------------------------------------}


Class Function TBlogsResource.ResourceName : String;

begin
  Result:='blogs';
end;

Class Function TBlogsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbloggerAPI;
end;

Function TBlogsResource.Get(blogId: string; AQuery : string = '') : TBlog;

Const
  _HTTPMethod = 'GET';
  _Path       = 'blogs/{blogId}';
  _Methodid   = 'blogger.blogs.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TBlog) as TBlog;
end;


Function TBlogsResource.Get(blogId: string; AQuery : TBlogsgetOptions) : TBlog;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxPosts',AQuery.maxPosts);
  AddToQuery(_Q,'view',AQuery.view);
  Result:=Get(blogId,_Q);
end;

Function TBlogsResource.GetByUrl(AQuery : string = '') : TBlog;

Const
  _HTTPMethod = 'GET';
  _Path       = 'blogs/byurl';
  _Methodid   = 'blogger.blogs.getByUrl';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TBlog) as TBlog;
end;


Function TBlogsResource.GetByUrl(AQuery : TBlogsgetByUrlOptions) : TBlog;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'url',AQuery.url);
  AddToQuery(_Q,'view',AQuery.view);
  Result:=GetByUrl(_Q);
end;

Function TBlogsResource.ListByUser(userId: string; AQuery : string = '') : TBlogList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'users/{userId}/blogs';
  _Methodid   = 'blogger.blogs.listByUser';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TBlogList) as TBlogList;
end;


Function TBlogsResource.ListByUser(userId: string; AQuery : TBlogslistByUserOptions) : TBlogList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'fetchUserInfo',AQuery.fetchUserInfo);
  AddToQuery(_Q,'role',AQuery.role);
  AddToQuery(_Q,'status',AQuery.status);
  AddToQuery(_Q,'view',AQuery.view);
  Result:=ListByUser(userId,_Q);
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
  Result:=TbloggerAPI;
end;

Function TCommentsResource.Approve(blogId: string; commentId: string; postId: string) : TComment;

Const
  _HTTPMethod = 'POST';
  _Path       = 'blogs/{blogId}/posts/{postId}/comments/{commentId}/approve';
  _Methodid   = 'blogger.comments.approve';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId,'commentId',commentId,'postId',postId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TComment) as TComment;
end;

Procedure TCommentsResource.Delete(blogId: string; commentId: string; postId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'blogs/{blogId}/posts/{postId}/comments/{commentId}';
  _Methodid   = 'blogger.comments.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId,'commentId',commentId,'postId',postId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TCommentsResource.Get(blogId: string; commentId: string; postId: string; AQuery : string = '') : TComment;

Const
  _HTTPMethod = 'GET';
  _Path       = 'blogs/{blogId}/posts/{postId}/comments/{commentId}';
  _Methodid   = 'blogger.comments.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId,'commentId',commentId,'postId',postId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TComment) as TComment;
end;


Function TCommentsResource.Get(blogId: string; commentId: string; postId: string; AQuery : TCommentsgetOptions) : TComment;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'view',AQuery.view);
  Result:=Get(blogId,commentId,postId,_Q);
end;

Function TCommentsResource.List(blogId: string; postId: string; AQuery : string = '') : TCommentList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'blogs/{blogId}/posts/{postId}/comments';
  _Methodid   = 'blogger.comments.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId,'postId',postId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TCommentList) as TCommentList;
end;


Function TCommentsResource.List(blogId: string; postId: string; AQuery : TCommentslistOptions) : TCommentList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'endDate',AQuery.endDate);
  AddToQuery(_Q,'fetchBodies',AQuery.fetchBodies);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'startDate',AQuery.startDate);
  AddToQuery(_Q,'status',AQuery.status);
  AddToQuery(_Q,'view',AQuery.view);
  Result:=List(blogId,postId,_Q);
end;

Function TCommentsResource.ListByBlog(blogId: string; AQuery : string = '') : TCommentList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'blogs/{blogId}/comments';
  _Methodid   = 'blogger.comments.listByBlog';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TCommentList) as TCommentList;
end;


Function TCommentsResource.ListByBlog(blogId: string; AQuery : TCommentslistByBlogOptions) : TCommentList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'endDate',AQuery.endDate);
  AddToQuery(_Q,'fetchBodies',AQuery.fetchBodies);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'startDate',AQuery.startDate);
  AddToQuery(_Q,'status',AQuery.status);
  Result:=ListByBlog(blogId,_Q);
end;

Function TCommentsResource.MarkAsSpam(blogId: string; commentId: string; postId: string) : TComment;

Const
  _HTTPMethod = 'POST';
  _Path       = 'blogs/{blogId}/posts/{postId}/comments/{commentId}/spam';
  _Methodid   = 'blogger.comments.markAsSpam';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId,'commentId',commentId,'postId',postId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TComment) as TComment;
end;

Function TCommentsResource.RemoveContent(blogId: string; commentId: string; postId: string) : TComment;

Const
  _HTTPMethod = 'POST';
  _Path       = 'blogs/{blogId}/posts/{postId}/comments/{commentId}/removecontent';
  _Methodid   = 'blogger.comments.removeContent';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId,'commentId',commentId,'postId',postId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TComment) as TComment;
end;



{ --------------------------------------------------------------------
  TPageViewsResource
  --------------------------------------------------------------------}


Class Function TPageViewsResource.ResourceName : String;

begin
  Result:='pageViews';
end;

Class Function TPageViewsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbloggerAPI;
end;

Function TPageViewsResource.Get(blogId: string; AQuery : string = '') : TPageviews;

Const
  _HTTPMethod = 'GET';
  _Path       = 'blogs/{blogId}/pageviews';
  _Methodid   = 'blogger.pageViews.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPageviews) as TPageviews;
end;


Function TPageViewsResource.Get(blogId: string; AQuery : TPageViewsgetOptions) : TPageviews;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'range',AQuery.range);
  Result:=Get(blogId,_Q);
end;



{ --------------------------------------------------------------------
  TPagesResource
  --------------------------------------------------------------------}


Class Function TPagesResource.ResourceName : String;

begin
  Result:='pages';
end;

Class Function TPagesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbloggerAPI;
end;

Procedure TPagesResource.Delete(blogId: string; pageId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'blogs/{blogId}/pages/{pageId}';
  _Methodid   = 'blogger.pages.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId,'pageId',pageId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TPagesResource.Get(blogId: string; pageId: string; AQuery : string = '') : TPage;

Const
  _HTTPMethod = 'GET';
  _Path       = 'blogs/{blogId}/pages/{pageId}';
  _Methodid   = 'blogger.pages.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId,'pageId',pageId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPage) as TPage;
end;


Function TPagesResource.Get(blogId: string; pageId: string; AQuery : TPagesgetOptions) : TPage;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'view',AQuery.view);
  Result:=Get(blogId,pageId,_Q);
end;

Function TPagesResource.Insert(blogId: string; aPage : TPage; AQuery : string = '') : TPage;

Const
  _HTTPMethod = 'POST';
  _Path       = 'blogs/{blogId}/pages';
  _Methodid   = 'blogger.pages.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aPage,TPage) as TPage;
end;


Function TPagesResource.Insert(blogId: string; aPage : TPage; AQuery : TPagesinsertOptions) : TPage;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'isDraft',AQuery.isDraft);
  Result:=Insert(blogId,aPage,_Q);
end;

Function TPagesResource.List(blogId: string; AQuery : string = '') : TPageList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'blogs/{blogId}/pages';
  _Methodid   = 'blogger.pages.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPageList) as TPageList;
end;


Function TPagesResource.List(blogId: string; AQuery : TPageslistOptions) : TPageList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'fetchBodies',AQuery.fetchBodies);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'status',AQuery.status);
  AddToQuery(_Q,'view',AQuery.view);
  Result:=List(blogId,_Q);
end;

Function TPagesResource.Patch(blogId: string; pageId: string; aPage : TPage; AQuery : string = '') : TPage;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'blogs/{blogId}/pages/{pageId}';
  _Methodid   = 'blogger.pages.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId,'pageId',pageId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aPage,TPage) as TPage;
end;


Function TPagesResource.Patch(blogId: string; pageId: string; aPage : TPage; AQuery : TPagespatchOptions) : TPage;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'publish',AQuery.publish);
  AddToQuery(_Q,'revert',AQuery.revert);
  Result:=Patch(blogId,pageId,aPage,_Q);
end;

Function TPagesResource.Publish(blogId: string; pageId: string) : TPage;

Const
  _HTTPMethod = 'POST';
  _Path       = 'blogs/{blogId}/pages/{pageId}/publish';
  _Methodid   = 'blogger.pages.publish';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId,'pageId',pageId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPage) as TPage;
end;

Function TPagesResource.Revert(blogId: string; pageId: string) : TPage;

Const
  _HTTPMethod = 'POST';
  _Path       = 'blogs/{blogId}/pages/{pageId}/revert';
  _Methodid   = 'blogger.pages.revert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId,'pageId',pageId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPage) as TPage;
end;

Function TPagesResource.Update(blogId: string; pageId: string; aPage : TPage; AQuery : string = '') : TPage;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'blogs/{blogId}/pages/{pageId}';
  _Methodid   = 'blogger.pages.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId,'pageId',pageId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aPage,TPage) as TPage;
end;


Function TPagesResource.Update(blogId: string; pageId: string; aPage : TPage; AQuery : TPagesupdateOptions) : TPage;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'publish',AQuery.publish);
  AddToQuery(_Q,'revert',AQuery.revert);
  Result:=Update(blogId,pageId,aPage,_Q);
end;



{ --------------------------------------------------------------------
  TPostUserInfosResource
  --------------------------------------------------------------------}


Class Function TPostUserInfosResource.ResourceName : String;

begin
  Result:='postUserInfos';
end;

Class Function TPostUserInfosResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbloggerAPI;
end;

Function TPostUserInfosResource.Get(blogId: string; postId: string; userId: string; AQuery : string = '') : TPostUserInfo;

Const
  _HTTPMethod = 'GET';
  _Path       = 'users/{userId}/blogs/{blogId}/posts/{postId}';
  _Methodid   = 'blogger.postUserInfos.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId,'postId',postId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPostUserInfo) as TPostUserInfo;
end;


Function TPostUserInfosResource.Get(blogId: string; postId: string; userId: string; AQuery : TPostUserInfosgetOptions) : TPostUserInfo;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxComments',AQuery.maxComments);
  Result:=Get(blogId,postId,userId,_Q);
end;

Function TPostUserInfosResource.List(blogId: string; userId: string; AQuery : string = '') : TPostUserInfosList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'users/{userId}/blogs/{blogId}/posts';
  _Methodid   = 'blogger.postUserInfos.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPostUserInfosList) as TPostUserInfosList;
end;


Function TPostUserInfosResource.List(blogId: string; userId: string; AQuery : TPostUserInfoslistOptions) : TPostUserInfosList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'endDate',AQuery.endDate);
  AddToQuery(_Q,'fetchBodies',AQuery.fetchBodies);
  AddToQuery(_Q,'labels',AQuery.labels);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'orderBy',AQuery.orderBy);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'startDate',AQuery.startDate);
  AddToQuery(_Q,'status',AQuery.status);
  AddToQuery(_Q,'view',AQuery.view);
  Result:=List(blogId,userId,_Q);
end;



{ --------------------------------------------------------------------
  TPostsResource
  --------------------------------------------------------------------}


Class Function TPostsResource.ResourceName : String;

begin
  Result:='posts';
end;

Class Function TPostsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbloggerAPI;
end;

Procedure TPostsResource.Delete(blogId: string; postId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'blogs/{blogId}/posts/{postId}';
  _Methodid   = 'blogger.posts.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId,'postId',postId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TPostsResource.Get(blogId: string; postId: string; AQuery : string = '') : TPost;

Const
  _HTTPMethod = 'GET';
  _Path       = 'blogs/{blogId}/posts/{postId}';
  _Methodid   = 'blogger.posts.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId,'postId',postId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPost) as TPost;
end;


Function TPostsResource.Get(blogId: string; postId: string; AQuery : TPostsgetOptions) : TPost;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'fetchBody',AQuery.fetchBody);
  AddToQuery(_Q,'fetchImages',AQuery.fetchImages);
  AddToQuery(_Q,'maxComments',AQuery.maxComments);
  AddToQuery(_Q,'view',AQuery.view);
  Result:=Get(blogId,postId,_Q);
end;

Function TPostsResource.GetByPath(blogId: string; AQuery : string = '') : TPost;

Const
  _HTTPMethod = 'GET';
  _Path       = 'blogs/{blogId}/posts/bypath';
  _Methodid   = 'blogger.posts.getByPath';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPost) as TPost;
end;


Function TPostsResource.GetByPath(blogId: string; AQuery : TPostsgetByPathOptions) : TPost;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxComments',AQuery.maxComments);
  AddToQuery(_Q,'path',AQuery.path);
  AddToQuery(_Q,'view',AQuery.view);
  Result:=GetByPath(blogId,_Q);
end;

Function TPostsResource.Insert(blogId: string; aPost : TPost; AQuery : string = '') : TPost;

Const
  _HTTPMethod = 'POST';
  _Path       = 'blogs/{blogId}/posts';
  _Methodid   = 'blogger.posts.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aPost,TPost) as TPost;
end;


Function TPostsResource.Insert(blogId: string; aPost : TPost; AQuery : TPostsinsertOptions) : TPost;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'fetchBody',AQuery.fetchBody);
  AddToQuery(_Q,'fetchImages',AQuery.fetchImages);
  AddToQuery(_Q,'isDraft',AQuery.isDraft);
  Result:=Insert(blogId,aPost,_Q);
end;

Function TPostsResource.List(blogId: string; AQuery : string = '') : TPostList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'blogs/{blogId}/posts';
  _Methodid   = 'blogger.posts.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPostList) as TPostList;
end;


Function TPostsResource.List(blogId: string; AQuery : TPostslistOptions) : TPostList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'endDate',AQuery.endDate);
  AddToQuery(_Q,'fetchBodies',AQuery.fetchBodies);
  AddToQuery(_Q,'fetchImages',AQuery.fetchImages);
  AddToQuery(_Q,'labels',AQuery.labels);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'orderBy',AQuery.orderBy);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'startDate',AQuery.startDate);
  AddToQuery(_Q,'status',AQuery.status);
  AddToQuery(_Q,'view',AQuery.view);
  Result:=List(blogId,_Q);
end;

Function TPostsResource.Patch(blogId: string; postId: string; aPost : TPost; AQuery : string = '') : TPost;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'blogs/{blogId}/posts/{postId}';
  _Methodid   = 'blogger.posts.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId,'postId',postId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aPost,TPost) as TPost;
end;


Function TPostsResource.Patch(blogId: string; postId: string; aPost : TPost; AQuery : TPostspatchOptions) : TPost;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'fetchBody',AQuery.fetchBody);
  AddToQuery(_Q,'fetchImages',AQuery.fetchImages);
  AddToQuery(_Q,'maxComments',AQuery.maxComments);
  AddToQuery(_Q,'publish',AQuery.publish);
  AddToQuery(_Q,'revert',AQuery.revert);
  Result:=Patch(blogId,postId,aPost,_Q);
end;

Function TPostsResource.Publish(blogId: string; postId: string; AQuery : string = '') : TPost;

Const
  _HTTPMethod = 'POST';
  _Path       = 'blogs/{blogId}/posts/{postId}/publish';
  _Methodid   = 'blogger.posts.publish';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId,'postId',postId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPost) as TPost;
end;


Function TPostsResource.Publish(blogId: string; postId: string; AQuery : TPostspublishOptions) : TPost;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'publishDate',AQuery.publishDate);
  Result:=Publish(blogId,postId,_Q);
end;

Function TPostsResource.Revert(blogId: string; postId: string) : TPost;

Const
  _HTTPMethod = 'POST';
  _Path       = 'blogs/{blogId}/posts/{postId}/revert';
  _Methodid   = 'blogger.posts.revert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId,'postId',postId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPost) as TPost;
end;

Function TPostsResource.Search(blogId: string; AQuery : string = '') : TPostList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'blogs/{blogId}/posts/search';
  _Methodid   = 'blogger.posts.search';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPostList) as TPostList;
end;


Function TPostsResource.Search(blogId: string; AQuery : TPostssearchOptions) : TPostList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'fetchBodies',AQuery.fetchBodies);
  AddToQuery(_Q,'orderBy',AQuery.orderBy);
  AddToQuery(_Q,'q',AQuery.q);
  Result:=Search(blogId,_Q);
end;

Function TPostsResource.Update(blogId: string; postId: string; aPost : TPost; AQuery : string = '') : TPost;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'blogs/{blogId}/posts/{postId}';
  _Methodid   = 'blogger.posts.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['blogId',blogId,'postId',postId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aPost,TPost) as TPost;
end;


Function TPostsResource.Update(blogId: string; postId: string; aPost : TPost; AQuery : TPostsupdateOptions) : TPost;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'fetchBody',AQuery.fetchBody);
  AddToQuery(_Q,'fetchImages',AQuery.fetchImages);
  AddToQuery(_Q,'maxComments',AQuery.maxComments);
  AddToQuery(_Q,'publish',AQuery.publish);
  AddToQuery(_Q,'revert',AQuery.revert);
  Result:=Update(blogId,postId,aPost,_Q);
end;



{ --------------------------------------------------------------------
  TUsersResource
  --------------------------------------------------------------------}


Class Function TUsersResource.ResourceName : String;

begin
  Result:='users';
end;

Class Function TUsersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TbloggerAPI;
end;

Function TUsersResource.Get(userId: string) : TUser;

Const
  _HTTPMethod = 'GET';
  _Path       = 'users/{userId}';
  _Methodid   = 'blogger.users.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TUser) as TUser;
end;



{ --------------------------------------------------------------------
  TBloggerAPI
  --------------------------------------------------------------------}

Class Function TBloggerAPI.APIName : String;

begin
  Result:='blogger';
end;

Class Function TBloggerAPI.APIVersion : String;

begin
  Result:='v3';
end;

Class Function TBloggerAPI.APIRevision : String;

begin
  Result:='20150422';
end;

Class Function TBloggerAPI.APIID : String;

begin
  Result:='blogger:v3';
end;

Class Function TBloggerAPI.APITitle : String;

begin
  Result:='Blogger API';
end;

Class Function TBloggerAPI.APIDescription : String;

begin
  Result:='API for access to the data within Blogger.';
end;

Class Function TBloggerAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TBloggerAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TBloggerAPI.APIIcon16 : String;

begin
  Result:='https://www.google.com/images/icons/product/blogger-16.png';
end;

Class Function TBloggerAPI.APIIcon32 : String;

begin
  Result:='https://www.google.com/images/icons/product/blogger-32.png';
end;

Class Function TBloggerAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/blogger/docs/3.0/getting_started';
end;

Class Function TBloggerAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TBloggerAPI.APIbasePath : string;

begin
  Result:='/blogger/v3/';
end;

Class Function TBloggerAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/blogger/v3/';
end;

Class Function TBloggerAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TBloggerAPI.APIservicePath : string;

begin
  Result:='blogger/v3/';
end;

Class Function TBloggerAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TBloggerAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,2);
  Result[0].Name:='https://www.googleapis.com/auth/blogger';
  Result[0].Description:='Manage your Blogger account';
  Result[1].Name:='https://www.googleapis.com/auth/blogger.readonly';
  Result[1].Description:='View your Blogger account';
  
end;

Class Function TBloggerAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TBloggerAPI.RegisterAPIResources;

begin
  TBlog.RegisterObject;
  TBloglocale.RegisterObject;
  TBlogpages.RegisterObject;
  TBlogposts.RegisterObject;
  TBlogpostsitems.RegisterObject;
  TBlogList.RegisterObject;
  TBlogListblogUserInfos.RegisterObject;
  TBlogListitems.RegisterObject;
  TBlogPerUserInfo.RegisterObject;
  TBlogUserInfo.RegisterObject;
  TComment.RegisterObject;
  TCommentauthor.RegisterObject;
  TCommentauthorimage.RegisterObject;
  TCommentblog.RegisterObject;
  TCommentinReplyTo.RegisterObject;
  TCommentpost.RegisterObject;
  TCommentList.RegisterObject;
  TCommentListitems.RegisterObject;
  TPage.RegisterObject;
  TPageauthor.RegisterObject;
  TPageauthorimage.RegisterObject;
  TPageblog.RegisterObject;
  TPageList.RegisterObject;
  TPageListitems.RegisterObject;
  TPageviews.RegisterObject;
  TPageviewscounts.RegisterObject;
  TPost.RegisterObject;
  TPostauthor.RegisterObject;
  TPostauthorimage.RegisterObject;
  TPostblog.RegisterObject;
  TPostimages.RegisterObject;
  TPostlabels.RegisterObject;
  TPostlocation.RegisterObject;
  TPostreplies.RegisterObject;
  TPostrepliesitems.RegisterObject;
  TPostList.RegisterObject;
  TPostListitems.RegisterObject;
  TPostPerUserInfo.RegisterObject;
  TPostUserInfo.RegisterObject;
  TPostUserInfosList.RegisterObject;
  TPostUserInfosListitems.RegisterObject;
  TUser.RegisterObject;
  TUserblogs.RegisterObject;
  TUserlocale.RegisterObject;
end;


Function TBloggerAPI.GetBlogUserInfosInstance : TBlogUserInfosResource;

begin
  if (FBlogUserInfosInstance=Nil) then
    FBlogUserInfosInstance:=CreateBlogUserInfosResource;
  Result:=FBlogUserInfosInstance;
end;

Function TBloggerAPI.CreateBlogUserInfosResource : TBlogUserInfosResource;

begin
  Result:=CreateBlogUserInfosResource(Self);
end;


Function TBloggerAPI.CreateBlogUserInfosResource(AOwner : TComponent) : TBlogUserInfosResource;

begin
  Result:=TBlogUserInfosResource.Create(AOwner);
  Result.API:=Self;
end;



Function TBloggerAPI.GetBlogsInstance : TBlogsResource;

begin
  if (FBlogsInstance=Nil) then
    FBlogsInstance:=CreateBlogsResource;
  Result:=FBlogsInstance;
end;

Function TBloggerAPI.CreateBlogsResource : TBlogsResource;

begin
  Result:=CreateBlogsResource(Self);
end;


Function TBloggerAPI.CreateBlogsResource(AOwner : TComponent) : TBlogsResource;

begin
  Result:=TBlogsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TBloggerAPI.GetCommentsInstance : TCommentsResource;

begin
  if (FCommentsInstance=Nil) then
    FCommentsInstance:=CreateCommentsResource;
  Result:=FCommentsInstance;
end;

Function TBloggerAPI.CreateCommentsResource : TCommentsResource;

begin
  Result:=CreateCommentsResource(Self);
end;


Function TBloggerAPI.CreateCommentsResource(AOwner : TComponent) : TCommentsResource;

begin
  Result:=TCommentsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TBloggerAPI.GetPageViewsInstance : TPageViewsResource;

begin
  if (FPageViewsInstance=Nil) then
    FPageViewsInstance:=CreatePageViewsResource;
  Result:=FPageViewsInstance;
end;

Function TBloggerAPI.CreatePageViewsResource : TPageViewsResource;

begin
  Result:=CreatePageViewsResource(Self);
end;


Function TBloggerAPI.CreatePageViewsResource(AOwner : TComponent) : TPageViewsResource;

begin
  Result:=TPageViewsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TBloggerAPI.GetPagesInstance : TPagesResource;

begin
  if (FPagesInstance=Nil) then
    FPagesInstance:=CreatePagesResource;
  Result:=FPagesInstance;
end;

Function TBloggerAPI.CreatePagesResource : TPagesResource;

begin
  Result:=CreatePagesResource(Self);
end;


Function TBloggerAPI.CreatePagesResource(AOwner : TComponent) : TPagesResource;

begin
  Result:=TPagesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TBloggerAPI.GetPostUserInfosInstance : TPostUserInfosResource;

begin
  if (FPostUserInfosInstance=Nil) then
    FPostUserInfosInstance:=CreatePostUserInfosResource;
  Result:=FPostUserInfosInstance;
end;

Function TBloggerAPI.CreatePostUserInfosResource : TPostUserInfosResource;

begin
  Result:=CreatePostUserInfosResource(Self);
end;


Function TBloggerAPI.CreatePostUserInfosResource(AOwner : TComponent) : TPostUserInfosResource;

begin
  Result:=TPostUserInfosResource.Create(AOwner);
  Result.API:=Self;
end;



Function TBloggerAPI.GetPostsInstance : TPostsResource;

begin
  if (FPostsInstance=Nil) then
    FPostsInstance:=CreatePostsResource;
  Result:=FPostsInstance;
end;

Function TBloggerAPI.CreatePostsResource : TPostsResource;

begin
  Result:=CreatePostsResource(Self);
end;


Function TBloggerAPI.CreatePostsResource(AOwner : TComponent) : TPostsResource;

begin
  Result:=TPostsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TBloggerAPI.GetUsersInstance : TUsersResource;

begin
  if (FUsersInstance=Nil) then
    FUsersInstance:=CreateUsersResource;
  Result:=FUsersInstance;
end;

Function TBloggerAPI.CreateUsersResource : TUsersResource;

begin
  Result:=CreateUsersResource(Self);
end;


Function TBloggerAPI.CreateUsersResource(AOwner : TComponent) : TUsersResource;

begin
  Result:=TUsersResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TBloggerAPI.RegisterAPI;
end.
