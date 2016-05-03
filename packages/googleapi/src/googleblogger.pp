unit googleblogger;
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
//Generated on: 16-5-15 08:52:59
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TBlog = Class;
  TBlogList = Class;
  TBlogPerUserInfo = Class;
  TBlogUserInfo = Class;
  TComment = Class;
  TCommentList = Class;
  TPage = Class;
  TPageList = Class;
  TPageviews = Class;
  TPost = Class;
  TPostList = Class;
  TPostPerUserInfo = Class;
  TPostUserInfo = Class;
  TPostUserInfosList = Class;
  TUser = Class;
  TBlogArray = Array of TBlog;
  TBlogListArray = Array of TBlogList;
  TBlogPerUserInfoArray = Array of TBlogPerUserInfo;
  TBlogUserInfoArray = Array of TBlogUserInfo;
  TCommentArray = Array of TComment;
  TCommentListArray = Array of TCommentList;
  TPageArray = Array of TPage;
  TPageListArray = Array of TPageList;
  TPageviewsArray = Array of TPageviews;
  TPostArray = Array of TPost;
  TPostListArray = Array of TPostList;
  TPostPerUserInfoArray = Array of TPostPerUserInfo;
  TPostUserInfoArray = Array of TPostUserInfo;
  TPostUserInfosListArray = Array of TPostUserInfosList;
  TUserArray = Array of TUser;
  //Anonymous types, using auto-generated names
  TBlogTypelocale = Class;
  TBlogTypepages = Class;
  TBlogTypeposts = Class;
  TCommentTypeauthorTypeimage = Class;
  TCommentTypeauthor = Class;
  TCommentTypeblog = Class;
  TCommentTypeinReplyTo = Class;
  TCommentTypepost = Class;
  TPageTypeauthorTypeimage = Class;
  TPageTypeauthor = Class;
  TPageTypeblog = Class;
  TPageviewsTypecountsItem = Class;
  TPostTypeauthorTypeimage = Class;
  TPostTypeauthor = Class;
  TPostTypeblog = Class;
  TPostTypeimagesItem = Class;
  TPostTypelocation = Class;
  TPostTypereplies = Class;
  TUserTypeblogs = Class;
  TUserTypelocale = Class;
  TBlogTypepostsTypeitemsArray = Array of TPost;
  TBlogListTypeblogUserInfosArray = Array of TBlogUserInfo;
  TBlogListTypeitemsArray = Array of TBlog;
  TCommentListTypeitemsArray = Array of TComment;
  TPageListTypeitemsArray = Array of TPage;
  TPageviewsTypecountsArray = Array of TPageviewsTypecountsItem;
  TPostTypeimagesArray = Array of TPostTypeimagesItem;
  TPostTyperepliesTypeitemsArray = Array of TComment;
  TPostListTypeitemsArray = Array of TPost;
  TPostUserInfosListTypeitemsArray = Array of TPostUserInfo;
  
  { --------------------------------------------------------------------
    TBlogTypelocale
    --------------------------------------------------------------------}
  
  TBlogTypelocale = Class(TGoogleBaseObject)
  Private
    Fcountry : String;
    Flanguage : String;
    Fvariant : String;
  Protected
    //Property setters
    Procedure Setcountry(AIndex : Integer; AValue : String); virtual;
    Procedure Setlanguage(AIndex : Integer; AValue : String); virtual;
    Procedure Setvariant(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property country : String Index 0 Read Fcountry Write Setcountry;
    Property language : String Index 8 Read Flanguage Write Setlanguage;
    Property variant : String Index 16 Read Fvariant Write Setvariant;
  end;
  TBlogTypelocaleClass = Class of TBlogTypelocale;
  
  { --------------------------------------------------------------------
    TBlogTypepages
    --------------------------------------------------------------------}
  
  TBlogTypepages = Class(TGoogleBaseObject)
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
  TBlogTypepagesClass = Class of TBlogTypepages;
  
  { --------------------------------------------------------------------
    TBlogTypeposts
    --------------------------------------------------------------------}
  
  TBlogTypeposts = Class(TGoogleBaseObject)
  Private
    Fitems : TBlogTypepostsTypeitemsArray;
    FselfLink : String;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TBlogTypepostsTypeitemsArray); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TBlogTypepostsTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property selfLink : String Index 8 Read FselfLink Write SetselfLink;
    Property totalItems : integer Index 16 Read FtotalItems Write SettotalItems;
  end;
  TBlogTypepostsClass = Class of TBlogTypeposts;
  
  { --------------------------------------------------------------------
    TBlog
    --------------------------------------------------------------------}
  
  TBlog = Class(TGoogleBaseObject)
  Private
    FcustomMetaData : String;
    Fdescription : String;
    Fid : String;
    Fkind : String;
    Flocale : TBlogTypelocale;
    Fname : String;
    Fpages : TBlogTypepages;
    Fposts : TBlogTypeposts;
    F_published : TDatetime;
    FselfLink : String;
    Fstatus : String;
    Fupdated : TDatetime;
    Furl : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetcustomMetaData(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlocale(AIndex : Integer; AValue : TBlogTypelocale); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setpages(AIndex : Integer; AValue : TBlogTypepages); virtual;
    Procedure Setposts(AIndex : Integer; AValue : TBlogTypeposts); virtual;
    Procedure Set_published(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property customMetaData : String Index 0 Read FcustomMetaData Write SetcustomMetaData;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property locale : TBlogTypelocale Index 32 Read Flocale Write Setlocale;
    Property name : String Index 40 Read Fname Write Setname;
    Property pages : TBlogTypepages Index 48 Read Fpages Write Setpages;
    Property posts : TBlogTypeposts Index 56 Read Fposts Write Setposts;
    Property _published : TDatetime Index 64 Read F_published Write Set_published;
    Property selfLink : String Index 72 Read FselfLink Write SetselfLink;
    Property status : String Index 80 Read Fstatus Write Setstatus;
    Property updated : TDatetime Index 88 Read Fupdated Write Setupdated;
    Property url : String Index 96 Read Furl Write Seturl;
  end;
  TBlogClass = Class of TBlog;
  
  { --------------------------------------------------------------------
    TBlogList
    --------------------------------------------------------------------}
  
  TBlogList = Class(TGoogleBaseObject)
  Private
    FblogUserInfos : TBlogListTypeblogUserInfosArray;
    Fitems : TBlogListTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetblogUserInfos(AIndex : Integer; AValue : TBlogListTypeblogUserInfosArray); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TBlogListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property blogUserInfos : TBlogListTypeblogUserInfosArray Index 0 Read FblogUserInfos Write SetblogUserInfos;
    Property items : TBlogListTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
  end;
  TBlogListClass = Class of TBlogList;
  
  { --------------------------------------------------------------------
    TBlogPerUserInfo
    --------------------------------------------------------------------}
  
  TBlogPerUserInfo = Class(TGoogleBaseObject)
  Private
    FblogId : String;
    FhasAdminAccess : boolean;
    Fkind : String;
    FphotosAlbumKey : String;
    Frole : String;
    FuserId : String;
  Protected
    //Property setters
    Procedure SetblogId(AIndex : Integer; AValue : String); virtual;
    Procedure SethasAdminAccess(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetphotosAlbumKey(AIndex : Integer; AValue : String); virtual;
    Procedure Setrole(AIndex : Integer; AValue : String); virtual;
    Procedure SetuserId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property blogId : String Index 0 Read FblogId Write SetblogId;
    Property hasAdminAccess : boolean Index 8 Read FhasAdminAccess Write SethasAdminAccess;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property photosAlbumKey : String Index 24 Read FphotosAlbumKey Write SetphotosAlbumKey;
    Property role : String Index 32 Read Frole Write Setrole;
    Property userId : String Index 40 Read FuserId Write SetuserId;
  end;
  TBlogPerUserInfoClass = Class of TBlogPerUserInfo;
  
  { --------------------------------------------------------------------
    TBlogUserInfo
    --------------------------------------------------------------------}
  
  TBlogUserInfo = Class(TGoogleBaseObject)
  Private
    Fblog : TBlog;
    Fblog_user_info : TBlogPerUserInfo;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setblog(AIndex : Integer; AValue : TBlog); virtual;
    Procedure Setblog_user_info(AIndex : Integer; AValue : TBlogPerUserInfo); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property blog : TBlog Index 0 Read Fblog Write Setblog;
    Property blog_user_info : TBlogPerUserInfo Index 8 Read Fblog_user_info Write Setblog_user_info;
    Property kind : String Index 16 Read Fkind Write Setkind;
  end;
  TBlogUserInfoClass = Class of TBlogUserInfo;
  
  { --------------------------------------------------------------------
    TCommentTypeauthorTypeimage
    --------------------------------------------------------------------}
  
  TCommentTypeauthorTypeimage = Class(TGoogleBaseObject)
  Private
    Furl : String;
  Protected
    //Property setters
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property url : String Index 0 Read Furl Write Seturl;
  end;
  TCommentTypeauthorTypeimageClass = Class of TCommentTypeauthorTypeimage;
  
  { --------------------------------------------------------------------
    TCommentTypeauthor
    --------------------------------------------------------------------}
  
  TCommentTypeauthor = Class(TGoogleBaseObject)
  Private
    FdisplayName : String;
    Fid : String;
    Fimage : TCommentTypeauthorTypeimage;
    Furl : String;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setimage(AIndex : Integer; AValue : TCommentTypeauthorTypeimage); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property displayName : String Index 0 Read FdisplayName Write SetdisplayName;
    Property id : String Index 8 Read Fid Write Setid;
    Property image : TCommentTypeauthorTypeimage Index 16 Read Fimage Write Setimage;
    Property url : String Index 24 Read Furl Write Seturl;
  end;
  TCommentTypeauthorClass = Class of TCommentTypeauthor;
  
  { --------------------------------------------------------------------
    TCommentTypeblog
    --------------------------------------------------------------------}
  
  TCommentTypeblog = Class(TGoogleBaseObject)
  Private
    Fid : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
  end;
  TCommentTypeblogClass = Class of TCommentTypeblog;
  
  { --------------------------------------------------------------------
    TCommentTypeinReplyTo
    --------------------------------------------------------------------}
  
  TCommentTypeinReplyTo = Class(TGoogleBaseObject)
  Private
    Fid : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
  end;
  TCommentTypeinReplyToClass = Class of TCommentTypeinReplyTo;
  
  { --------------------------------------------------------------------
    TCommentTypepost
    --------------------------------------------------------------------}
  
  TCommentTypepost = Class(TGoogleBaseObject)
  Private
    Fid : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
  end;
  TCommentTypepostClass = Class of TCommentTypepost;
  
  { --------------------------------------------------------------------
    TComment
    --------------------------------------------------------------------}
  
  TComment = Class(TGoogleBaseObject)
  Private
    Fauthor : TCommentTypeauthor;
    Fblog : TCommentTypeblog;
    Fcontent : String;
    Fid : String;
    FinReplyTo : TCommentTypeinReplyTo;
    Fkind : String;
    Fpost : TCommentTypepost;
    F_published : TDatetime;
    FselfLink : String;
    Fstatus : String;
    Fupdated : TDatetime;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setauthor(AIndex : Integer; AValue : TCommentTypeauthor); virtual;
    Procedure Setblog(AIndex : Integer; AValue : TCommentTypeblog); virtual;
    Procedure Setcontent(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetinReplyTo(AIndex : Integer; AValue : TCommentTypeinReplyTo); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setpost(AIndex : Integer; AValue : TCommentTypepost); virtual;
    Procedure Set_published(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property author : TCommentTypeauthor Index 0 Read Fauthor Write Setauthor;
    Property blog : TCommentTypeblog Index 8 Read Fblog Write Setblog;
    Property content : String Index 16 Read Fcontent Write Setcontent;
    Property id : String Index 24 Read Fid Write Setid;
    Property inReplyTo : TCommentTypeinReplyTo Index 32 Read FinReplyTo Write SetinReplyTo;
    Property kind : String Index 40 Read Fkind Write Setkind;
    Property post : TCommentTypepost Index 48 Read Fpost Write Setpost;
    Property _published : TDatetime Index 56 Read F_published Write Set_published;
    Property selfLink : String Index 64 Read FselfLink Write SetselfLink;
    Property status : String Index 72 Read Fstatus Write Setstatus;
    Property updated : TDatetime Index 80 Read Fupdated Write Setupdated;
  end;
  TCommentClass = Class of TComment;
  
  { --------------------------------------------------------------------
    TCommentList
    --------------------------------------------------------------------}
  
  TCommentList = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fitems : TCommentListTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FprevPageToken : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TCommentListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetprevPageToken(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property items : TCommentListTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
    Property prevPageToken : String Index 32 Read FprevPageToken Write SetprevPageToken;
  end;
  TCommentListClass = Class of TCommentList;
  
  { --------------------------------------------------------------------
    TPageTypeauthorTypeimage
    --------------------------------------------------------------------}
  
  TPageTypeauthorTypeimage = Class(TGoogleBaseObject)
  Private
    Furl : String;
  Protected
    //Property setters
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property url : String Index 0 Read Furl Write Seturl;
  end;
  TPageTypeauthorTypeimageClass = Class of TPageTypeauthorTypeimage;
  
  { --------------------------------------------------------------------
    TPageTypeauthor
    --------------------------------------------------------------------}
  
  TPageTypeauthor = Class(TGoogleBaseObject)
  Private
    FdisplayName : String;
    Fid : String;
    Fimage : TPageTypeauthorTypeimage;
    Furl : String;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setimage(AIndex : Integer; AValue : TPageTypeauthorTypeimage); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property displayName : String Index 0 Read FdisplayName Write SetdisplayName;
    Property id : String Index 8 Read Fid Write Setid;
    Property image : TPageTypeauthorTypeimage Index 16 Read Fimage Write Setimage;
    Property url : String Index 24 Read Furl Write Seturl;
  end;
  TPageTypeauthorClass = Class of TPageTypeauthor;
  
  { --------------------------------------------------------------------
    TPageTypeblog
    --------------------------------------------------------------------}
  
  TPageTypeblog = Class(TGoogleBaseObject)
  Private
    Fid : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
  end;
  TPageTypeblogClass = Class of TPageTypeblog;
  
  { --------------------------------------------------------------------
    TPage
    --------------------------------------------------------------------}
  
  TPage = Class(TGoogleBaseObject)
  Private
    Fauthor : TPageTypeauthor;
    Fblog : TPageTypeblog;
    Fcontent : String;
    Fetag : String;
    Fid : String;
    Fkind : String;
    F_published : TDatetime;
    FselfLink : String;
    Fstatus : String;
    Ftitle : String;
    Fupdated : TDatetime;
    Furl : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setauthor(AIndex : Integer; AValue : TPageTypeauthor); virtual;
    Procedure Setblog(AIndex : Integer; AValue : TPageTypeblog); virtual;
    Procedure Setcontent(AIndex : Integer; AValue : String); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Set_published(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property author : TPageTypeauthor Index 0 Read Fauthor Write Setauthor;
    Property blog : TPageTypeblog Index 8 Read Fblog Write Setblog;
    Property content : String Index 16 Read Fcontent Write Setcontent;
    Property etag : String Index 24 Read Fetag Write Setetag;
    Property id : String Index 32 Read Fid Write Setid;
    Property kind : String Index 40 Read Fkind Write Setkind;
    Property _published : TDatetime Index 48 Read F_published Write Set_published;
    Property selfLink : String Index 56 Read FselfLink Write SetselfLink;
    Property status : String Index 64 Read Fstatus Write Setstatus;
    Property title : String Index 72 Read Ftitle Write Settitle;
    Property updated : TDatetime Index 80 Read Fupdated Write Setupdated;
    Property url : String Index 88 Read Furl Write Seturl;
  end;
  TPageClass = Class of TPage;
  
  { --------------------------------------------------------------------
    TPageList
    --------------------------------------------------------------------}
  
  TPageList = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fitems : TPageListTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TPageListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property items : TPageListTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TPageListClass = Class of TPageList;
  
  { --------------------------------------------------------------------
    TPageviewsTypecountsItem
    --------------------------------------------------------------------}
  
  TPageviewsTypecountsItem = Class(TGoogleBaseObject)
  Private
    Fcount : String;
    FtimeRange : String;
  Protected
    //Property setters
    Procedure Setcount(AIndex : Integer; AValue : String); virtual;
    Procedure SettimeRange(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property count : String Index 0 Read Fcount Write Setcount;
    Property timeRange : String Index 8 Read FtimeRange Write SettimeRange;
  end;
  TPageviewsTypecountsItemClass = Class of TPageviewsTypecountsItem;
  
  { --------------------------------------------------------------------
    TPageviews
    --------------------------------------------------------------------}
  
  TPageviews = Class(TGoogleBaseObject)
  Private
    FblogId : String;
    Fcounts : TPageviewsTypecountsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetblogId(AIndex : Integer; AValue : String); virtual;
    Procedure Setcounts(AIndex : Integer; AValue : TPageviewsTypecountsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property blogId : String Index 0 Read FblogId Write SetblogId;
    Property counts : TPageviewsTypecountsArray Index 8 Read Fcounts Write Setcounts;
    Property kind : String Index 16 Read Fkind Write Setkind;
  end;
  TPageviewsClass = Class of TPageviews;
  
  { --------------------------------------------------------------------
    TPostTypeauthorTypeimage
    --------------------------------------------------------------------}
  
  TPostTypeauthorTypeimage = Class(TGoogleBaseObject)
  Private
    Furl : String;
  Protected
    //Property setters
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property url : String Index 0 Read Furl Write Seturl;
  end;
  TPostTypeauthorTypeimageClass = Class of TPostTypeauthorTypeimage;
  
  { --------------------------------------------------------------------
    TPostTypeauthor
    --------------------------------------------------------------------}
  
  TPostTypeauthor = Class(TGoogleBaseObject)
  Private
    FdisplayName : String;
    Fid : String;
    Fimage : TPostTypeauthorTypeimage;
    Furl : String;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setimage(AIndex : Integer; AValue : TPostTypeauthorTypeimage); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property displayName : String Index 0 Read FdisplayName Write SetdisplayName;
    Property id : String Index 8 Read Fid Write Setid;
    Property image : TPostTypeauthorTypeimage Index 16 Read Fimage Write Setimage;
    Property url : String Index 24 Read Furl Write Seturl;
  end;
  TPostTypeauthorClass = Class of TPostTypeauthor;
  
  { --------------------------------------------------------------------
    TPostTypeblog
    --------------------------------------------------------------------}
  
  TPostTypeblog = Class(TGoogleBaseObject)
  Private
    Fid : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
  end;
  TPostTypeblogClass = Class of TPostTypeblog;
  
  { --------------------------------------------------------------------
    TPostTypeimagesItem
    --------------------------------------------------------------------}
  
  TPostTypeimagesItem = Class(TGoogleBaseObject)
  Private
    Furl : String;
  Protected
    //Property setters
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property url : String Index 0 Read Furl Write Seturl;
  end;
  TPostTypeimagesItemClass = Class of TPostTypeimagesItem;
  
  { --------------------------------------------------------------------
    TPostTypelocation
    --------------------------------------------------------------------}
  
  TPostTypelocation = Class(TGoogleBaseObject)
  Private
    Flat : double;
    Flng : double;
    Fname : String;
    Fspan : String;
  Protected
    //Property setters
    Procedure Setlat(AIndex : Integer; AValue : double); virtual;
    Procedure Setlng(AIndex : Integer; AValue : double); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setspan(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property lat : double Index 0 Read Flat Write Setlat;
    Property lng : double Index 8 Read Flng Write Setlng;
    Property name : String Index 16 Read Fname Write Setname;
    Property span : String Index 24 Read Fspan Write Setspan;
  end;
  TPostTypelocationClass = Class of TPostTypelocation;
  
  { --------------------------------------------------------------------
    TPostTypereplies
    --------------------------------------------------------------------}
  
  TPostTypereplies = Class(TGoogleBaseObject)
  Private
    Fitems : TPostTyperepliesTypeitemsArray;
    FselfLink : String;
    FtotalItems : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TPostTyperepliesTypeitemsArray); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TPostTyperepliesTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property selfLink : String Index 8 Read FselfLink Write SetselfLink;
    Property totalItems : String Index 16 Read FtotalItems Write SettotalItems;
  end;
  TPostTyperepliesClass = Class of TPostTypereplies;
  
  { --------------------------------------------------------------------
    TPost
    --------------------------------------------------------------------}
  
  TPost = Class(TGoogleBaseObject)
  Private
    Fauthor : TPostTypeauthor;
    Fblog : TPostTypeblog;
    Fcontent : String;
    FcustomMetaData : String;
    Fetag : String;
    Fid : String;
    Fimages : TPostTypeimagesArray;
    Fkind : String;
    Flabels : TStringArray;
    Flocation : TPostTypelocation;
    F_published : TDatetime;
    FreaderComments : String;
    Freplies : TPostTypereplies;
    FselfLink : String;
    Fstatus : String;
    Ftitle : String;
    FtitleLink : String;
    Fupdated : TDatetime;
    Furl : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setauthor(AIndex : Integer; AValue : TPostTypeauthor); virtual;
    Procedure Setblog(AIndex : Integer; AValue : TPostTypeblog); virtual;
    Procedure Setcontent(AIndex : Integer; AValue : String); virtual;
    Procedure SetcustomMetaData(AIndex : Integer; AValue : String); virtual;
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setimages(AIndex : Integer; AValue : TPostTypeimagesArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlabels(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : TPostTypelocation); virtual;
    Procedure Set_published(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetreaderComments(AIndex : Integer; AValue : String); virtual;
    Procedure Setreplies(AIndex : Integer; AValue : TPostTypereplies); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    Procedure SettitleLink(AIndex : Integer; AValue : String); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property author : TPostTypeauthor Index 0 Read Fauthor Write Setauthor;
    Property blog : TPostTypeblog Index 8 Read Fblog Write Setblog;
    Property content : String Index 16 Read Fcontent Write Setcontent;
    Property customMetaData : String Index 24 Read FcustomMetaData Write SetcustomMetaData;
    Property etag : String Index 32 Read Fetag Write Setetag;
    Property id : String Index 40 Read Fid Write Setid;
    Property images : TPostTypeimagesArray Index 48 Read Fimages Write Setimages;
    Property kind : String Index 56 Read Fkind Write Setkind;
    Property labels : TStringArray Index 64 Read Flabels Write Setlabels;
    Property location : TPostTypelocation Index 72 Read Flocation Write Setlocation;
    Property _published : TDatetime Index 80 Read F_published Write Set_published;
    Property readerComments : String Index 88 Read FreaderComments Write SetreaderComments;
    Property replies : TPostTypereplies Index 96 Read Freplies Write Setreplies;
    Property selfLink : String Index 104 Read FselfLink Write SetselfLink;
    Property status : String Index 112 Read Fstatus Write Setstatus;
    Property title : String Index 120 Read Ftitle Write Settitle;
    Property titleLink : String Index 128 Read FtitleLink Write SettitleLink;
    Property updated : TDatetime Index 136 Read Fupdated Write Setupdated;
    Property url : String Index 144 Read Furl Write Seturl;
  end;
  TPostClass = Class of TPost;
  
  { --------------------------------------------------------------------
    TPostList
    --------------------------------------------------------------------}
  
  TPostList = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fitems : TPostListTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TPostListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property items : TPostListTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TPostListClass = Class of TPostList;
  
  { --------------------------------------------------------------------
    TPostPerUserInfo
    --------------------------------------------------------------------}
  
  TPostPerUserInfo = Class(TGoogleBaseObject)
  Private
    FblogId : String;
    FhasEditAccess : boolean;
    Fkind : String;
    FpostId : String;
    FuserId : String;
  Protected
    //Property setters
    Procedure SetblogId(AIndex : Integer; AValue : String); virtual;
    Procedure SethasEditAccess(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetpostId(AIndex : Integer; AValue : String); virtual;
    Procedure SetuserId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property blogId : String Index 0 Read FblogId Write SetblogId;
    Property hasEditAccess : boolean Index 8 Read FhasEditAccess Write SethasEditAccess;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property postId : String Index 24 Read FpostId Write SetpostId;
    Property userId : String Index 32 Read FuserId Write SetuserId;
  end;
  TPostPerUserInfoClass = Class of TPostPerUserInfo;
  
  { --------------------------------------------------------------------
    TPostUserInfo
    --------------------------------------------------------------------}
  
  TPostUserInfo = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fpost : TPost;
    Fpost_user_info : TPostPerUserInfo;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setpost(AIndex : Integer; AValue : TPost); virtual;
    Procedure Setpost_user_info(AIndex : Integer; AValue : TPostPerUserInfo); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property post : TPost Index 8 Read Fpost Write Setpost;
    Property post_user_info : TPostPerUserInfo Index 16 Read Fpost_user_info Write Setpost_user_info;
  end;
  TPostUserInfoClass = Class of TPostUserInfo;
  
  { --------------------------------------------------------------------
    TPostUserInfosList
    --------------------------------------------------------------------}
  
  TPostUserInfosList = Class(TGoogleBaseObject)
  Private
    Fitems : TPostUserInfosListTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TPostUserInfosListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TPostUserInfosListTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TPostUserInfosListClass = Class of TPostUserInfosList;
  
  { --------------------------------------------------------------------
    TUserTypeblogs
    --------------------------------------------------------------------}
  
  TUserTypeblogs = Class(TGoogleBaseObject)
  Private
    FselfLink : String;
  Protected
    //Property setters
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property selfLink : String Index 0 Read FselfLink Write SetselfLink;
  end;
  TUserTypeblogsClass = Class of TUserTypeblogs;
  
  { --------------------------------------------------------------------
    TUserTypelocale
    --------------------------------------------------------------------}
  
  TUserTypelocale = Class(TGoogleBaseObject)
  Private
    Fcountry : String;
    Flanguage : String;
    Fvariant : String;
  Protected
    //Property setters
    Procedure Setcountry(AIndex : Integer; AValue : String); virtual;
    Procedure Setlanguage(AIndex : Integer; AValue : String); virtual;
    Procedure Setvariant(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property country : String Index 0 Read Fcountry Write Setcountry;
    Property language : String Index 8 Read Flanguage Write Setlanguage;
    Property variant : String Index 16 Read Fvariant Write Setvariant;
  end;
  TUserTypelocaleClass = Class of TUserTypelocale;
  
  { --------------------------------------------------------------------
    TUser
    --------------------------------------------------------------------}
  
  TUser = Class(TGoogleBaseObject)
  Private
    Fabout : String;
    Fblogs : TUserTypeblogs;
    Fcreated : TDatetime;
    FdisplayName : String;
    Fid : String;
    Fkind : String;
    Flocale : TUserTypelocale;
    FselfLink : String;
    Furl : String;
  Protected
    //Property setters
    Procedure Setabout(AIndex : Integer; AValue : String); virtual;
    Procedure Setblogs(AIndex : Integer; AValue : TUserTypeblogs); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetdisplayName(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlocale(AIndex : Integer; AValue : TUserTypelocale); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property about : String Index 0 Read Fabout Write Setabout;
    Property blogs : TUserTypeblogs Index 8 Read Fblogs Write Setblogs;
    Property created : TDatetime Index 16 Read Fcreated Write Setcreated;
    Property displayName : String Index 24 Read FdisplayName Write SetdisplayName;
    Property id : String Index 32 Read Fid Write Setid;
    Property kind : String Index 40 Read Fkind Write Setkind;
    Property locale : TUserTypelocale Index 48 Read Flocale Write Setlocale;
    Property selfLink : String Index 56 Read FselfLink Write SetselfLink;
    Property url : String Index 64 Read Furl Write Seturl;
  end;
  TUserClass = Class of TUser;
  
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
    view : String;
  end;
  
  
  //Optional query Options for TBlogsResource, method GetByUrl
  
  TBlogsGetByUrlOptions = Record
    url : String;
    view : String;
  end;
  
  
  //Optional query Options for TBlogsResource, method ListByUser
  
  TBlogsListByUserOptions = Record
    fetchUserInfo : boolean;
    role : String;
    status : String;
    view : String;
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
    view : String;
  end;
  
  
  //Optional query Options for TCommentsResource, method List
  
  TCommentsListOptions = Record
    endDate : TDatetime;
    fetchBodies : boolean;
    maxResults : integer;
    pageToken : String;
    startDate : TDatetime;
    status : String;
    view : String;
  end;
  
  
  //Optional query Options for TCommentsResource, method ListByBlog
  
  TCommentsListByBlogOptions = Record
    endDate : TDatetime;
    fetchBodies : boolean;
    maxResults : integer;
    pageToken : String;
    startDate : TDatetime;
    status : String;
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
    range : String;
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
    view : String;
  end;
  
  
  //Optional query Options for TPagesResource, method Insert
  
  TPagesInsertOptions = Record
    isDraft : boolean;
  end;
  
  
  //Optional query Options for TPagesResource, method List
  
  TPagesListOptions = Record
    fetchBodies : boolean;
    maxResults : integer;
    pageToken : String;
    status : String;
    view : String;
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
    labels : String;
    maxResults : integer;
    orderBy : String;
    pageToken : String;
    startDate : TDatetime;
    status : String;
    view : String;
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
    view : String;
  end;
  
  
  //Optional query Options for TPostsResource, method GetByPath
  
  TPostsGetByPathOptions = Record
    maxComments : integer;
    path : String;
    view : String;
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
    labels : String;
    maxResults : integer;
    orderBy : String;
    pageToken : String;
    startDate : TDatetime;
    status : String;
    view : String;
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
    orderBy : String;
    q : String;
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
  TBlogTypelocale
  --------------------------------------------------------------------}


Procedure TBlogTypelocale.Setcountry(AIndex : Integer; AValue : String); 

begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlogTypelocale.Setlanguage(AIndex : Integer; AValue : String); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlogTypelocale.Setvariant(AIndex : Integer; AValue : String); 

begin
  If (Fvariant=AValue) then exit;
  Fvariant:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBlogTypepages
  --------------------------------------------------------------------}


Procedure TBlogTypepages.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlogTypepages.SettotalItems(AIndex : Integer; AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBlogTypeposts
  --------------------------------------------------------------------}


Procedure TBlogTypeposts.Setitems(AIndex : Integer; AValue : TBlogTypepostsTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlogTypeposts.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlogTypeposts.SettotalItems(AIndex : Integer; AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBlogTypeposts.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBlog
  --------------------------------------------------------------------}


Procedure TBlog.SetcustomMetaData(AIndex : Integer; AValue : String); 

begin
  If (FcustomMetaData=AValue) then exit;
  FcustomMetaData:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlog.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlog.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlog.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlog.Setlocale(AIndex : Integer; AValue : TBlogTypelocale); 

begin
  If (Flocale=AValue) then exit;
  Flocale:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlog.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlog.Setpages(AIndex : Integer; AValue : TBlogTypepages); 

begin
  If (Fpages=AValue) then exit;
  Fpages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlog.Setposts(AIndex : Integer; AValue : TBlogTypeposts); 

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



Procedure TBlog.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlog.Setstatus(AIndex : Integer; AValue : String); 

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



Procedure TBlog.Seturl(AIndex : Integer; AValue : String); 

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
  TBlogList
  --------------------------------------------------------------------}


Procedure TBlogList.SetblogUserInfos(AIndex : Integer; AValue : TBlogListTypeblogUserInfosArray); 

begin
  If (FblogUserInfos=AValue) then exit;
  FblogUserInfos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlogList.Setitems(AIndex : Integer; AValue : TBlogListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlogList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBlogList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'bloguserinfos' : SetLength(FblogUserInfos,ALength);
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBlogPerUserInfo
  --------------------------------------------------------------------}


Procedure TBlogPerUserInfo.SetblogId(AIndex : Integer; AValue : String); 

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



Procedure TBlogPerUserInfo.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlogPerUserInfo.SetphotosAlbumKey(AIndex : Integer; AValue : String); 

begin
  If (FphotosAlbumKey=AValue) then exit;
  FphotosAlbumKey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlogPerUserInfo.Setrole(AIndex : Integer; AValue : String); 

begin
  If (Frole=AValue) then exit;
  Frole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBlogPerUserInfo.SetuserId(AIndex : Integer; AValue : String); 

begin
  If (FuserId=AValue) then exit;
  FuserId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBlogUserInfo
  --------------------------------------------------------------------}


Procedure TBlogUserInfo.Setblog(AIndex : Integer; AValue : TBlog); 

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



Procedure TBlogUserInfo.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentTypeauthorTypeimage
  --------------------------------------------------------------------}


Procedure TCommentTypeauthorTypeimage.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentTypeauthor
  --------------------------------------------------------------------}


Procedure TCommentTypeauthor.SetdisplayName(AIndex : Integer; AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentTypeauthor.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentTypeauthor.Setimage(AIndex : Integer; AValue : TCommentTypeauthorTypeimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentTypeauthor.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentTypeblog
  --------------------------------------------------------------------}


Procedure TCommentTypeblog.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentTypeinReplyTo
  --------------------------------------------------------------------}


Procedure TCommentTypeinReplyTo.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommentTypepost
  --------------------------------------------------------------------}


Procedure TCommentTypepost.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TComment
  --------------------------------------------------------------------}


Procedure TComment.Setauthor(AIndex : Integer; AValue : TCommentTypeauthor); 

begin
  If (Fauthor=AValue) then exit;
  Fauthor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setblog(AIndex : Integer; AValue : TCommentTypeblog); 

begin
  If (Fblog=AValue) then exit;
  Fblog:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setcontent(AIndex : Integer; AValue : String); 

begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.SetinReplyTo(AIndex : Integer; AValue : TCommentTypeinReplyTo); 

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



Procedure TComment.Setpost(AIndex : Integer; AValue : TCommentTypepost); 

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



Procedure TComment.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComment.Setstatus(AIndex : Integer; AValue : String); 

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
  TCommentList
  --------------------------------------------------------------------}


Procedure TCommentList.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentList.Setitems(AIndex : Integer; AValue : TCommentListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentList.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommentList.SetprevPageToken(AIndex : Integer; AValue : String); 

begin
  If (FprevPageToken=AValue) then exit;
  FprevPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCommentList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPageTypeauthorTypeimage
  --------------------------------------------------------------------}


Procedure TPageTypeauthorTypeimage.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPageTypeauthor
  --------------------------------------------------------------------}


Procedure TPageTypeauthor.SetdisplayName(AIndex : Integer; AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPageTypeauthor.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPageTypeauthor.Setimage(AIndex : Integer; AValue : TPageTypeauthorTypeimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPageTypeauthor.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPageTypeblog
  --------------------------------------------------------------------}


Procedure TPageTypeblog.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPage
  --------------------------------------------------------------------}


Procedure TPage.Setauthor(AIndex : Integer; AValue : TPageTypeauthor); 

begin
  If (Fauthor=AValue) then exit;
  Fauthor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPage.Setblog(AIndex : Integer; AValue : TPageTypeblog); 

begin
  If (Fblog=AValue) then exit;
  Fblog:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPage.Setcontent(AIndex : Integer; AValue : String); 

begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPage.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPage.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPage.Setkind(AIndex : Integer; AValue : String); 

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



Procedure TPage.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPage.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPage.Settitle(AIndex : Integer; AValue : String); 

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



Procedure TPage.Seturl(AIndex : Integer; AValue : String); 

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
  TPageList
  --------------------------------------------------------------------}


Procedure TPageList.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPageList.Setitems(AIndex : Integer; AValue : TPageListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPageList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPageList.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPageList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPageviewsTypecountsItem
  --------------------------------------------------------------------}


Procedure TPageviewsTypecountsItem.Setcount(AIndex : Integer; AValue : String); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPageviewsTypecountsItem.SettimeRange(AIndex : Integer; AValue : String); 

begin
  If (FtimeRange=AValue) then exit;
  FtimeRange:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPageviews
  --------------------------------------------------------------------}


Procedure TPageviews.SetblogId(AIndex : Integer; AValue : String); 

begin
  If (FblogId=AValue) then exit;
  FblogId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPageviews.Setcounts(AIndex : Integer; AValue : TPageviewsTypecountsArray); 

begin
  If (Fcounts=AValue) then exit;
  Fcounts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPageviews.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPageviews.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'counts' : SetLength(Fcounts,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPostTypeauthorTypeimage
  --------------------------------------------------------------------}


Procedure TPostTypeauthorTypeimage.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPostTypeauthor
  --------------------------------------------------------------------}


Procedure TPostTypeauthor.SetdisplayName(AIndex : Integer; AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostTypeauthor.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostTypeauthor.Setimage(AIndex : Integer; AValue : TPostTypeauthorTypeimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostTypeauthor.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPostTypeblog
  --------------------------------------------------------------------}


Procedure TPostTypeblog.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPostTypeimagesItem
  --------------------------------------------------------------------}


Procedure TPostTypeimagesItem.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPostTypelocation
  --------------------------------------------------------------------}


Procedure TPostTypelocation.Setlat(AIndex : Integer; AValue : double); 

begin
  If (Flat=AValue) then exit;
  Flat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostTypelocation.Setlng(AIndex : Integer; AValue : double); 

begin
  If (Flng=AValue) then exit;
  Flng:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostTypelocation.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostTypelocation.Setspan(AIndex : Integer; AValue : String); 

begin
  If (Fspan=AValue) then exit;
  Fspan:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPostTypereplies
  --------------------------------------------------------------------}


Procedure TPostTypereplies.Setitems(AIndex : Integer; AValue : TPostTyperepliesTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostTypereplies.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostTypereplies.SettotalItems(AIndex : Integer; AValue : String); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPostTypereplies.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPost
  --------------------------------------------------------------------}


Procedure TPost.Setauthor(AIndex : Integer; AValue : TPostTypeauthor); 

begin
  If (Fauthor=AValue) then exit;
  Fauthor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.Setblog(AIndex : Integer; AValue : TPostTypeblog); 

begin
  If (Fblog=AValue) then exit;
  Fblog:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.Setcontent(AIndex : Integer; AValue : String); 

begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.SetcustomMetaData(AIndex : Integer; AValue : String); 

begin
  If (FcustomMetaData=AValue) then exit;
  FcustomMetaData:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.Setimages(AIndex : Integer; AValue : TPostTypeimagesArray); 

begin
  If (Fimages=AValue) then exit;
  Fimages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.Setlabels(AIndex : Integer; AValue : TStringArray); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.Setlocation(AIndex : Integer; AValue : TPostTypelocation); 

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



Procedure TPost.SetreaderComments(AIndex : Integer; AValue : String); 

begin
  If (FreaderComments=AValue) then exit;
  FreaderComments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.Setreplies(AIndex : Integer; AValue : TPostTypereplies); 

begin
  If (Freplies=AValue) then exit;
  Freplies:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPost.SettitleLink(AIndex : Integer; AValue : String); 

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



Procedure TPost.Seturl(AIndex : Integer; AValue : String); 

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

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPost.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'images' : SetLength(Fimages,ALength);
  'labels' : SetLength(Flabels,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPostList
  --------------------------------------------------------------------}


Procedure TPostList.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostList.Setitems(AIndex : Integer; AValue : TPostListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostList.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPostList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPostPerUserInfo
  --------------------------------------------------------------------}


Procedure TPostPerUserInfo.SetblogId(AIndex : Integer; AValue : String); 

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



Procedure TPostPerUserInfo.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostPerUserInfo.SetpostId(AIndex : Integer; AValue : String); 

begin
  If (FpostId=AValue) then exit;
  FpostId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostPerUserInfo.SetuserId(AIndex : Integer; AValue : String); 

begin
  If (FuserId=AValue) then exit;
  FuserId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPostUserInfo
  --------------------------------------------------------------------}


Procedure TPostUserInfo.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostUserInfo.Setpost(AIndex : Integer; AValue : TPost); 

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


Procedure TPostUserInfosList.Setitems(AIndex : Integer; AValue : TPostUserInfosListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostUserInfosList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPostUserInfosList.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPostUserInfosList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TUserTypeblogs
  --------------------------------------------------------------------}


Procedure TUserTypeblogs.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUserTypelocale
  --------------------------------------------------------------------}


Procedure TUserTypelocale.Setcountry(AIndex : Integer; AValue : String); 

begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserTypelocale.Setlanguage(AIndex : Integer; AValue : String); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserTypelocale.Setvariant(AIndex : Integer; AValue : String); 

begin
  If (Fvariant=AValue) then exit;
  Fvariant:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUser
  --------------------------------------------------------------------}


Procedure TUser.Setabout(AIndex : Integer; AValue : String); 

begin
  If (Fabout=AValue) then exit;
  Fabout:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setblogs(AIndex : Integer; AValue : TUserTypeblogs); 

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



Procedure TUser.SetdisplayName(AIndex : Integer; AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setlocale(AIndex : Integer; AValue : TUserTypelocale); 

begin
  If (Flocale=AValue) then exit;
  Flocale:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
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
  Result:='https://www.googleapis.com:443/';
end;

Class Function TBloggerAPI.APIbasePath : string;

begin
  Result:='/blogger/v3/';
end;

Class Function TBloggerAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/blogger/v3/';
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
  TBlogTypelocale.RegisterObject;
  TBlogTypepages.RegisterObject;
  TBlogTypeposts.RegisterObject;
  TBlog.RegisterObject;
  TBlogList.RegisterObject;
  TBlogPerUserInfo.RegisterObject;
  TBlogUserInfo.RegisterObject;
  TCommentTypeauthorTypeimage.RegisterObject;
  TCommentTypeauthor.RegisterObject;
  TCommentTypeblog.RegisterObject;
  TCommentTypeinReplyTo.RegisterObject;
  TCommentTypepost.RegisterObject;
  TComment.RegisterObject;
  TCommentList.RegisterObject;
  TPageTypeauthorTypeimage.RegisterObject;
  TPageTypeauthor.RegisterObject;
  TPageTypeblog.RegisterObject;
  TPage.RegisterObject;
  TPageList.RegisterObject;
  TPageviewsTypecountsItem.RegisterObject;
  TPageviews.RegisterObject;
  TPostTypeauthorTypeimage.RegisterObject;
  TPostTypeauthor.RegisterObject;
  TPostTypeblog.RegisterObject;
  TPostTypeimagesItem.RegisterObject;
  TPostTypelocation.RegisterObject;
  TPostTypereplies.RegisterObject;
  TPost.RegisterObject;
  TPostList.RegisterObject;
  TPostPerUserInfo.RegisterObject;
  TPostUserInfo.RegisterObject;
  TPostUserInfosList.RegisterObject;
  TUserTypeblogs.RegisterObject;
  TUserTypelocale.RegisterObject;
  TUser.RegisterObject;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
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
  Result.API:=Self.API;
end;



initialization
  TBloggerAPI.RegisterAPI;
end.
