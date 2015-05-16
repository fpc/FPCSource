unit googlegmail;
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
//Generated on: 16-5-15 08:53:04
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TDraft = Class;
  THistory = Class;
  THistoryLabelAdded = Class;
  THistoryLabelRemoved = Class;
  THistoryMessageAdded = Class;
  THistoryMessageDeleted = Class;
  TLabel = Class;
  TListDraftsResponse = Class;
  TListHistoryResponse = Class;
  TListLabelsResponse = Class;
  TListMessagesResponse = Class;
  TListThreadsResponse = Class;
  TMessage = Class;
  TMessagePart = Class;
  TMessagePartBody = Class;
  TMessagePartHeader = Class;
  TModifyMessageRequest = Class;
  TModifyThreadRequest = Class;
  TProfile = Class;
  TThread = Class;
  TDraftArray = Array of TDraft;
  THistoryArray = Array of THistory;
  THistoryLabelAddedArray = Array of THistoryLabelAdded;
  THistoryLabelRemovedArray = Array of THistoryLabelRemoved;
  THistoryMessageAddedArray = Array of THistoryMessageAdded;
  THistoryMessageDeletedArray = Array of THistoryMessageDeleted;
  TLabelArray = Array of TLabel;
  TListDraftsResponseArray = Array of TListDraftsResponse;
  TListHistoryResponseArray = Array of TListHistoryResponse;
  TListLabelsResponseArray = Array of TListLabelsResponse;
  TListMessagesResponseArray = Array of TListMessagesResponse;
  TListThreadsResponseArray = Array of TListThreadsResponse;
  TMessageArray = Array of TMessage;
  TMessagePartArray = Array of TMessagePart;
  TMessagePartBodyArray = Array of TMessagePartBody;
  TMessagePartHeaderArray = Array of TMessagePartHeader;
  TModifyMessageRequestArray = Array of TModifyMessageRequest;
  TModifyThreadRequestArray = Array of TModifyThreadRequest;
  TProfileArray = Array of TProfile;
  TThreadArray = Array of TThread;
  //Anonymous types, using auto-generated names
  THistoryTypelabelsAddedArray = Array of THistoryLabelAdded;
  THistoryTypelabelsRemovedArray = Array of THistoryLabelRemoved;
  THistoryTypemessagesArray = Array of TMessage;
  THistoryTypemessagesAddedArray = Array of THistoryMessageAdded;
  THistoryTypemessagesDeletedArray = Array of THistoryMessageDeleted;
  TListDraftsResponseTypedraftsArray = Array of TDraft;
  TListHistoryResponseTypehistoryArray = Array of THistory;
  TListLabelsResponseTypelabelsArray = Array of TLabel;
  TListMessagesResponseTypemessagesArray = Array of TMessage;
  TListThreadsResponseTypethreadsArray = Array of TThread;
  TMessagePartTypeheadersArray = Array of TMessagePartHeader;
  TMessagePartTypepartsArray = Array of TMessagePart;
  TThreadTypemessagesArray = Array of TMessage;
  
  { --------------------------------------------------------------------
    TDraft
    --------------------------------------------------------------------}
  
  TDraft = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fmessage : TMessage;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : TMessage); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property message : TMessage Index 8 Read Fmessage Write Setmessage;
  end;
  TDraftClass = Class of TDraft;
  
  { --------------------------------------------------------------------
    THistory
    --------------------------------------------------------------------}
  
  THistory = Class(TGoogleBaseObject)
  Private
    Fid : String;
    FlabelsAdded : THistoryTypelabelsAddedArray;
    FlabelsRemoved : THistoryTypelabelsRemovedArray;
    Fmessages : THistoryTypemessagesArray;
    FmessagesAdded : THistoryTypemessagesAddedArray;
    FmessagesDeleted : THistoryTypemessagesDeletedArray;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetlabelsAdded(AIndex : Integer; AValue : THistoryTypelabelsAddedArray); virtual;
    Procedure SetlabelsRemoved(AIndex : Integer; AValue : THistoryTypelabelsRemovedArray); virtual;
    Procedure Setmessages(AIndex : Integer; AValue : THistoryTypemessagesArray); virtual;
    Procedure SetmessagesAdded(AIndex : Integer; AValue : THistoryTypemessagesAddedArray); virtual;
    Procedure SetmessagesDeleted(AIndex : Integer; AValue : THistoryTypemessagesDeletedArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property labelsAdded : THistoryTypelabelsAddedArray Index 8 Read FlabelsAdded Write SetlabelsAdded;
    Property labelsRemoved : THistoryTypelabelsRemovedArray Index 16 Read FlabelsRemoved Write SetlabelsRemoved;
    Property messages : THistoryTypemessagesArray Index 24 Read Fmessages Write Setmessages;
    Property messagesAdded : THistoryTypemessagesAddedArray Index 32 Read FmessagesAdded Write SetmessagesAdded;
    Property messagesDeleted : THistoryTypemessagesDeletedArray Index 40 Read FmessagesDeleted Write SetmessagesDeleted;
  end;
  THistoryClass = Class of THistory;
  
  { --------------------------------------------------------------------
    THistoryLabelAdded
    --------------------------------------------------------------------}
  
  THistoryLabelAdded = Class(TGoogleBaseObject)
  Private
    FlabelIds : TStringArray;
    Fmessage : TMessage;
  Protected
    //Property setters
    Procedure SetlabelIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : TMessage); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property labelIds : TStringArray Index 0 Read FlabelIds Write SetlabelIds;
    Property message : TMessage Index 8 Read Fmessage Write Setmessage;
  end;
  THistoryLabelAddedClass = Class of THistoryLabelAdded;
  
  { --------------------------------------------------------------------
    THistoryLabelRemoved
    --------------------------------------------------------------------}
  
  THistoryLabelRemoved = Class(TGoogleBaseObject)
  Private
    FlabelIds : TStringArray;
    Fmessage : TMessage;
  Protected
    //Property setters
    Procedure SetlabelIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : TMessage); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property labelIds : TStringArray Index 0 Read FlabelIds Write SetlabelIds;
    Property message : TMessage Index 8 Read Fmessage Write Setmessage;
  end;
  THistoryLabelRemovedClass = Class of THistoryLabelRemoved;
  
  { --------------------------------------------------------------------
    THistoryMessageAdded
    --------------------------------------------------------------------}
  
  THistoryMessageAdded = Class(TGoogleBaseObject)
  Private
    Fmessage : TMessage;
  Protected
    //Property setters
    Procedure Setmessage(AIndex : Integer; AValue : TMessage); virtual;
  Public
  Published
    Property message : TMessage Index 0 Read Fmessage Write Setmessage;
  end;
  THistoryMessageAddedClass = Class of THistoryMessageAdded;
  
  { --------------------------------------------------------------------
    THistoryMessageDeleted
    --------------------------------------------------------------------}
  
  THistoryMessageDeleted = Class(TGoogleBaseObject)
  Private
    Fmessage : TMessage;
  Protected
    //Property setters
    Procedure Setmessage(AIndex : Integer; AValue : TMessage); virtual;
  Public
  Published
    Property message : TMessage Index 0 Read Fmessage Write Setmessage;
  end;
  THistoryMessageDeletedClass = Class of THistoryMessageDeleted;
  
  { --------------------------------------------------------------------
    TLabel
    --------------------------------------------------------------------}
  
  TLabel = Class(TGoogleBaseObject)
  Private
    Fid : String;
    FlabelListVisibility : String;
    FmessageListVisibility : String;
    FmessagesTotal : integer;
    FmessagesUnread : integer;
    Fname : String;
    FthreadsTotal : integer;
    FthreadsUnread : integer;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetlabelListVisibility(AIndex : Integer; AValue : String); virtual;
    Procedure SetmessageListVisibility(AIndex : Integer; AValue : String); virtual;
    Procedure SetmessagesTotal(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmessagesUnread(AIndex : Integer; AValue : integer); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetthreadsTotal(AIndex : Integer; AValue : integer); virtual;
    Procedure SetthreadsUnread(AIndex : Integer; AValue : integer); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property labelListVisibility : String Index 8 Read FlabelListVisibility Write SetlabelListVisibility;
    Property messageListVisibility : String Index 16 Read FmessageListVisibility Write SetmessageListVisibility;
    Property messagesTotal : integer Index 24 Read FmessagesTotal Write SetmessagesTotal;
    Property messagesUnread : integer Index 32 Read FmessagesUnread Write SetmessagesUnread;
    Property name : String Index 40 Read Fname Write Setname;
    Property threadsTotal : integer Index 48 Read FthreadsTotal Write SetthreadsTotal;
    Property threadsUnread : integer Index 56 Read FthreadsUnread Write SetthreadsUnread;
    Property _type : String Index 64 Read F_type Write Set_type;
  end;
  TLabelClass = Class of TLabel;
  
  { --------------------------------------------------------------------
    TListDraftsResponse
    --------------------------------------------------------------------}
  
  TListDraftsResponse = Class(TGoogleBaseObject)
  Private
    Fdrafts : TListDraftsResponseTypedraftsArray;
    FnextPageToken : String;
    FresultSizeEstimate : integer;
  Protected
    //Property setters
    Procedure Setdrafts(AIndex : Integer; AValue : TListDraftsResponseTypedraftsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetresultSizeEstimate(AIndex : Integer; AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property drafts : TListDraftsResponseTypedraftsArray Index 0 Read Fdrafts Write Setdrafts;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property resultSizeEstimate : integer Index 16 Read FresultSizeEstimate Write SetresultSizeEstimate;
  end;
  TListDraftsResponseClass = Class of TListDraftsResponse;
  
  { --------------------------------------------------------------------
    TListHistoryResponse
    --------------------------------------------------------------------}
  
  TListHistoryResponse = Class(TGoogleBaseObject)
  Private
    Fhistory : TListHistoryResponseTypehistoryArray;
    FhistoryId : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Sethistory(AIndex : Integer; AValue : TListHistoryResponseTypehistoryArray); virtual;
    Procedure SethistoryId(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property history : TListHistoryResponseTypehistoryArray Index 0 Read Fhistory Write Sethistory;
    Property historyId : String Index 8 Read FhistoryId Write SethistoryId;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TListHistoryResponseClass = Class of TListHistoryResponse;
  
  { --------------------------------------------------------------------
    TListLabelsResponse
    --------------------------------------------------------------------}
  
  TListLabelsResponse = Class(TGoogleBaseObject)
  Private
    Flabels : TListLabelsResponseTypelabelsArray;
  Protected
    //Property setters
    Procedure Setlabels(AIndex : Integer; AValue : TListLabelsResponseTypelabelsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property labels : TListLabelsResponseTypelabelsArray Index 0 Read Flabels Write Setlabels;
  end;
  TListLabelsResponseClass = Class of TListLabelsResponse;
  
  { --------------------------------------------------------------------
    TListMessagesResponse
    --------------------------------------------------------------------}
  
  TListMessagesResponse = Class(TGoogleBaseObject)
  Private
    Fmessages : TListMessagesResponseTypemessagesArray;
    FnextPageToken : String;
    FresultSizeEstimate : integer;
  Protected
    //Property setters
    Procedure Setmessages(AIndex : Integer; AValue : TListMessagesResponseTypemessagesArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetresultSizeEstimate(AIndex : Integer; AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property messages : TListMessagesResponseTypemessagesArray Index 0 Read Fmessages Write Setmessages;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property resultSizeEstimate : integer Index 16 Read FresultSizeEstimate Write SetresultSizeEstimate;
  end;
  TListMessagesResponseClass = Class of TListMessagesResponse;
  
  { --------------------------------------------------------------------
    TListThreadsResponse
    --------------------------------------------------------------------}
  
  TListThreadsResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    FresultSizeEstimate : integer;
    Fthreads : TListThreadsResponseTypethreadsArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetresultSizeEstimate(AIndex : Integer; AValue : integer); virtual;
    Procedure Setthreads(AIndex : Integer; AValue : TListThreadsResponseTypethreadsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property resultSizeEstimate : integer Index 8 Read FresultSizeEstimate Write SetresultSizeEstimate;
    Property threads : TListThreadsResponseTypethreadsArray Index 16 Read Fthreads Write Setthreads;
  end;
  TListThreadsResponseClass = Class of TListThreadsResponse;
  
  { --------------------------------------------------------------------
    TMessage
    --------------------------------------------------------------------}
  
  TMessage = Class(TGoogleBaseObject)
  Private
    FhistoryId : String;
    Fid : String;
    FlabelIds : TStringArray;
    Fpayload : TMessagePart;
    Fraw : String;
    FsizeEstimate : integer;
    Fsnippet : String;
    FthreadId : String;
  Protected
    //Property setters
    Procedure SethistoryId(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetlabelIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setpayload(AIndex : Integer; AValue : TMessagePart); virtual;
    Procedure Setraw(AIndex : Integer; AValue : String); virtual;
    Procedure SetsizeEstimate(AIndex : Integer; AValue : integer); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : String); virtual;
    Procedure SetthreadId(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property historyId : String Index 0 Read FhistoryId Write SethistoryId;
    Property id : String Index 8 Read Fid Write Setid;
    Property labelIds : TStringArray Index 16 Read FlabelIds Write SetlabelIds;
    Property payload : TMessagePart Index 24 Read Fpayload Write Setpayload;
    Property raw : String Index 32 Read Fraw Write Setraw;
    Property sizeEstimate : integer Index 40 Read FsizeEstimate Write SetsizeEstimate;
    Property snippet : String Index 48 Read Fsnippet Write Setsnippet;
    Property threadId : String Index 56 Read FthreadId Write SetthreadId;
  end;
  TMessageClass = Class of TMessage;
  
  { --------------------------------------------------------------------
    TMessagePart
    --------------------------------------------------------------------}
  
  TMessagePart = Class(TGoogleBaseObject)
  Private
    Fbody : TMessagePartBody;
    Ffilename : String;
    Fheaders : TMessagePartTypeheadersArray;
    FmimeType : String;
    FpartId : String;
    Fparts : TMessagePartTypepartsArray;
  Protected
    //Property setters
    Procedure Setbody(AIndex : Integer; AValue : TMessagePartBody); virtual;
    Procedure Setfilename(AIndex : Integer; AValue : String); virtual;
    Procedure Setheaders(AIndex : Integer; AValue : TMessagePartTypeheadersArray); virtual;
    Procedure SetmimeType(AIndex : Integer; AValue : String); virtual;
    Procedure SetpartId(AIndex : Integer; AValue : String); virtual;
    Procedure Setparts(AIndex : Integer; AValue : TMessagePartTypepartsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property body : TMessagePartBody Index 0 Read Fbody Write Setbody;
    Property filename : String Index 8 Read Ffilename Write Setfilename;
    Property headers : TMessagePartTypeheadersArray Index 16 Read Fheaders Write Setheaders;
    Property mimeType : String Index 24 Read FmimeType Write SetmimeType;
    Property partId : String Index 32 Read FpartId Write SetpartId;
    Property parts : TMessagePartTypepartsArray Index 40 Read Fparts Write Setparts;
  end;
  TMessagePartClass = Class of TMessagePart;
  
  { --------------------------------------------------------------------
    TMessagePartBody
    --------------------------------------------------------------------}
  
  TMessagePartBody = Class(TGoogleBaseObject)
  Private
    FattachmentId : String;
    Fdata : String;
    Fsize : integer;
  Protected
    //Property setters
    Procedure SetattachmentId(AIndex : Integer; AValue : String); virtual;
    Procedure Setdata(AIndex : Integer; AValue : String); virtual;
    Procedure Setsize(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property attachmentId : String Index 0 Read FattachmentId Write SetattachmentId;
    Property data : String Index 8 Read Fdata Write Setdata;
    Property size : integer Index 16 Read Fsize Write Setsize;
  end;
  TMessagePartBodyClass = Class of TMessagePartBody;
  
  { --------------------------------------------------------------------
    TMessagePartHeader
    --------------------------------------------------------------------}
  
  TMessagePartHeader = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TMessagePartHeaderClass = Class of TMessagePartHeader;
  
  { --------------------------------------------------------------------
    TModifyMessageRequest
    --------------------------------------------------------------------}
  
  TModifyMessageRequest = Class(TGoogleBaseObject)
  Private
    FaddLabelIds : TStringArray;
    FremoveLabelIds : TStringArray;
  Protected
    //Property setters
    Procedure SetaddLabelIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetremoveLabelIds(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property addLabelIds : TStringArray Index 0 Read FaddLabelIds Write SetaddLabelIds;
    Property removeLabelIds : TStringArray Index 8 Read FremoveLabelIds Write SetremoveLabelIds;
  end;
  TModifyMessageRequestClass = Class of TModifyMessageRequest;
  
  { --------------------------------------------------------------------
    TModifyThreadRequest
    --------------------------------------------------------------------}
  
  TModifyThreadRequest = Class(TGoogleBaseObject)
  Private
    FaddLabelIds : TStringArray;
    FremoveLabelIds : TStringArray;
  Protected
    //Property setters
    Procedure SetaddLabelIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetremoveLabelIds(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property addLabelIds : TStringArray Index 0 Read FaddLabelIds Write SetaddLabelIds;
    Property removeLabelIds : TStringArray Index 8 Read FremoveLabelIds Write SetremoveLabelIds;
  end;
  TModifyThreadRequestClass = Class of TModifyThreadRequest;
  
  { --------------------------------------------------------------------
    TProfile
    --------------------------------------------------------------------}
  
  TProfile = Class(TGoogleBaseObject)
  Private
    FemailAddress : String;
    FhistoryId : String;
    FmessagesTotal : integer;
    FthreadsTotal : integer;
  Protected
    //Property setters
    Procedure SetemailAddress(AIndex : Integer; AValue : String); virtual;
    Procedure SethistoryId(AIndex : Integer; AValue : String); virtual;
    Procedure SetmessagesTotal(AIndex : Integer; AValue : integer); virtual;
    Procedure SetthreadsTotal(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property emailAddress : String Index 0 Read FemailAddress Write SetemailAddress;
    Property historyId : String Index 8 Read FhistoryId Write SethistoryId;
    Property messagesTotal : integer Index 16 Read FmessagesTotal Write SetmessagesTotal;
    Property threadsTotal : integer Index 24 Read FthreadsTotal Write SetthreadsTotal;
  end;
  TProfileClass = Class of TProfile;
  
  { --------------------------------------------------------------------
    TThread
    --------------------------------------------------------------------}
  
  TThread = Class(TGoogleBaseObject)
  Private
    FhistoryId : String;
    Fid : String;
    Fmessages : TThreadTypemessagesArray;
    Fsnippet : String;
  Protected
    //Property setters
    Procedure SethistoryId(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setmessages(AIndex : Integer; AValue : TThreadTypemessagesArray); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property historyId : String Index 0 Read FhistoryId Write SethistoryId;
    Property id : String Index 8 Read Fid Write Setid;
    Property messages : TThreadTypemessagesArray Index 16 Read Fmessages Write Setmessages;
    Property snippet : String Index 24 Read Fsnippet Write Setsnippet;
  end;
  TThreadClass = Class of TThread;
  
  { --------------------------------------------------------------------
    TUsersDraftsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TUsersDraftsResource, method Get
  
  TUsersDraftsGetOptions = Record
    format : String;
  end;
  
  
  //Optional query Options for TUsersDraftsResource, method List
  
  TUsersDraftsListOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  TUsersDraftsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(userId: string; aDraft : TDraft) : TDraft;overload;
    Procedure Delete(id: string; userId: string);
    Function Get(id: string; userId: string; AQuery : string  = '') : TDraft;
    Function Get(id: string; userId: string; AQuery : TUsersDraftsgetOptions) : TDraft;
    Function List(userId: string; AQuery : string  = '') : TListDraftsResponse;
    Function List(userId: string; AQuery : TUsersDraftslistOptions) : TListDraftsResponse;
    Function Send(userId: string; aDraft : TDraft) : TMessage;
    Function Update(id: string; userId: string; aDraft : TDraft) : TDraft;
  end;
  
  
  { --------------------------------------------------------------------
    TUsersHistoryResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TUsersHistoryResource, method List
  
  TUsersHistoryListOptions = Record
    labelId : String;
    maxResults : integer;
    pageToken : String;
    startHistoryId : String;
  end;
  
  TUsersHistoryResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(userId: string; AQuery : string  = '') : TListHistoryResponse;
    Function List(userId: string; AQuery : TUsersHistorylistOptions) : TListHistoryResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TUsersLabelsResource
    --------------------------------------------------------------------}
  
  TUsersLabelsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(userId: string; aLabel : TLabel) : TLabel;overload;
    Procedure Delete(id: string; userId: string);
    Function Get(id: string; userId: string) : TLabel;
    Function List(userId: string) : TListLabelsResponse;
    Function Patch(id: string; userId: string; aLabel : TLabel) : TLabel;
    Function Update(id: string; userId: string; aLabel : TLabel) : TLabel;
  end;
  
  
  { --------------------------------------------------------------------
    TUsersMessagesAttachmentsResource
    --------------------------------------------------------------------}
  
  TUsersMessagesAttachmentsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; messageId: string; userId: string) : TMessagePartBody;
  end;
  
  
  { --------------------------------------------------------------------
    TUsersMessagesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TUsersMessagesResource, method Get
  
  TUsersMessagesGetOptions = Record
    format : String;
    metadataHeaders : String;
  end;
  
  
  //Optional query Options for TUsersMessagesResource, method Import
  
  TUsersMessagesImportOptions = Record
    deleted : boolean;
    internalDateSource : String;
    neverMarkSpam : boolean;
    processForCalendar : boolean;
  end;
  
  
  //Optional query Options for TUsersMessagesResource, method Insert
  
  TUsersMessagesInsertOptions = Record
    deleted : boolean;
    internalDateSource : String;
  end;
  
  
  //Optional query Options for TUsersMessagesResource, method List
  
  TUsersMessagesListOptions = Record
    includeSpamTrash : boolean;
    labelIds : String;
    maxResults : integer;
    pageToken : String;
    q : String;
  end;
  
  TUsersMessagesResource = Class(TGoogleResource)
  Private
    FAttachmentsInstance : TUsersMessagesAttachmentsResource;
    Function GetAttachmentsInstance : TUsersMessagesAttachmentsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(id: string; userId: string);
    Function Get(id: string; userId: string; AQuery : string  = '') : TMessage;
    Function Get(id: string; userId: string; AQuery : TUsersMessagesgetOptions) : TMessage;
    Function Import(userId: string; aMessage : TMessage; AQuery : string  = '') : TMessage;
    Function Import(userId: string; aMessage : TMessage; AQuery : TUsersMessagesimportOptions) : TMessage;
    Function Insert(userId: string; aMessage : TMessage; AQuery : string  = '') : TMessage;
    Function Insert(userId: string; aMessage : TMessage; AQuery : TUsersMessagesinsertOptions) : TMessage;
    Function List(userId: string; AQuery : string  = '') : TListMessagesResponse;
    Function List(userId: string; AQuery : TUsersMessageslistOptions) : TListMessagesResponse;
    Function Modify(id: string; userId: string; aModifyMessageRequest : TModifyMessageRequest) : TMessage;
    Function Send(userId: string; aMessage : TMessage) : TMessage;
    Function Trash(id: string; userId: string) : TMessage;
    Function Untrash(id: string; userId: string) : TMessage;
    Function CreateAttachmentsResource(AOwner : TComponent) : TUsersMessagesAttachmentsResource;virtual;overload;
    Function CreateAttachmentsResource : TUsersMessagesAttachmentsResource;virtual;overload;
    Property AttachmentsResource : TUsersMessagesAttachmentsResource Read GetAttachmentsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TUsersThreadsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TUsersThreadsResource, method Get
  
  TUsersThreadsGetOptions = Record
    format : String;
    metadataHeaders : String;
  end;
  
  
  //Optional query Options for TUsersThreadsResource, method List
  
  TUsersThreadsListOptions = Record
    includeSpamTrash : boolean;
    labelIds : String;
    maxResults : integer;
    pageToken : String;
    q : String;
  end;
  
  TUsersThreadsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(id: string; userId: string);
    Function Get(id: string; userId: string; AQuery : string  = '') : TThread;
    Function Get(id: string; userId: string; AQuery : TUsersThreadsgetOptions) : TThread;
    Function List(userId: string; AQuery : string  = '') : TListThreadsResponse;
    Function List(userId: string; AQuery : TUsersThreadslistOptions) : TListThreadsResponse;
    Function Modify(id: string; userId: string; aModifyThreadRequest : TModifyThreadRequest) : TThread;
    Function Trash(id: string; userId: string) : TThread;
    Function Untrash(id: string; userId: string) : TThread;
  end;
  
  
  { --------------------------------------------------------------------
    TUsersResource
    --------------------------------------------------------------------}
  
  TUsersResource = Class(TGoogleResource)
  Private
    FDraftsInstance : TUsersDraftsResource;
    FHistoryInstance : TUsersHistoryResource;
    FLabelsInstance : TUsersLabelsResource;
    FMessagesAttachmentsInstance : TUsersMessagesAttachmentsResource;
    FMessagesInstance : TUsersMessagesResource;
    FThreadsInstance : TUsersThreadsResource;
    Function GetDraftsInstance : TUsersDraftsResource;virtual;
    Function GetHistoryInstance : TUsersHistoryResource;virtual;
    Function GetLabelsInstance : TUsersLabelsResource;virtual;
    Function GetMessagesAttachmentsInstance : TUsersMessagesAttachmentsResource;virtual;
    Function GetMessagesInstance : TUsersMessagesResource;virtual;
    Function GetThreadsInstance : TUsersThreadsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function GetProfile(userId: string) : TProfile;
    Function CreateDraftsResource(AOwner : TComponent) : TUsersDraftsResource;virtual;overload;
    Function CreateDraftsResource : TUsersDraftsResource;virtual;overload;
    Function CreateHistoryResource(AOwner : TComponent) : TUsersHistoryResource;virtual;overload;
    Function CreateHistoryResource : TUsersHistoryResource;virtual;overload;
    Function CreateLabelsResource(AOwner : TComponent) : TUsersLabelsResource;virtual;overload;
    Function CreateLabelsResource : TUsersLabelsResource;virtual;overload;
    Function CreateMessagesAttachmentsResource(AOwner : TComponent) : TUsersMessagesAttachmentsResource;virtual;overload;
    Function CreateMessagesAttachmentsResource : TUsersMessagesAttachmentsResource;virtual;overload;
    Function CreateMessagesResource(AOwner : TComponent) : TUsersMessagesResource;virtual;overload;
    Function CreateMessagesResource : TUsersMessagesResource;virtual;overload;
    Function CreateThreadsResource(AOwner : TComponent) : TUsersThreadsResource;virtual;overload;
    Function CreateThreadsResource : TUsersThreadsResource;virtual;overload;
    Property DraftsResource : TUsersDraftsResource Read GetDraftsInstance;
    Property HistoryResource : TUsersHistoryResource Read GetHistoryInstance;
    Property LabelsResource : TUsersLabelsResource Read GetLabelsInstance;
    Property MessagesAttachmentsResource : TUsersMessagesAttachmentsResource Read GetMessagesAttachmentsInstance;
    Property MessagesResource : TUsersMessagesResource Read GetMessagesInstance;
    Property ThreadsResource : TUsersThreadsResource Read GetThreadsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TGmailAPI
    --------------------------------------------------------------------}
  
  TGmailAPI = Class(TGoogleAPI)
  Private
    FUsersDraftsInstance : TUsersDraftsResource;
    FUsersHistoryInstance : TUsersHistoryResource;
    FUsersLabelsInstance : TUsersLabelsResource;
    FUsersMessagesAttachmentsInstance : TUsersMessagesAttachmentsResource;
    FUsersMessagesInstance : TUsersMessagesResource;
    FUsersThreadsInstance : TUsersThreadsResource;
    FUsersInstance : TUsersResource;
    Function GetUsersDraftsInstance : TUsersDraftsResource;virtual;
    Function GetUsersHistoryInstance : TUsersHistoryResource;virtual;
    Function GetUsersLabelsInstance : TUsersLabelsResource;virtual;
    Function GetUsersMessagesAttachmentsInstance : TUsersMessagesAttachmentsResource;virtual;
    Function GetUsersMessagesInstance : TUsersMessagesResource;virtual;
    Function GetUsersThreadsInstance : TUsersThreadsResource;virtual;
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
    Function CreateUsersDraftsResource(AOwner : TComponent) : TUsersDraftsResource;virtual;overload;
    Function CreateUsersDraftsResource : TUsersDraftsResource;virtual;overload;
    Function CreateUsersHistoryResource(AOwner : TComponent) : TUsersHistoryResource;virtual;overload;
    Function CreateUsersHistoryResource : TUsersHistoryResource;virtual;overload;
    Function CreateUsersLabelsResource(AOwner : TComponent) : TUsersLabelsResource;virtual;overload;
    Function CreateUsersLabelsResource : TUsersLabelsResource;virtual;overload;
    Function CreateUsersMessagesAttachmentsResource(AOwner : TComponent) : TUsersMessagesAttachmentsResource;virtual;overload;
    Function CreateUsersMessagesAttachmentsResource : TUsersMessagesAttachmentsResource;virtual;overload;
    Function CreateUsersMessagesResource(AOwner : TComponent) : TUsersMessagesResource;virtual;overload;
    Function CreateUsersMessagesResource : TUsersMessagesResource;virtual;overload;
    Function CreateUsersThreadsResource(AOwner : TComponent) : TUsersThreadsResource;virtual;overload;
    Function CreateUsersThreadsResource : TUsersThreadsResource;virtual;overload;
    Function CreateUsersResource(AOwner : TComponent) : TUsersResource;virtual;overload;
    Function CreateUsersResource : TUsersResource;virtual;overload;
    //Add default on-demand instances for resources
    Property UsersDraftsResource : TUsersDraftsResource Read GetUsersDraftsInstance;
    Property UsersHistoryResource : TUsersHistoryResource Read GetUsersHistoryInstance;
    Property UsersLabelsResource : TUsersLabelsResource Read GetUsersLabelsInstance;
    Property UsersMessagesAttachmentsResource : TUsersMessagesAttachmentsResource Read GetUsersMessagesAttachmentsInstance;
    Property UsersMessagesResource : TUsersMessagesResource Read GetUsersMessagesInstance;
    Property UsersThreadsResource : TUsersThreadsResource Read GetUsersThreadsInstance;
    Property UsersResource : TUsersResource Read GetUsersInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TDraft
  --------------------------------------------------------------------}


Procedure TDraft.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDraft.Setmessage(AIndex : Integer; AValue : TMessage); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  THistory
  --------------------------------------------------------------------}


Procedure THistory.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THistory.SetlabelsAdded(AIndex : Integer; AValue : THistoryTypelabelsAddedArray); 

begin
  If (FlabelsAdded=AValue) then exit;
  FlabelsAdded:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THistory.SetlabelsRemoved(AIndex : Integer; AValue : THistoryTypelabelsRemovedArray); 

begin
  If (FlabelsRemoved=AValue) then exit;
  FlabelsRemoved:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THistory.Setmessages(AIndex : Integer; AValue : THistoryTypemessagesArray); 

begin
  If (Fmessages=AValue) then exit;
  Fmessages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THistory.SetmessagesAdded(AIndex : Integer; AValue : THistoryTypemessagesAddedArray); 

begin
  If (FmessagesAdded=AValue) then exit;
  FmessagesAdded:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THistory.SetmessagesDeleted(AIndex : Integer; AValue : THistoryTypemessagesDeletedArray); 

begin
  If (FmessagesDeleted=AValue) then exit;
  FmessagesDeleted:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure THistory.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'labelsadded' : SetLength(FlabelsAdded,ALength);
  'labelsremoved' : SetLength(FlabelsRemoved,ALength);
  'messages' : SetLength(Fmessages,ALength);
  'messagesadded' : SetLength(FmessagesAdded,ALength);
  'messagesdeleted' : SetLength(FmessagesDeleted,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  THistoryLabelAdded
  --------------------------------------------------------------------}


Procedure THistoryLabelAdded.SetlabelIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FlabelIds=AValue) then exit;
  FlabelIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THistoryLabelAdded.Setmessage(AIndex : Integer; AValue : TMessage); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure THistoryLabelAdded.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'labelids' : SetLength(FlabelIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  THistoryLabelRemoved
  --------------------------------------------------------------------}


Procedure THistoryLabelRemoved.SetlabelIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FlabelIds=AValue) then exit;
  FlabelIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THistoryLabelRemoved.Setmessage(AIndex : Integer; AValue : TMessage); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure THistoryLabelRemoved.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'labelids' : SetLength(FlabelIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  THistoryMessageAdded
  --------------------------------------------------------------------}


Procedure THistoryMessageAdded.Setmessage(AIndex : Integer; AValue : TMessage); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  THistoryMessageDeleted
  --------------------------------------------------------------------}


Procedure THistoryMessageDeleted.Setmessage(AIndex : Integer; AValue : TMessage); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLabel
  --------------------------------------------------------------------}


Procedure TLabel.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLabel.SetlabelListVisibility(AIndex : Integer; AValue : String); 

begin
  If (FlabelListVisibility=AValue) then exit;
  FlabelListVisibility:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLabel.SetmessageListVisibility(AIndex : Integer; AValue : String); 

begin
  If (FmessageListVisibility=AValue) then exit;
  FmessageListVisibility:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLabel.SetmessagesTotal(AIndex : Integer; AValue : integer); 

begin
  If (FmessagesTotal=AValue) then exit;
  FmessagesTotal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLabel.SetmessagesUnread(AIndex : Integer; AValue : integer); 

begin
  If (FmessagesUnread=AValue) then exit;
  FmessagesUnread:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLabel.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLabel.SetthreadsTotal(AIndex : Integer; AValue : integer); 

begin
  If (FthreadsTotal=AValue) then exit;
  FthreadsTotal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLabel.SetthreadsUnread(AIndex : Integer; AValue : integer); 

begin
  If (FthreadsUnread=AValue) then exit;
  FthreadsUnread:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLabel.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TLabel.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TListDraftsResponse
  --------------------------------------------------------------------}


Procedure TListDraftsResponse.Setdrafts(AIndex : Integer; AValue : TListDraftsResponseTypedraftsArray); 

begin
  If (Fdrafts=AValue) then exit;
  Fdrafts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListDraftsResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListDraftsResponse.SetresultSizeEstimate(AIndex : Integer; AValue : integer); 

begin
  If (FresultSizeEstimate=AValue) then exit;
  FresultSizeEstimate:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListDraftsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'drafts' : SetLength(Fdrafts,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListHistoryResponse
  --------------------------------------------------------------------}


Procedure TListHistoryResponse.Sethistory(AIndex : Integer; AValue : TListHistoryResponseTypehistoryArray); 

begin
  If (Fhistory=AValue) then exit;
  Fhistory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListHistoryResponse.SethistoryId(AIndex : Integer; AValue : String); 

begin
  If (FhistoryId=AValue) then exit;
  FhistoryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListHistoryResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListHistoryResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'history' : SetLength(Fhistory,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListLabelsResponse
  --------------------------------------------------------------------}


Procedure TListLabelsResponse.Setlabels(AIndex : Integer; AValue : TListLabelsResponseTypelabelsArray); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListLabelsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'labels' : SetLength(Flabels,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListMessagesResponse
  --------------------------------------------------------------------}


Procedure TListMessagesResponse.Setmessages(AIndex : Integer; AValue : TListMessagesResponseTypemessagesArray); 

begin
  If (Fmessages=AValue) then exit;
  Fmessages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListMessagesResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListMessagesResponse.SetresultSizeEstimate(AIndex : Integer; AValue : integer); 

begin
  If (FresultSizeEstimate=AValue) then exit;
  FresultSizeEstimate:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListMessagesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'messages' : SetLength(Fmessages,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListThreadsResponse
  --------------------------------------------------------------------}


Procedure TListThreadsResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListThreadsResponse.SetresultSizeEstimate(AIndex : Integer; AValue : integer); 

begin
  If (FresultSizeEstimate=AValue) then exit;
  FresultSizeEstimate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListThreadsResponse.Setthreads(AIndex : Integer; AValue : TListThreadsResponseTypethreadsArray); 

begin
  If (Fthreads=AValue) then exit;
  Fthreads:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListThreadsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'threads' : SetLength(Fthreads,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMessage
  --------------------------------------------------------------------}


Procedure TMessage.SethistoryId(AIndex : Integer; AValue : String); 

begin
  If (FhistoryId=AValue) then exit;
  FhistoryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMessage.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMessage.SetlabelIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FlabelIds=AValue) then exit;
  FlabelIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMessage.Setpayload(AIndex : Integer; AValue : TMessagePart); 

begin
  If (Fpayload=AValue) then exit;
  Fpayload:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMessage.Setraw(AIndex : Integer; AValue : String); 

begin
  If (Fraw=AValue) then exit;
  Fraw:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMessage.SetsizeEstimate(AIndex : Integer; AValue : integer); 

begin
  If (FsizeEstimate=AValue) then exit;
  FsizeEstimate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMessage.Setsnippet(AIndex : Integer; AValue : String); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMessage.SetthreadId(AIndex : Integer; AValue : String); 

begin
  If (FthreadId=AValue) then exit;
  FthreadId:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TMessage.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'labelids' : SetLength(FlabelIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMessagePart
  --------------------------------------------------------------------}


Procedure TMessagePart.Setbody(AIndex : Integer; AValue : TMessagePartBody); 

begin
  If (Fbody=AValue) then exit;
  Fbody:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMessagePart.Setfilename(AIndex : Integer; AValue : String); 

begin
  If (Ffilename=AValue) then exit;
  Ffilename:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMessagePart.Setheaders(AIndex : Integer; AValue : TMessagePartTypeheadersArray); 

begin
  If (Fheaders=AValue) then exit;
  Fheaders:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMessagePart.SetmimeType(AIndex : Integer; AValue : String); 

begin
  If (FmimeType=AValue) then exit;
  FmimeType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMessagePart.SetpartId(AIndex : Integer; AValue : String); 

begin
  If (FpartId=AValue) then exit;
  FpartId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMessagePart.Setparts(AIndex : Integer; AValue : TMessagePartTypepartsArray); 

begin
  If (Fparts=AValue) then exit;
  Fparts:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TMessagePart.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'headers' : SetLength(Fheaders,ALength);
  'parts' : SetLength(Fparts,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMessagePartBody
  --------------------------------------------------------------------}


Procedure TMessagePartBody.SetattachmentId(AIndex : Integer; AValue : String); 

begin
  If (FattachmentId=AValue) then exit;
  FattachmentId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMessagePartBody.Setdata(AIndex : Integer; AValue : String); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMessagePartBody.Setsize(AIndex : Integer; AValue : integer); 

begin
  If (Fsize=AValue) then exit;
  Fsize:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMessagePartHeader
  --------------------------------------------------------------------}


Procedure TMessagePartHeader.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMessagePartHeader.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TModifyMessageRequest
  --------------------------------------------------------------------}


Procedure TModifyMessageRequest.SetaddLabelIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FaddLabelIds=AValue) then exit;
  FaddLabelIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TModifyMessageRequest.SetremoveLabelIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FremoveLabelIds=AValue) then exit;
  FremoveLabelIds:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TModifyMessageRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'addlabelids' : SetLength(FaddLabelIds,ALength);
  'removelabelids' : SetLength(FremoveLabelIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TModifyThreadRequest
  --------------------------------------------------------------------}


Procedure TModifyThreadRequest.SetaddLabelIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FaddLabelIds=AValue) then exit;
  FaddLabelIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TModifyThreadRequest.SetremoveLabelIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FremoveLabelIds=AValue) then exit;
  FremoveLabelIds:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TModifyThreadRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'addlabelids' : SetLength(FaddLabelIds,ALength);
  'removelabelids' : SetLength(FremoveLabelIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TProfile
  --------------------------------------------------------------------}


Procedure TProfile.SetemailAddress(AIndex : Integer; AValue : String); 

begin
  If (FemailAddress=AValue) then exit;
  FemailAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.SethistoryId(AIndex : Integer; AValue : String); 

begin
  If (FhistoryId=AValue) then exit;
  FhistoryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.SetmessagesTotal(AIndex : Integer; AValue : integer); 

begin
  If (FmessagesTotal=AValue) then exit;
  FmessagesTotal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.SetthreadsTotal(AIndex : Integer; AValue : integer); 

begin
  If (FthreadsTotal=AValue) then exit;
  FthreadsTotal:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TThread
  --------------------------------------------------------------------}


Procedure TThread.SethistoryId(AIndex : Integer; AValue : String); 

begin
  If (FhistoryId=AValue) then exit;
  FhistoryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThread.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThread.Setmessages(AIndex : Integer; AValue : TThreadTypemessagesArray); 

begin
  If (Fmessages=AValue) then exit;
  Fmessages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThread.Setsnippet(AIndex : Integer; AValue : String); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TThread.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'messages' : SetLength(Fmessages,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TUsersDraftsResource
  --------------------------------------------------------------------}


Class Function TUsersDraftsResource.ResourceName : String;

begin
  Result:='drafts';
end;

Class Function TUsersDraftsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgmailAPI;
end;

Function TUsersDraftsResource.Create(userId: string; aDraft : TDraft) : TDraft;

Const
  _HTTPMethod = 'POST';
  _Path       = '{userId}/drafts';
  _Methodid   = 'gmail.users.drafts.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aDraft,TDraft) as TDraft;
end;

Procedure TUsersDraftsResource.Delete(id: string; userId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{userId}/drafts/{id}';
  _Methodid   = 'gmail.users.drafts.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'userId',userId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TUsersDraftsResource.Get(id: string; userId: string; AQuery : string = '') : TDraft;

Const
  _HTTPMethod = 'GET';
  _Path       = '{userId}/drafts/{id}';
  _Methodid   = 'gmail.users.drafts.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TDraft) as TDraft;
end;


Function TUsersDraftsResource.Get(id: string; userId: string; AQuery : TUsersDraftsgetOptions) : TDraft;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'format',AQuery.format);
  Result:=Get(id,userId,_Q);
end;

Function TUsersDraftsResource.List(userId: string; AQuery : string = '') : TListDraftsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{userId}/drafts';
  _Methodid   = 'gmail.users.drafts.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListDraftsResponse) as TListDraftsResponse;
end;


Function TUsersDraftsResource.List(userId: string; AQuery : TUsersDraftslistOptions) : TListDraftsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(userId,_Q);
end;

Function TUsersDraftsResource.Send(userId: string; aDraft : TDraft) : TMessage;

Const
  _HTTPMethod = 'POST';
  _Path       = '{userId}/drafts/send';
  _Methodid   = 'gmail.users.drafts.send';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aDraft,TMessage) as TMessage;
end;

Function TUsersDraftsResource.Update(id: string; userId: string; aDraft : TDraft) : TDraft;

Const
  _HTTPMethod = 'PUT';
  _Path       = '{userId}/drafts/{id}';
  _Methodid   = 'gmail.users.drafts.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aDraft,TDraft) as TDraft;
end;



{ --------------------------------------------------------------------
  TUsersHistoryResource
  --------------------------------------------------------------------}


Class Function TUsersHistoryResource.ResourceName : String;

begin
  Result:='history';
end;

Class Function TUsersHistoryResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgmailAPI;
end;

Function TUsersHistoryResource.List(userId: string; AQuery : string = '') : TListHistoryResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{userId}/history';
  _Methodid   = 'gmail.users.history.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListHistoryResponse) as TListHistoryResponse;
end;


Function TUsersHistoryResource.List(userId: string; AQuery : TUsersHistorylistOptions) : TListHistoryResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'labelId',AQuery.labelId);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'startHistoryId',AQuery.startHistoryId);
  Result:=List(userId,_Q);
end;



{ --------------------------------------------------------------------
  TUsersLabelsResource
  --------------------------------------------------------------------}


Class Function TUsersLabelsResource.ResourceName : String;

begin
  Result:='labels';
end;

Class Function TUsersLabelsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgmailAPI;
end;

Function TUsersLabelsResource.Create(userId: string; aLabel : TLabel) : TLabel;

Const
  _HTTPMethod = 'POST';
  _Path       = '{userId}/labels';
  _Methodid   = 'gmail.users.labels.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aLabel,TLabel) as TLabel;
end;

Procedure TUsersLabelsResource.Delete(id: string; userId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{userId}/labels/{id}';
  _Methodid   = 'gmail.users.labels.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'userId',userId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TUsersLabelsResource.Get(id: string; userId: string) : TLabel;

Const
  _HTTPMethod = 'GET';
  _Path       = '{userId}/labels/{id}';
  _Methodid   = 'gmail.users.labels.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TLabel) as TLabel;
end;

Function TUsersLabelsResource.List(userId: string) : TListLabelsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{userId}/labels';
  _Methodid   = 'gmail.users.labels.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TListLabelsResponse) as TListLabelsResponse;
end;

Function TUsersLabelsResource.Patch(id: string; userId: string; aLabel : TLabel) : TLabel;

Const
  _HTTPMethod = 'PATCH';
  _Path       = '{userId}/labels/{id}';
  _Methodid   = 'gmail.users.labels.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aLabel,TLabel) as TLabel;
end;

Function TUsersLabelsResource.Update(id: string; userId: string; aLabel : TLabel) : TLabel;

Const
  _HTTPMethod = 'PUT';
  _Path       = '{userId}/labels/{id}';
  _Methodid   = 'gmail.users.labels.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aLabel,TLabel) as TLabel;
end;



{ --------------------------------------------------------------------
  TUsersMessagesAttachmentsResource
  --------------------------------------------------------------------}


Class Function TUsersMessagesAttachmentsResource.ResourceName : String;

begin
  Result:='attachments';
end;

Class Function TUsersMessagesAttachmentsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgmailAPI;
end;

Function TUsersMessagesAttachmentsResource.Get(id: string; messageId: string; userId: string) : TMessagePartBody;

Const
  _HTTPMethod = 'GET';
  _Path       = '{userId}/messages/{messageId}/attachments/{id}';
  _Methodid   = 'gmail.users.messages.attachments.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'messageId',messageId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TMessagePartBody) as TMessagePartBody;
end;



{ --------------------------------------------------------------------
  TUsersMessagesResource
  --------------------------------------------------------------------}


Class Function TUsersMessagesResource.ResourceName : String;

begin
  Result:='messages';
end;

Class Function TUsersMessagesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgmailAPI;
end;

Procedure TUsersMessagesResource.Delete(id: string; userId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{userId}/messages/{id}';
  _Methodid   = 'gmail.users.messages.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'userId',userId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TUsersMessagesResource.Get(id: string; userId: string; AQuery : string = '') : TMessage;

Const
  _HTTPMethod = 'GET';
  _Path       = '{userId}/messages/{id}';
  _Methodid   = 'gmail.users.messages.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TMessage) as TMessage;
end;


Function TUsersMessagesResource.Get(id: string; userId: string; AQuery : TUsersMessagesgetOptions) : TMessage;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'format',AQuery.format);
  AddToQuery(_Q,'metadataHeaders',AQuery.metadataHeaders);
  Result:=Get(id,userId,_Q);
end;

Function TUsersMessagesResource.Import(userId: string; aMessage : TMessage; AQuery : string = '') : TMessage;

Const
  _HTTPMethod = 'POST';
  _Path       = '{userId}/messages/import';
  _Methodid   = 'gmail.users.messages.import';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aMessage,TMessage) as TMessage;
end;


Function TUsersMessagesResource.Import(userId: string; aMessage : TMessage; AQuery : TUsersMessagesimportOptions) : TMessage;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'deleted',AQuery.deleted);
  AddToQuery(_Q,'internalDateSource',AQuery.internalDateSource);
  AddToQuery(_Q,'neverMarkSpam',AQuery.neverMarkSpam);
  AddToQuery(_Q,'processForCalendar',AQuery.processForCalendar);
  Result:=Import(userId,aMessage,_Q);
end;

Function TUsersMessagesResource.Insert(userId: string; aMessage : TMessage; AQuery : string = '') : TMessage;

Const
  _HTTPMethod = 'POST';
  _Path       = '{userId}/messages';
  _Methodid   = 'gmail.users.messages.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aMessage,TMessage) as TMessage;
end;


Function TUsersMessagesResource.Insert(userId: string; aMessage : TMessage; AQuery : TUsersMessagesinsertOptions) : TMessage;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'deleted',AQuery.deleted);
  AddToQuery(_Q,'internalDateSource',AQuery.internalDateSource);
  Result:=Insert(userId,aMessage,_Q);
end;

Function TUsersMessagesResource.List(userId: string; AQuery : string = '') : TListMessagesResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{userId}/messages';
  _Methodid   = 'gmail.users.messages.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListMessagesResponse) as TListMessagesResponse;
end;


Function TUsersMessagesResource.List(userId: string; AQuery : TUsersMessageslistOptions) : TListMessagesResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'includeSpamTrash',AQuery.includeSpamTrash);
  AddToQuery(_Q,'labelIds',AQuery.labelIds);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'q',AQuery.q);
  Result:=List(userId,_Q);
end;

Function TUsersMessagesResource.Modify(id: string; userId: string; aModifyMessageRequest : TModifyMessageRequest) : TMessage;

Const
  _HTTPMethod = 'POST';
  _Path       = '{userId}/messages/{id}/modify';
  _Methodid   = 'gmail.users.messages.modify';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aModifyMessageRequest,TMessage) as TMessage;
end;

Function TUsersMessagesResource.Send(userId: string; aMessage : TMessage) : TMessage;

Const
  _HTTPMethod = 'POST';
  _Path       = '{userId}/messages/send';
  _Methodid   = 'gmail.users.messages.send';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aMessage,TMessage) as TMessage;
end;

Function TUsersMessagesResource.Trash(id: string; userId: string) : TMessage;

Const
  _HTTPMethod = 'POST';
  _Path       = '{userId}/messages/{id}/trash';
  _Methodid   = 'gmail.users.messages.trash';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TMessage) as TMessage;
end;

Function TUsersMessagesResource.Untrash(id: string; userId: string) : TMessage;

Const
  _HTTPMethod = 'POST';
  _Path       = '{userId}/messages/{id}/untrash';
  _Methodid   = 'gmail.users.messages.untrash';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TMessage) as TMessage;
end;



Function TUsersMessagesResource.GetAttachmentsInstance : TUsersMessagesAttachmentsResource;

begin
  if (FAttachmentsInstance=Nil) then
    FAttachmentsInstance:=CreateAttachmentsResource;
  Result:=FAttachmentsInstance;
end;

Function TUsersMessagesResource.CreateAttachmentsResource : TUsersMessagesAttachmentsResource;

begin
  Result:=CreateAttachmentsResource(Self);
end;


Function TUsersMessagesResource.CreateAttachmentsResource(AOwner : TComponent) : TUsersMessagesAttachmentsResource;

begin
  Result:=TUsersMessagesAttachmentsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TUsersThreadsResource
  --------------------------------------------------------------------}


Class Function TUsersThreadsResource.ResourceName : String;

begin
  Result:='threads';
end;

Class Function TUsersThreadsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgmailAPI;
end;

Procedure TUsersThreadsResource.Delete(id: string; userId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{userId}/threads/{id}';
  _Methodid   = 'gmail.users.threads.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'userId',userId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TUsersThreadsResource.Get(id: string; userId: string; AQuery : string = '') : TThread;

Const
  _HTTPMethod = 'GET';
  _Path       = '{userId}/threads/{id}';
  _Methodid   = 'gmail.users.threads.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TThread) as TThread;
end;


Function TUsersThreadsResource.Get(id: string; userId: string; AQuery : TUsersThreadsgetOptions) : TThread;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'format',AQuery.format);
  AddToQuery(_Q,'metadataHeaders',AQuery.metadataHeaders);
  Result:=Get(id,userId,_Q);
end;

Function TUsersThreadsResource.List(userId: string; AQuery : string = '') : TListThreadsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{userId}/threads';
  _Methodid   = 'gmail.users.threads.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListThreadsResponse) as TListThreadsResponse;
end;


Function TUsersThreadsResource.List(userId: string; AQuery : TUsersThreadslistOptions) : TListThreadsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'includeSpamTrash',AQuery.includeSpamTrash);
  AddToQuery(_Q,'labelIds',AQuery.labelIds);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'q',AQuery.q);
  Result:=List(userId,_Q);
end;

Function TUsersThreadsResource.Modify(id: string; userId: string; aModifyThreadRequest : TModifyThreadRequest) : TThread;

Const
  _HTTPMethod = 'POST';
  _Path       = '{userId}/threads/{id}/modify';
  _Methodid   = 'gmail.users.threads.modify';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aModifyThreadRequest,TThread) as TThread;
end;

Function TUsersThreadsResource.Trash(id: string; userId: string) : TThread;

Const
  _HTTPMethod = 'POST';
  _Path       = '{userId}/threads/{id}/trash';
  _Methodid   = 'gmail.users.threads.trash';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TThread) as TThread;
end;

Function TUsersThreadsResource.Untrash(id: string; userId: string) : TThread;

Const
  _HTTPMethod = 'POST';
  _Path       = '{userId}/threads/{id}/untrash';
  _Methodid   = 'gmail.users.threads.untrash';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TThread) as TThread;
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
  Result:=TgmailAPI;
end;

Function TUsersResource.GetProfile(userId: string) : TProfile;

Const
  _HTTPMethod = 'GET';
  _Path       = '{userId}/profile';
  _Methodid   = 'gmail.users.getProfile';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TProfile) as TProfile;
end;



Function TUsersResource.GetDraftsInstance : TUsersDraftsResource;

begin
  if (FDraftsInstance=Nil) then
    FDraftsInstance:=CreateDraftsResource;
  Result:=FDraftsInstance;
end;

Function TUsersResource.CreateDraftsResource : TUsersDraftsResource;

begin
  Result:=CreateDraftsResource(Self);
end;


Function TUsersResource.CreateDraftsResource(AOwner : TComponent) : TUsersDraftsResource;

begin
  Result:=TUsersDraftsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TUsersResource.GetHistoryInstance : TUsersHistoryResource;

begin
  if (FHistoryInstance=Nil) then
    FHistoryInstance:=CreateHistoryResource;
  Result:=FHistoryInstance;
end;

Function TUsersResource.CreateHistoryResource : TUsersHistoryResource;

begin
  Result:=CreateHistoryResource(Self);
end;


Function TUsersResource.CreateHistoryResource(AOwner : TComponent) : TUsersHistoryResource;

begin
  Result:=TUsersHistoryResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TUsersResource.GetLabelsInstance : TUsersLabelsResource;

begin
  if (FLabelsInstance=Nil) then
    FLabelsInstance:=CreateLabelsResource;
  Result:=FLabelsInstance;
end;

Function TUsersResource.CreateLabelsResource : TUsersLabelsResource;

begin
  Result:=CreateLabelsResource(Self);
end;


Function TUsersResource.CreateLabelsResource(AOwner : TComponent) : TUsersLabelsResource;

begin
  Result:=TUsersLabelsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TUsersResource.GetMessagesAttachmentsInstance : TUsersMessagesAttachmentsResource;

begin
  if (FMessagesAttachmentsInstance=Nil) then
    FMessagesAttachmentsInstance:=CreateMessagesAttachmentsResource;
  Result:=FMessagesAttachmentsInstance;
end;

Function TUsersResource.CreateMessagesAttachmentsResource : TUsersMessagesAttachmentsResource;

begin
  Result:=CreateMessagesAttachmentsResource(Self);
end;


Function TUsersResource.CreateMessagesAttachmentsResource(AOwner : TComponent) : TUsersMessagesAttachmentsResource;

begin
  Result:=TUsersMessagesAttachmentsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TUsersResource.GetMessagesInstance : TUsersMessagesResource;

begin
  if (FMessagesInstance=Nil) then
    FMessagesInstance:=CreateMessagesResource;
  Result:=FMessagesInstance;
end;

Function TUsersResource.CreateMessagesResource : TUsersMessagesResource;

begin
  Result:=CreateMessagesResource(Self);
end;


Function TUsersResource.CreateMessagesResource(AOwner : TComponent) : TUsersMessagesResource;

begin
  Result:=TUsersMessagesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TUsersResource.GetThreadsInstance : TUsersThreadsResource;

begin
  if (FThreadsInstance=Nil) then
    FThreadsInstance:=CreateThreadsResource;
  Result:=FThreadsInstance;
end;

Function TUsersResource.CreateThreadsResource : TUsersThreadsResource;

begin
  Result:=CreateThreadsResource(Self);
end;


Function TUsersResource.CreateThreadsResource(AOwner : TComponent) : TUsersThreadsResource;

begin
  Result:=TUsersThreadsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TGmailAPI
  --------------------------------------------------------------------}

Class Function TGmailAPI.APIName : String;

begin
  Result:='gmail';
end;

Class Function TGmailAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TGmailAPI.APIRevision : String;

begin
  Result:='20150326';
end;

Class Function TGmailAPI.APIID : String;

begin
  Result:='gmail:v1';
end;

Class Function TGmailAPI.APITitle : String;

begin
  Result:='Gmail API';
end;

Class Function TGmailAPI.APIDescription : String;

begin
  Result:='The Gmail REST API.';
end;

Class Function TGmailAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TGmailAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TGmailAPI.APIIcon16 : String;

begin
  Result:='https://www.google.com/images/icons/product/googlemail-16.png';
end;

Class Function TGmailAPI.APIIcon32 : String;

begin
  Result:='https://www.google.com/images/icons/product/googlemail-32.png';
end;

Class Function TGmailAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/gmail/api/';
end;

Class Function TGmailAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com:443/';
end;

Class Function TGmailAPI.APIbasePath : string;

begin
  Result:='/gmail/v1/users/';
end;

Class Function TGmailAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/gmail/v1/users/';
end;

Class Function TGmailAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TGmailAPI.APIservicePath : string;

begin
  Result:='gmail/v1/users/';
end;

Class Function TGmailAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TGmailAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,6);
  Result[0].Name:='https://mail.google.com/';
  Result[0].Description:='View and manage your mail';
  Result[1].Name:='https://www.googleapis.com/auth/gmail.compose';
  Result[1].Description:='Manage drafts and send emails';
  Result[2].Name:='https://www.googleapis.com/auth/gmail.insert';
  Result[2].Description:='Insert mail into your mailbox';
  Result[3].Name:='https://www.googleapis.com/auth/gmail.labels';
  Result[3].Description:='Manage mailbox labels';
  Result[4].Name:='https://www.googleapis.com/auth/gmail.modify';
  Result[4].Description:='View and modify but not delete your email';
  Result[5].Name:='https://www.googleapis.com/auth/gmail.readonly';
  Result[5].Description:='View your emails messages and settings';
  
end;

Class Function TGmailAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TGmailAPI.RegisterAPIResources;

begin
  TDraft.RegisterObject;
  THistory.RegisterObject;
  THistoryLabelAdded.RegisterObject;
  THistoryLabelRemoved.RegisterObject;
  THistoryMessageAdded.RegisterObject;
  THistoryMessageDeleted.RegisterObject;
  TLabel.RegisterObject;
  TListDraftsResponse.RegisterObject;
  TListHistoryResponse.RegisterObject;
  TListLabelsResponse.RegisterObject;
  TListMessagesResponse.RegisterObject;
  TListThreadsResponse.RegisterObject;
  TMessage.RegisterObject;
  TMessagePart.RegisterObject;
  TMessagePartBody.RegisterObject;
  TMessagePartHeader.RegisterObject;
  TModifyMessageRequest.RegisterObject;
  TModifyThreadRequest.RegisterObject;
  TProfile.RegisterObject;
  TThread.RegisterObject;
end;


Function TGmailAPI.GetUsersDraftsInstance : TUsersDraftsResource;

begin
  if (FUsersDraftsInstance=Nil) then
    FUsersDraftsInstance:=CreateUsersDraftsResource;
  Result:=FUsersDraftsInstance;
end;

Function TGmailAPI.CreateUsersDraftsResource : TUsersDraftsResource;

begin
  Result:=CreateUsersDraftsResource(Self);
end;


Function TGmailAPI.CreateUsersDraftsResource(AOwner : TComponent) : TUsersDraftsResource;

begin
  Result:=TUsersDraftsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TGmailAPI.GetUsersHistoryInstance : TUsersHistoryResource;

begin
  if (FUsersHistoryInstance=Nil) then
    FUsersHistoryInstance:=CreateUsersHistoryResource;
  Result:=FUsersHistoryInstance;
end;

Function TGmailAPI.CreateUsersHistoryResource : TUsersHistoryResource;

begin
  Result:=CreateUsersHistoryResource(Self);
end;


Function TGmailAPI.CreateUsersHistoryResource(AOwner : TComponent) : TUsersHistoryResource;

begin
  Result:=TUsersHistoryResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TGmailAPI.GetUsersLabelsInstance : TUsersLabelsResource;

begin
  if (FUsersLabelsInstance=Nil) then
    FUsersLabelsInstance:=CreateUsersLabelsResource;
  Result:=FUsersLabelsInstance;
end;

Function TGmailAPI.CreateUsersLabelsResource : TUsersLabelsResource;

begin
  Result:=CreateUsersLabelsResource(Self);
end;


Function TGmailAPI.CreateUsersLabelsResource(AOwner : TComponent) : TUsersLabelsResource;

begin
  Result:=TUsersLabelsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TGmailAPI.GetUsersMessagesAttachmentsInstance : TUsersMessagesAttachmentsResource;

begin
  if (FUsersMessagesAttachmentsInstance=Nil) then
    FUsersMessagesAttachmentsInstance:=CreateUsersMessagesAttachmentsResource;
  Result:=FUsersMessagesAttachmentsInstance;
end;

Function TGmailAPI.CreateUsersMessagesAttachmentsResource : TUsersMessagesAttachmentsResource;

begin
  Result:=CreateUsersMessagesAttachmentsResource(Self);
end;


Function TGmailAPI.CreateUsersMessagesAttachmentsResource(AOwner : TComponent) : TUsersMessagesAttachmentsResource;

begin
  Result:=TUsersMessagesAttachmentsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TGmailAPI.GetUsersMessagesInstance : TUsersMessagesResource;

begin
  if (FUsersMessagesInstance=Nil) then
    FUsersMessagesInstance:=CreateUsersMessagesResource;
  Result:=FUsersMessagesInstance;
end;

Function TGmailAPI.CreateUsersMessagesResource : TUsersMessagesResource;

begin
  Result:=CreateUsersMessagesResource(Self);
end;


Function TGmailAPI.CreateUsersMessagesResource(AOwner : TComponent) : TUsersMessagesResource;

begin
  Result:=TUsersMessagesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TGmailAPI.GetUsersThreadsInstance : TUsersThreadsResource;

begin
  if (FUsersThreadsInstance=Nil) then
    FUsersThreadsInstance:=CreateUsersThreadsResource;
  Result:=FUsersThreadsInstance;
end;

Function TGmailAPI.CreateUsersThreadsResource : TUsersThreadsResource;

begin
  Result:=CreateUsersThreadsResource(Self);
end;


Function TGmailAPI.CreateUsersThreadsResource(AOwner : TComponent) : TUsersThreadsResource;

begin
  Result:=TUsersThreadsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TGmailAPI.GetUsersInstance : TUsersResource;

begin
  if (FUsersInstance=Nil) then
    FUsersInstance:=CreateUsersResource;
  Result:=FUsersInstance;
end;

Function TGmailAPI.CreateUsersResource : TUsersResource;

begin
  Result:=CreateUsersResource(Self);
end;


Function TGmailAPI.CreateUsersResource(AOwner : TComponent) : TUsersResource;

begin
  Result:=TUsersResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TGmailAPI.RegisterAPI;
end.
