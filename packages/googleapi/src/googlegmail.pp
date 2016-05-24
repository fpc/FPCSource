unit googlegmail;
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
  TDraft = class;
  TDraftArray = Array of TDraft;
  THistory = class;
  THistoryArray = Array of THistory;
  THistorylabelsAdded = class;
  THistorylabelsAddedArray = Array of THistorylabelsAdded;
  THistorylabelsRemoved = class;
  THistorylabelsRemovedArray = Array of THistorylabelsRemoved;
  THistorymessages = class;
  THistorymessagesArray = Array of THistorymessages;
  THistorymessagesAdded = class;
  THistorymessagesAddedArray = Array of THistorymessagesAdded;
  THistorymessagesDeleted = class;
  THistorymessagesDeletedArray = Array of THistorymessagesDeleted;
  THistoryLabelAdded = class;
  THistoryLabelAddedArray = Array of THistoryLabelAdded;
  THistoryLabelAddedlabelIds = class;
  THistoryLabelAddedlabelIdsArray = Array of THistoryLabelAddedlabelIds;
  THistoryLabelRemoved = class;
  THistoryLabelRemovedArray = Array of THistoryLabelRemoved;
  THistoryLabelRemovedlabelIds = class;
  THistoryLabelRemovedlabelIdsArray = Array of THistoryLabelRemovedlabelIds;
  THistoryMessageAdded = class;
  THistoryMessageAddedArray = Array of THistoryMessageAdded;
  THistoryMessageDeleted = class;
  THistoryMessageDeletedArray = Array of THistoryMessageDeleted;
  TLabel = class;
  TLabelArray = Array of TLabel;
  TListDraftsResponse = class;
  TListDraftsResponseArray = Array of TListDraftsResponse;
  TListDraftsResponsedrafts = class;
  TListDraftsResponsedraftsArray = Array of TListDraftsResponsedrafts;
  TListHistoryResponse = class;
  TListHistoryResponseArray = Array of TListHistoryResponse;
  TListHistoryResponsehistory = class;
  TListHistoryResponsehistoryArray = Array of TListHistoryResponsehistory;
  TListLabelsResponse = class;
  TListLabelsResponseArray = Array of TListLabelsResponse;
  TListLabelsResponselabels = class;
  TListLabelsResponselabelsArray = Array of TListLabelsResponselabels;
  TListMessagesResponse = class;
  TListMessagesResponseArray = Array of TListMessagesResponse;
  TListMessagesResponsemessages = class;
  TListMessagesResponsemessagesArray = Array of TListMessagesResponsemessages;
  TListThreadsResponse = class;
  TListThreadsResponseArray = Array of TListThreadsResponse;
  TListThreadsResponsethreads = class;
  TListThreadsResponsethreadsArray = Array of TListThreadsResponsethreads;
  TMessage = class;
  TMessageArray = Array of TMessage;
  TMessagelabelIds = class;
  TMessagelabelIdsArray = Array of TMessagelabelIds;
  TMessagePart = class;
  TMessagePartArray = Array of TMessagePart;
  TMessagePartheaders = class;
  TMessagePartheadersArray = Array of TMessagePartheaders;
  TMessagePartparts = class;
  TMessagePartpartsArray = Array of TMessagePartparts;
  TMessagePartBody = class;
  TMessagePartBodyArray = Array of TMessagePartBody;
  TMessagePartHeader = class;
  TMessagePartHeaderArray = Array of TMessagePartHeader;
  TModifyMessageRequest = class;
  TModifyMessageRequestArray = Array of TModifyMessageRequest;
  TModifyMessageRequestaddLabelIds = class;
  TModifyMessageRequestaddLabelIdsArray = Array of TModifyMessageRequestaddLabelIds;
  TModifyMessageRequestremoveLabelIds = class;
  TModifyMessageRequestremoveLabelIdsArray = Array of TModifyMessageRequestremoveLabelIds;
  TModifyThreadRequest = class;
  TModifyThreadRequestArray = Array of TModifyThreadRequest;
  TModifyThreadRequestaddLabelIds = class;
  TModifyThreadRequestaddLabelIdsArray = Array of TModifyThreadRequestaddLabelIds;
  TModifyThreadRequestremoveLabelIds = class;
  TModifyThreadRequestremoveLabelIdsArray = Array of TModifyThreadRequestremoveLabelIds;
  TProfile = class;
  TProfileArray = Array of TProfile;
  TThread = class;
  TThreadArray = Array of TThread;
  TThreadmessages = class;
  TThreadmessagesArray = Array of TThreadmessages;
  
  { --------------------------------------------------------------------
    TDraft
    --------------------------------------------------------------------}
  
  TDraft = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fmessage : TMessage;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : TMessage); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property message : TMessage Index 8 Read Fmessage Write Setmessage;
  end;
  TDraftClass = Class of TDraft;
  
  { --------------------------------------------------------------------
    THistory
    --------------------------------------------------------------------}
  
  THistory = Class(TGoogleBaseObject)
  Private
    Fid : string;
    FlabelsAdded : THistorylabelsAdded;
    FlabelsRemoved : THistorylabelsRemoved;
    Fmessages : THistorymessages;
    FmessagesAdded : THistorymessagesAdded;
    FmessagesDeleted : THistorymessagesDeleted;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetlabelsAdded(AIndex : Integer; AValue : THistorylabelsAdded); virtual;
    Procedure SetlabelsRemoved(AIndex : Integer; AValue : THistorylabelsRemoved); virtual;
    Procedure Setmessages(AIndex : Integer; AValue : THistorymessages); virtual;
    Procedure SetmessagesAdded(AIndex : Integer; AValue : THistorymessagesAdded); virtual;
    Procedure SetmessagesDeleted(AIndex : Integer; AValue : THistorymessagesDeleted); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property labelsAdded : THistorylabelsAdded Index 8 Read FlabelsAdded Write SetlabelsAdded;
    Property labelsRemoved : THistorylabelsRemoved Index 16 Read FlabelsRemoved Write SetlabelsRemoved;
    Property messages : THistorymessages Index 24 Read Fmessages Write Setmessages;
    Property messagesAdded : THistorymessagesAdded Index 32 Read FmessagesAdded Write SetmessagesAdded;
    Property messagesDeleted : THistorymessagesDeleted Index 40 Read FmessagesDeleted Write SetmessagesDeleted;
  end;
  THistoryClass = Class of THistory;
  
  { --------------------------------------------------------------------
    THistorylabelsAdded
    --------------------------------------------------------------------}
  
  THistorylabelsAdded = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  THistorylabelsAddedClass = Class of THistorylabelsAdded;
  
  { --------------------------------------------------------------------
    THistorylabelsRemoved
    --------------------------------------------------------------------}
  
  THistorylabelsRemoved = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  THistorylabelsRemovedClass = Class of THistorylabelsRemoved;
  
  { --------------------------------------------------------------------
    THistorymessages
    --------------------------------------------------------------------}
  
  THistorymessages = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  THistorymessagesClass = Class of THistorymessages;
  
  { --------------------------------------------------------------------
    THistorymessagesAdded
    --------------------------------------------------------------------}
  
  THistorymessagesAdded = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  THistorymessagesAddedClass = Class of THistorymessagesAdded;
  
  { --------------------------------------------------------------------
    THistorymessagesDeleted
    --------------------------------------------------------------------}
  
  THistorymessagesDeleted = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  THistorymessagesDeletedClass = Class of THistorymessagesDeleted;
  
  { --------------------------------------------------------------------
    THistoryLabelAdded
    --------------------------------------------------------------------}
  
  THistoryLabelAdded = Class(TGoogleBaseObject)
  Private
    FlabelIds : THistoryLabelAddedlabelIds;
    Fmessage : TMessage;
  Protected
    //Property setters
    Procedure SetlabelIds(AIndex : Integer; AValue : THistoryLabelAddedlabelIds); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : TMessage); virtual;
  Public
  Published
    Property labelIds : THistoryLabelAddedlabelIds Index 0 Read FlabelIds Write SetlabelIds;
    Property message : TMessage Index 8 Read Fmessage Write Setmessage;
  end;
  THistoryLabelAddedClass = Class of THistoryLabelAdded;
  
  { --------------------------------------------------------------------
    THistoryLabelAddedlabelIds
    --------------------------------------------------------------------}
  
  THistoryLabelAddedlabelIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  THistoryLabelAddedlabelIdsClass = Class of THistoryLabelAddedlabelIds;
  
  { --------------------------------------------------------------------
    THistoryLabelRemoved
    --------------------------------------------------------------------}
  
  THistoryLabelRemoved = Class(TGoogleBaseObject)
  Private
    FlabelIds : THistoryLabelRemovedlabelIds;
    Fmessage : TMessage;
  Protected
    //Property setters
    Procedure SetlabelIds(AIndex : Integer; AValue : THistoryLabelRemovedlabelIds); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : TMessage); virtual;
  Public
  Published
    Property labelIds : THistoryLabelRemovedlabelIds Index 0 Read FlabelIds Write SetlabelIds;
    Property message : TMessage Index 8 Read Fmessage Write Setmessage;
  end;
  THistoryLabelRemovedClass = Class of THistoryLabelRemoved;
  
  { --------------------------------------------------------------------
    THistoryLabelRemovedlabelIds
    --------------------------------------------------------------------}
  
  THistoryLabelRemovedlabelIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  THistoryLabelRemovedlabelIdsClass = Class of THistoryLabelRemovedlabelIds;
  
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
    Fid : string;
    FlabelListVisibility : string;
    FmessageListVisibility : string;
    FmessagesTotal : integer;
    FmessagesUnread : integer;
    Fname : string;
    FthreadsTotal : integer;
    FthreadsUnread : integer;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetlabelListVisibility(AIndex : Integer; AValue : string); virtual;
    Procedure SetmessageListVisibility(AIndex : Integer; AValue : string); virtual;
    Procedure SetmessagesTotal(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmessagesUnread(AIndex : Integer; AValue : integer); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetthreadsTotal(AIndex : Integer; AValue : integer); virtual;
    Procedure SetthreadsUnread(AIndex : Integer; AValue : integer); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property labelListVisibility : string Index 8 Read FlabelListVisibility Write SetlabelListVisibility;
    Property messageListVisibility : string Index 16 Read FmessageListVisibility Write SetmessageListVisibility;
    Property messagesTotal : integer Index 24 Read FmessagesTotal Write SetmessagesTotal;
    Property messagesUnread : integer Index 32 Read FmessagesUnread Write SetmessagesUnread;
    Property name : string Index 40 Read Fname Write Setname;
    Property threadsTotal : integer Index 48 Read FthreadsTotal Write SetthreadsTotal;
    Property threadsUnread : integer Index 56 Read FthreadsUnread Write SetthreadsUnread;
    Property _type : string Index 64 Read F_type Write Set_type;
  end;
  TLabelClass = Class of TLabel;
  
  { --------------------------------------------------------------------
    TListDraftsResponse
    --------------------------------------------------------------------}
  
  TListDraftsResponse = Class(TGoogleBaseObject)
  Private
    Fdrafts : TListDraftsResponsedrafts;
    FnextPageToken : string;
    FresultSizeEstimate : integer;
  Protected
    //Property setters
    Procedure Setdrafts(AIndex : Integer; AValue : TListDraftsResponsedrafts); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetresultSizeEstimate(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property drafts : TListDraftsResponsedrafts Index 0 Read Fdrafts Write Setdrafts;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
    Property resultSizeEstimate : integer Index 16 Read FresultSizeEstimate Write SetresultSizeEstimate;
  end;
  TListDraftsResponseClass = Class of TListDraftsResponse;
  
  { --------------------------------------------------------------------
    TListDraftsResponsedrafts
    --------------------------------------------------------------------}
  
  TListDraftsResponsedrafts = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListDraftsResponsedraftsClass = Class of TListDraftsResponsedrafts;
  
  { --------------------------------------------------------------------
    TListHistoryResponse
    --------------------------------------------------------------------}
  
  TListHistoryResponse = Class(TGoogleBaseObject)
  Private
    Fhistory : TListHistoryResponsehistory;
    FhistoryId : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Sethistory(AIndex : Integer; AValue : TListHistoryResponsehistory); virtual;
    Procedure SethistoryId(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property history : TListHistoryResponsehistory Index 0 Read Fhistory Write Sethistory;
    Property historyId : string Index 8 Read FhistoryId Write SethistoryId;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TListHistoryResponseClass = Class of TListHistoryResponse;
  
  { --------------------------------------------------------------------
    TListHistoryResponsehistory
    --------------------------------------------------------------------}
  
  TListHistoryResponsehistory = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListHistoryResponsehistoryClass = Class of TListHistoryResponsehistory;
  
  { --------------------------------------------------------------------
    TListLabelsResponse
    --------------------------------------------------------------------}
  
  TListLabelsResponse = Class(TGoogleBaseObject)
  Private
    Flabels : TListLabelsResponselabels;
  Protected
    //Property setters
    Procedure Setlabels(AIndex : Integer; AValue : TListLabelsResponselabels); virtual;
  Public
  Published
    Property labels : TListLabelsResponselabels Index 0 Read Flabels Write Setlabels;
  end;
  TListLabelsResponseClass = Class of TListLabelsResponse;
  
  { --------------------------------------------------------------------
    TListLabelsResponselabels
    --------------------------------------------------------------------}
  
  TListLabelsResponselabels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListLabelsResponselabelsClass = Class of TListLabelsResponselabels;
  
  { --------------------------------------------------------------------
    TListMessagesResponse
    --------------------------------------------------------------------}
  
  TListMessagesResponse = Class(TGoogleBaseObject)
  Private
    Fmessages : TListMessagesResponsemessages;
    FnextPageToken : string;
    FresultSizeEstimate : integer;
  Protected
    //Property setters
    Procedure Setmessages(AIndex : Integer; AValue : TListMessagesResponsemessages); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetresultSizeEstimate(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property messages : TListMessagesResponsemessages Index 0 Read Fmessages Write Setmessages;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
    Property resultSizeEstimate : integer Index 16 Read FresultSizeEstimate Write SetresultSizeEstimate;
  end;
  TListMessagesResponseClass = Class of TListMessagesResponse;
  
  { --------------------------------------------------------------------
    TListMessagesResponsemessages
    --------------------------------------------------------------------}
  
  TListMessagesResponsemessages = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListMessagesResponsemessagesClass = Class of TListMessagesResponsemessages;
  
  { --------------------------------------------------------------------
    TListThreadsResponse
    --------------------------------------------------------------------}
  
  TListThreadsResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : string;
    FresultSizeEstimate : integer;
    Fthreads : TListThreadsResponsethreads;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetresultSizeEstimate(AIndex : Integer; AValue : integer); virtual;
    Procedure Setthreads(AIndex : Integer; AValue : TListThreadsResponsethreads); virtual;
  Public
  Published
    Property nextPageToken : string Index 0 Read FnextPageToken Write SetnextPageToken;
    Property resultSizeEstimate : integer Index 8 Read FresultSizeEstimate Write SetresultSizeEstimate;
    Property threads : TListThreadsResponsethreads Index 16 Read Fthreads Write Setthreads;
  end;
  TListThreadsResponseClass = Class of TListThreadsResponse;
  
  { --------------------------------------------------------------------
    TListThreadsResponsethreads
    --------------------------------------------------------------------}
  
  TListThreadsResponsethreads = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListThreadsResponsethreadsClass = Class of TListThreadsResponsethreads;
  
  { --------------------------------------------------------------------
    TMessage
    --------------------------------------------------------------------}
  
  TMessage = Class(TGoogleBaseObject)
  Private
    FhistoryId : string;
    Fid : string;
    FlabelIds : TMessagelabelIds;
    Fpayload : TMessagePart;
    Fraw : string;
    FsizeEstimate : integer;
    Fsnippet : string;
    FthreadId : string;
  Protected
    //Property setters
    Procedure SethistoryId(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetlabelIds(AIndex : Integer; AValue : TMessagelabelIds); virtual;
    Procedure Setpayload(AIndex : Integer; AValue : TMessagePart); virtual;
    Procedure Setraw(AIndex : Integer; AValue : string); virtual;
    Procedure SetsizeEstimate(AIndex : Integer; AValue : integer); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : string); virtual;
    Procedure SetthreadId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property historyId : string Index 0 Read FhistoryId Write SethistoryId;
    Property id : string Index 8 Read Fid Write Setid;
    Property labelIds : TMessagelabelIds Index 16 Read FlabelIds Write SetlabelIds;
    Property payload : TMessagePart Index 24 Read Fpayload Write Setpayload;
    Property raw : string Index 32 Read Fraw Write Setraw;
    Property sizeEstimate : integer Index 40 Read FsizeEstimate Write SetsizeEstimate;
    Property snippet : string Index 48 Read Fsnippet Write Setsnippet;
    Property threadId : string Index 56 Read FthreadId Write SetthreadId;
  end;
  TMessageClass = Class of TMessage;
  
  { --------------------------------------------------------------------
    TMessagelabelIds
    --------------------------------------------------------------------}
  
  TMessagelabelIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMessagelabelIdsClass = Class of TMessagelabelIds;
  
  { --------------------------------------------------------------------
    TMessagePart
    --------------------------------------------------------------------}
  
  TMessagePart = Class(TGoogleBaseObject)
  Private
    Fbody : TMessagePartBody;
    Ffilename : string;
    Fheaders : TMessagePartheaders;
    FmimeType : string;
    FpartId : string;
    Fparts : TMessagePartparts;
  Protected
    //Property setters
    Procedure Setbody(AIndex : Integer; AValue : TMessagePartBody); virtual;
    Procedure Setfilename(AIndex : Integer; AValue : string); virtual;
    Procedure Setheaders(AIndex : Integer; AValue : TMessagePartheaders); virtual;
    Procedure SetmimeType(AIndex : Integer; AValue : string); virtual;
    Procedure SetpartId(AIndex : Integer; AValue : string); virtual;
    Procedure Setparts(AIndex : Integer; AValue : TMessagePartparts); virtual;
  Public
  Published
    Property body : TMessagePartBody Index 0 Read Fbody Write Setbody;
    Property filename : string Index 8 Read Ffilename Write Setfilename;
    Property headers : TMessagePartheaders Index 16 Read Fheaders Write Setheaders;
    Property mimeType : string Index 24 Read FmimeType Write SetmimeType;
    Property partId : string Index 32 Read FpartId Write SetpartId;
    Property parts : TMessagePartparts Index 40 Read Fparts Write Setparts;
  end;
  TMessagePartClass = Class of TMessagePart;
  
  { --------------------------------------------------------------------
    TMessagePartheaders
    --------------------------------------------------------------------}
  
  TMessagePartheaders = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMessagePartheadersClass = Class of TMessagePartheaders;
  
  { --------------------------------------------------------------------
    TMessagePartparts
    --------------------------------------------------------------------}
  
  TMessagePartparts = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMessagePartpartsClass = Class of TMessagePartparts;
  
  { --------------------------------------------------------------------
    TMessagePartBody
    --------------------------------------------------------------------}
  
  TMessagePartBody = Class(TGoogleBaseObject)
  Private
    FattachmentId : string;
    Fdata : string;
    Fsize : integer;
  Protected
    //Property setters
    Procedure SetattachmentId(AIndex : Integer; AValue : string); virtual;
    Procedure Setdata(AIndex : Integer; AValue : string); virtual;
    Procedure Setsize(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property attachmentId : string Index 0 Read FattachmentId Write SetattachmentId;
    Property data : string Index 8 Read Fdata Write Setdata;
    Property size : integer Index 16 Read Fsize Write Setsize;
  end;
  TMessagePartBodyClass = Class of TMessagePartBody;
  
  { --------------------------------------------------------------------
    TMessagePartHeader
    --------------------------------------------------------------------}
  
  TMessagePartHeader = Class(TGoogleBaseObject)
  Private
    Fname : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property name : string Index 0 Read Fname Write Setname;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TMessagePartHeaderClass = Class of TMessagePartHeader;
  
  { --------------------------------------------------------------------
    TModifyMessageRequest
    --------------------------------------------------------------------}
  
  TModifyMessageRequest = Class(TGoogleBaseObject)
  Private
    FaddLabelIds : TModifyMessageRequestaddLabelIds;
    FremoveLabelIds : TModifyMessageRequestremoveLabelIds;
  Protected
    //Property setters
    Procedure SetaddLabelIds(AIndex : Integer; AValue : TModifyMessageRequestaddLabelIds); virtual;
    Procedure SetremoveLabelIds(AIndex : Integer; AValue : TModifyMessageRequestremoveLabelIds); virtual;
  Public
  Published
    Property addLabelIds : TModifyMessageRequestaddLabelIds Index 0 Read FaddLabelIds Write SetaddLabelIds;
    Property removeLabelIds : TModifyMessageRequestremoveLabelIds Index 8 Read FremoveLabelIds Write SetremoveLabelIds;
  end;
  TModifyMessageRequestClass = Class of TModifyMessageRequest;
  
  { --------------------------------------------------------------------
    TModifyMessageRequestaddLabelIds
    --------------------------------------------------------------------}
  
  TModifyMessageRequestaddLabelIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TModifyMessageRequestaddLabelIdsClass = Class of TModifyMessageRequestaddLabelIds;
  
  { --------------------------------------------------------------------
    TModifyMessageRequestremoveLabelIds
    --------------------------------------------------------------------}
  
  TModifyMessageRequestremoveLabelIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TModifyMessageRequestremoveLabelIdsClass = Class of TModifyMessageRequestremoveLabelIds;
  
  { --------------------------------------------------------------------
    TModifyThreadRequest
    --------------------------------------------------------------------}
  
  TModifyThreadRequest = Class(TGoogleBaseObject)
  Private
    FaddLabelIds : TModifyThreadRequestaddLabelIds;
    FremoveLabelIds : TModifyThreadRequestremoveLabelIds;
  Protected
    //Property setters
    Procedure SetaddLabelIds(AIndex : Integer; AValue : TModifyThreadRequestaddLabelIds); virtual;
    Procedure SetremoveLabelIds(AIndex : Integer; AValue : TModifyThreadRequestremoveLabelIds); virtual;
  Public
  Published
    Property addLabelIds : TModifyThreadRequestaddLabelIds Index 0 Read FaddLabelIds Write SetaddLabelIds;
    Property removeLabelIds : TModifyThreadRequestremoveLabelIds Index 8 Read FremoveLabelIds Write SetremoveLabelIds;
  end;
  TModifyThreadRequestClass = Class of TModifyThreadRequest;
  
  { --------------------------------------------------------------------
    TModifyThreadRequestaddLabelIds
    --------------------------------------------------------------------}
  
  TModifyThreadRequestaddLabelIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TModifyThreadRequestaddLabelIdsClass = Class of TModifyThreadRequestaddLabelIds;
  
  { --------------------------------------------------------------------
    TModifyThreadRequestremoveLabelIds
    --------------------------------------------------------------------}
  
  TModifyThreadRequestremoveLabelIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TModifyThreadRequestremoveLabelIdsClass = Class of TModifyThreadRequestremoveLabelIds;
  
  { --------------------------------------------------------------------
    TProfile
    --------------------------------------------------------------------}
  
  TProfile = Class(TGoogleBaseObject)
  Private
    FemailAddress : string;
    FhistoryId : string;
    FmessagesTotal : integer;
    FthreadsTotal : integer;
  Protected
    //Property setters
    Procedure SetemailAddress(AIndex : Integer; AValue : string); virtual;
    Procedure SethistoryId(AIndex : Integer; AValue : string); virtual;
    Procedure SetmessagesTotal(AIndex : Integer; AValue : integer); virtual;
    Procedure SetthreadsTotal(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property emailAddress : string Index 0 Read FemailAddress Write SetemailAddress;
    Property historyId : string Index 8 Read FhistoryId Write SethistoryId;
    Property messagesTotal : integer Index 16 Read FmessagesTotal Write SetmessagesTotal;
    Property threadsTotal : integer Index 24 Read FthreadsTotal Write SetthreadsTotal;
  end;
  TProfileClass = Class of TProfile;
  
  { --------------------------------------------------------------------
    TThread
    --------------------------------------------------------------------}
  
  TThread = Class(TGoogleBaseObject)
  Private
    FhistoryId : string;
    Fid : string;
    Fmessages : TThreadmessages;
    Fsnippet : string;
  Protected
    //Property setters
    Procedure SethistoryId(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setmessages(AIndex : Integer; AValue : TThreadmessages); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property historyId : string Index 0 Read FhistoryId Write SethistoryId;
    Property id : string Index 8 Read Fid Write Setid;
    Property messages : TThreadmessages Index 16 Read Fmessages Write Setmessages;
    Property snippet : string Index 24 Read Fsnippet Write Setsnippet;
  end;
  TThreadClass = Class of TThread;
  
  { --------------------------------------------------------------------
    TThreadmessages
    --------------------------------------------------------------------}
  
  TThreadmessages = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TThreadmessagesClass = Class of TThreadmessages;
  
  { --------------------------------------------------------------------
    TUsersResource
    --------------------------------------------------------------------}
  
  TUsersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function GetProfile(userId: string) : TProfile;
  end;
  
  
  { --------------------------------------------------------------------
    TGmailAPI
    --------------------------------------------------------------------}
  
  TGmailAPI = Class(TGoogleAPI)
  Private
    FUsersInstance : TUsersResource;
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
    Function CreateUsersResource(AOwner : TComponent) : TUsersResource;virtual;overload;
    Function CreateUsersResource : TUsersResource;virtual;overload;
    //Add default on-demand instances for resources
    Property UsersResource : TUsersResource Read GetUsersInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TDraft
  --------------------------------------------------------------------}


Procedure TDraft.Setid(AIndex : Integer; AValue : string); 

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


Procedure THistory.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THistory.SetlabelsAdded(AIndex : Integer; AValue : THistorylabelsAdded); 

begin
  If (FlabelsAdded=AValue) then exit;
  FlabelsAdded:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THistory.SetlabelsRemoved(AIndex : Integer; AValue : THistorylabelsRemoved); 

begin
  If (FlabelsRemoved=AValue) then exit;
  FlabelsRemoved:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THistory.Setmessages(AIndex : Integer; AValue : THistorymessages); 

begin
  If (Fmessages=AValue) then exit;
  Fmessages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THistory.SetmessagesAdded(AIndex : Integer; AValue : THistorymessagesAdded); 

begin
  If (FmessagesAdded=AValue) then exit;
  FmessagesAdded:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THistory.SetmessagesDeleted(AIndex : Integer; AValue : THistorymessagesDeleted); 

begin
  If (FmessagesDeleted=AValue) then exit;
  FmessagesDeleted:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  THistorylabelsAdded
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  THistorylabelsRemoved
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  THistorymessages
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  THistorymessagesAdded
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  THistorymessagesDeleted
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  THistoryLabelAdded
  --------------------------------------------------------------------}


Procedure THistoryLabelAdded.SetlabelIds(AIndex : Integer; AValue : THistoryLabelAddedlabelIds); 

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





{ --------------------------------------------------------------------
  THistoryLabelAddedlabelIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  THistoryLabelRemoved
  --------------------------------------------------------------------}


Procedure THistoryLabelRemoved.SetlabelIds(AIndex : Integer; AValue : THistoryLabelRemovedlabelIds); 

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





{ --------------------------------------------------------------------
  THistoryLabelRemovedlabelIds
  --------------------------------------------------------------------}




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


Procedure TLabel.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLabel.SetlabelListVisibility(AIndex : Integer; AValue : string); 

begin
  If (FlabelListVisibility=AValue) then exit;
  FlabelListVisibility:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLabel.SetmessageListVisibility(AIndex : Integer; AValue : string); 

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



Procedure TLabel.Setname(AIndex : Integer; AValue : string); 

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



Procedure TLabel.Set_type(AIndex : Integer; AValue : string); 

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


Procedure TListDraftsResponse.Setdrafts(AIndex : Integer; AValue : TListDraftsResponsedrafts); 

begin
  If (Fdrafts=AValue) then exit;
  Fdrafts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListDraftsResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

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





{ --------------------------------------------------------------------
  TListDraftsResponsedrafts
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListHistoryResponse
  --------------------------------------------------------------------}


Procedure TListHistoryResponse.Sethistory(AIndex : Integer; AValue : TListHistoryResponsehistory); 

begin
  If (Fhistory=AValue) then exit;
  Fhistory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListHistoryResponse.SethistoryId(AIndex : Integer; AValue : string); 

begin
  If (FhistoryId=AValue) then exit;
  FhistoryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListHistoryResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListHistoryResponsehistory
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListLabelsResponse
  --------------------------------------------------------------------}


Procedure TListLabelsResponse.Setlabels(AIndex : Integer; AValue : TListLabelsResponselabels); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListLabelsResponselabels
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListMessagesResponse
  --------------------------------------------------------------------}


Procedure TListMessagesResponse.Setmessages(AIndex : Integer; AValue : TListMessagesResponsemessages); 

begin
  If (Fmessages=AValue) then exit;
  Fmessages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListMessagesResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

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





{ --------------------------------------------------------------------
  TListMessagesResponsemessages
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListThreadsResponse
  --------------------------------------------------------------------}


Procedure TListThreadsResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

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



Procedure TListThreadsResponse.Setthreads(AIndex : Integer; AValue : TListThreadsResponsethreads); 

begin
  If (Fthreads=AValue) then exit;
  Fthreads:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListThreadsResponsethreads
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMessage
  --------------------------------------------------------------------}


Procedure TMessage.SethistoryId(AIndex : Integer; AValue : string); 

begin
  If (FhistoryId=AValue) then exit;
  FhistoryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMessage.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMessage.SetlabelIds(AIndex : Integer; AValue : TMessagelabelIds); 

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



Procedure TMessage.Setraw(AIndex : Integer; AValue : string); 

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



Procedure TMessage.Setsnippet(AIndex : Integer; AValue : string); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMessage.SetthreadId(AIndex : Integer; AValue : string); 

begin
  If (FthreadId=AValue) then exit;
  FthreadId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMessagelabelIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMessagePart
  --------------------------------------------------------------------}


Procedure TMessagePart.Setbody(AIndex : Integer; AValue : TMessagePartBody); 

begin
  If (Fbody=AValue) then exit;
  Fbody:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMessagePart.Setfilename(AIndex : Integer; AValue : string); 

begin
  If (Ffilename=AValue) then exit;
  Ffilename:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMessagePart.Setheaders(AIndex : Integer; AValue : TMessagePartheaders); 

begin
  If (Fheaders=AValue) then exit;
  Fheaders:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMessagePart.SetmimeType(AIndex : Integer; AValue : string); 

begin
  If (FmimeType=AValue) then exit;
  FmimeType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMessagePart.SetpartId(AIndex : Integer; AValue : string); 

begin
  If (FpartId=AValue) then exit;
  FpartId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMessagePart.Setparts(AIndex : Integer; AValue : TMessagePartparts); 

begin
  If (Fparts=AValue) then exit;
  Fparts:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMessagePartheaders
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMessagePartparts
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMessagePartBody
  --------------------------------------------------------------------}


Procedure TMessagePartBody.SetattachmentId(AIndex : Integer; AValue : string); 

begin
  If (FattachmentId=AValue) then exit;
  FattachmentId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMessagePartBody.Setdata(AIndex : Integer; AValue : string); 

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


Procedure TMessagePartHeader.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMessagePartHeader.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TModifyMessageRequest
  --------------------------------------------------------------------}


Procedure TModifyMessageRequest.SetaddLabelIds(AIndex : Integer; AValue : TModifyMessageRequestaddLabelIds); 

begin
  If (FaddLabelIds=AValue) then exit;
  FaddLabelIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TModifyMessageRequest.SetremoveLabelIds(AIndex : Integer; AValue : TModifyMessageRequestremoveLabelIds); 

begin
  If (FremoveLabelIds=AValue) then exit;
  FremoveLabelIds:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TModifyMessageRequestaddLabelIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TModifyMessageRequestremoveLabelIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TModifyThreadRequest
  --------------------------------------------------------------------}


Procedure TModifyThreadRequest.SetaddLabelIds(AIndex : Integer; AValue : TModifyThreadRequestaddLabelIds); 

begin
  If (FaddLabelIds=AValue) then exit;
  FaddLabelIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TModifyThreadRequest.SetremoveLabelIds(AIndex : Integer; AValue : TModifyThreadRequestremoveLabelIds); 

begin
  If (FremoveLabelIds=AValue) then exit;
  FremoveLabelIds:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TModifyThreadRequestaddLabelIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TModifyThreadRequestremoveLabelIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TProfile
  --------------------------------------------------------------------}


Procedure TProfile.SetemailAddress(AIndex : Integer; AValue : string); 

begin
  If (FemailAddress=AValue) then exit;
  FemailAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProfile.SethistoryId(AIndex : Integer; AValue : string); 

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


Procedure TThread.SethistoryId(AIndex : Integer; AValue : string); 

begin
  If (FhistoryId=AValue) then exit;
  FhistoryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThread.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThread.Setmessages(AIndex : Integer; AValue : TThreadmessages); 

begin
  If (Fmessages=AValue) then exit;
  Fmessages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThread.Setsnippet(AIndex : Integer; AValue : string); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TThreadmessages
  --------------------------------------------------------------------}




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
  Result:='20150303';
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
  Result:='https://www.googleapis.com/';
end;

Class Function TGmailAPI.APIbasePath : string;

begin
  Result:='/gmail/v1/users/';
end;

Class Function TGmailAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/gmail/v1/users/';
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
  THistorylabelsAdded.RegisterObject;
  THistorylabelsRemoved.RegisterObject;
  THistorymessages.RegisterObject;
  THistorymessagesAdded.RegisterObject;
  THistorymessagesDeleted.RegisterObject;
  THistoryLabelAdded.RegisterObject;
  THistoryLabelAddedlabelIds.RegisterObject;
  THistoryLabelRemoved.RegisterObject;
  THistoryLabelRemovedlabelIds.RegisterObject;
  THistoryMessageAdded.RegisterObject;
  THistoryMessageDeleted.RegisterObject;
  TLabel.RegisterObject;
  TListDraftsResponse.RegisterObject;
  TListDraftsResponsedrafts.RegisterObject;
  TListHistoryResponse.RegisterObject;
  TListHistoryResponsehistory.RegisterObject;
  TListLabelsResponse.RegisterObject;
  TListLabelsResponselabels.RegisterObject;
  TListMessagesResponse.RegisterObject;
  TListMessagesResponsemessages.RegisterObject;
  TListThreadsResponse.RegisterObject;
  TListThreadsResponsethreads.RegisterObject;
  TMessage.RegisterObject;
  TMessagelabelIds.RegisterObject;
  TMessagePart.RegisterObject;
  TMessagePartheaders.RegisterObject;
  TMessagePartparts.RegisterObject;
  TMessagePartBody.RegisterObject;
  TMessagePartHeader.RegisterObject;
  TModifyMessageRequest.RegisterObject;
  TModifyMessageRequestaddLabelIds.RegisterObject;
  TModifyMessageRequestremoveLabelIds.RegisterObject;
  TModifyThreadRequest.RegisterObject;
  TModifyThreadRequestaddLabelIds.RegisterObject;
  TModifyThreadRequestremoveLabelIds.RegisterObject;
  TProfile.RegisterObject;
  TThread.RegisterObject;
  TThreadmessages.RegisterObject;
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
  Result.API:=Self;
end;



initialization
  TGmailAPI.RegisterAPI;
end.
