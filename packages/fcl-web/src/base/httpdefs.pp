{
    $Id: header,v 1.1 2000/07/13 06:33:45 michael Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{

    HTTPDefs: Basic HTTP protocol declarations and classes

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$mode objfpc}
{$H+}
{ $DEFINE CGIDEBUG}
unit HTTPDefs;

interface

uses Classes,Sysutils;

const
  DefaultTimeOut = 15;
  SFPWebSession  = 'FPWebSession'; // Cookie name for session.

  fieldAccept          = 'Accept';
  fieldAcceptCharset   = 'Accept-Charset';
  fieldAcceptEncoding  = 'Accept-Encoding';
  fieldAcceptLanguage  = 'Accept-Language';
  fieldAuthorization   = 'Authorization';
  fieldConnection      = 'Connection';
  fieldContentEncoding = 'Content-Encoding';
  fieldContentLanguage = 'Content-Language';
  fieldContentLength   = 'Content-Length';
  fieldContentType     = 'Content-Type';
  fieldCookie          = 'Cookie';
  fieldDate            = 'Date';
  fieldExpires         = 'Expires';
  fieldFrom            = 'From';
  fieldIfModifiedSince = 'If-Modified-Since';
  fieldLastModified    = 'Last-Modified';
  fieldLocation        = 'Location';
  fieldPragma          = 'Pragma';
  fieldReferer         = 'Referer';
  fieldRetryAfter      = 'Retry-After';
  fieldServer          = 'Server';
  fieldSetCookie       = 'Set-Cookie';
  fieldUserAgent       = 'User-Agent';
  fieldWWWAuthenticate = 'WWW-Authenticate';
  // These cannot be added to the NoHTTPFields constant
  // They are in the extra array.
  fieldHost            = 'Host';
  fieldCacheControl    = 'Cache-Control';
  fieldXRequestedWith  = 'X-Requested-With';

  NoHTTPFields    = 27;

  HTTPDateFmt     = '"%s", dd "%s" yyyy hh:mm:ss'; // For use in FormatDateTime
  SCookieExpire   = ' "Expires="'+HTTPDateFmt+' "GMT"';
  SCookieDomain   = ' Domain=%s';
  SCookiePath     = ' Path=%s';
  SCookieSecure   = ' Secure';
  SCookieHttpOnly = ' HttpOnly';

  HTTPMonths: array[1..12] of string[3] = (
    'Jan', 'Feb', 'Mar', 'Apr',
    'May', 'Jun', 'Jul', 'Aug',
    'Sep', 'Oct', 'Nov', 'Dec');
  HTTPDays: array[1..7] of string[3] = (
    'Sun', 'Mon', 'Tue', 'Wed',
    'Thu', 'Fri', 'Sat');


Type
  THttpFields  = Array[1..NoHTTPFields] of string;
  THttpIndexes = Array[1..NoHTTPFields] of integer;


Const
  // For this constant, the header names corresponds to the property index used in THTTPHeader.
  HTTPFieldNames : THttpFields
             = (fieldAccept, fieldAcceptCharset, fieldAcceptEncoding, 
                fieldAcceptLanguage, fieldAuthorization, fieldConnection,
                fieldContentEncoding, fieldContentLanguage, fieldContentLength,
                fieldContentType, fieldCookie, fieldDate, fieldExpires, 
                fieldFrom, fieldIfModifiedSince, fieldLastModified, fieldLocation,
                fieldPragma, fieldReferer, fieldRetryAfter, fieldServer, 
                fieldSetCookie, fieldUserAgent, fieldWWWAuthenticate,
                fieldHost, fieldCacheControl,fieldXRequestedWith);
  // Map header names on indexes in property getter/setter. 0 means not mapped !
  HTTPFieldIndexes : THTTPIndexes
                   =  (1,2,3,
                       4,5,6,
                       7,8,9,
                       10,11,12,13,
                       14,15,16,17,
                       18,19,20,21,
                       22,23,24,
                       34,0,36);



type
  TRequest = Class;

  { TCookie }

  TCookie = class(TCollectionItem)
  private
    FHttpOnly: Boolean;
    FName: string;
    FValue: string;
    FPath: string;
    FDomain: string;
    FExpires: TDateTime;
    FSecure: Boolean;
  protected
    Function GetAsString: string;
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    procedure Expire;
    property Name: string read FName write FName;
    property Value: string read FValue write FValue;
    property Domain: string read FDomain write FDomain;
    property Path: string read FPath write FPath;
    property Expires: TDateTime read FExpires write FExpires;
    property Secure: Boolean read FSecure write FSecure;
    property HttpOnly: Boolean read FHttpOnly write FHttpOnly;
    Property AsString : String Read GetAsString;
  end;

{ TCookies }

  TCookies = class(TCollection)
  private
  protected
    function GetCookie(Index: Integer): TCookie;
    procedure SetCookie(Index: Integer; Value: TCookie);
  public
    function  Add: TCookie;
    Function CookieByName(AName : String) : TCookie;
    Function FindCookie(AName : String): TCookie;
    Function IndexOfCookie(AName : String) : Integer;
    property Items[Index: Integer]: TCookie read GetCookie write SetCookie; default;
  end;

  { TUploadedFile }

  TUploadedFile = Class(TCollectionItem)
  Private
    FContentType: String;
    FDescription: String;
    FDisposition: String;
    FFieldName: String;
    FFileName: String;
    FLocalFileName: String;
    FSize: Int64;
    FStream : TStream;
  Protected
    // Note that this will free the file stream, to be able to close it - file is share deny write locked!
    Procedure DeleteTempUploadedFile; virtual;
    function GetStream: TStream; virtual;
    Procedure FreeStream; virtual;
  Public
    Destructor Destroy; override;
    Property FieldName : String Read FFieldName Write FFieldName;
    Property FileName : String Read FFileName Write FFileName;
    Property Stream : TStream Read GetStream;
    Property Size : Int64 Read FSize Write FSize;
    Property ContentType : String Read FContentType Write FContentType;
    Property Disposition : String Read FDisposition Write FDisposition;
    Property LocalFileName : String Read FLocalFileName Write FLocalFileName;
    Property Description : String Read FDescription Write FDescription;
  end;
  TUploadedFileClass = Class of TUploadedFile;

  { TUploadedFiles }

  TUploadedFiles = Class(TCollection)
  private
    FRequest : TRequest; // May be nil
    function GetFile(Index : Integer): TUploadedFile;
    procedure SetFile(Index : Integer; const AValue: TUploadedFile);
  Protected
    Function GetTempUploadFileName(Const AName, AFileName : String; ASize : Int64): String;
    Procedure DeleteTempUploadedFiles; virtual;
  public
    Function First : TUploadedFile;
    Function Last : TUploadedFile;
    Function IndexOfFile(AName : String) : Integer;
    Function FileByName(AName : String) : TUploadedFile;
    Function FindFile(AName : String) : TUploadedFile;
    Property Files[Index : Integer] : TUploadedFile read GetFile Write SetFile; default;
  end;
  TUploadedFilesClass = Class of TUploadedFiles;

  { TMimeItem }
  // Used to decode multipart encoded content

  TMimeItem = Class(TCollectionItem)
  private
  protected
    Function CreateUploadedFile(Files : TUploadedFiles) : TUploadedFile; virtual;
    Function ProcessHeader(Const AHeader,AValue : String) : Boolean; virtual;
    procedure SaveToFile(const AFileName: String); virtual;
    function GetIsFile: Boolean; virtual;
    // These must be implemented in descendents;
    function GetDataSize: Int64; virtual; abstract;
    function GetHeader(AIndex: Integer): String; virtual; abstract;
    Procedure SetHeader(AIndex: Integer; Const AValue: String); virtual; abstract;
  Public
    Procedure Process(Stream : TStream); virtual; abstract;
    Property Data : String index 0 Read GetHeader Write SetHeader;
    Property Name : String index 1 Read GetHeader Write SetHeader;
    Property Disposition : String index 2 Read GetHeader Write SetHeader;
    Property FileName : String index 3 Read GetHeader Write SetHeader;
    Property ContentType : String index 4 Read GetHeader Write SetHeader;
    Property Description : String index 5 Read GetHeader Write SetHeader;
    Property IsFile : Boolean  Read GetIsFile;
    Property DataSize : Int64 Read GetDataSize;
  end;
  TMimeItemClass = Class of TMimeItem;
  { TMimeItems }

  TMimeItems = Class(TCollection)
  private
    function GetP(AIndex : Integer): TMimeItem;
  Protected
    Procedure CreateUploadFiles(Files : TUploadedFiles; Vars : TStrings); virtual;
    procedure FormSplit(var Cnt: String; boundary: String); virtual;
  Public
    Function First : TMimeItem;
    Function Last : TMimeItem;
    Property Parts[AIndex : Integer] : TMimeItem Read GetP; default;
  end;
  TMimeItemsClass = Class of TMimeItems;

  { THTTPHeader }

  THTTPHeader = class(TObject)
  private
    FContentFields: TStrings;
    FCookieFields: TStrings;
    FHTTPVersion: String;
    FHTTPXRequestedWith: String;
    FFields : THttpFields;
    FQueryFields: TStrings;
    function GetSetField(AIndex: Integer): String;
    function GetSetFieldName(AIndex: Integer): String;
    procedure SetCookieFields(const AValue: TStrings);
    Function GetFieldCount : Integer;
    Function GetContentLength : Integer;
    Procedure SetContentLength(Value : Integer);
    Function GetFieldIndex(AIndex : Integer) : Integer;
    Function GetServerPort : Word;
    Procedure SetServerPort(AValue : Word);
    Function GetSetFieldValue(Index : Integer) : String; virtual;
  Protected
    Function GetFieldValue(Index : Integer) : String; virtual;
    Procedure SetFieldValue(Index : Integer; Value : String); virtual;
    procedure ParseFirstHeaderLine(const line: String);virtual;
    Procedure ParseCookies; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetFieldByName(const AName, AValue: String);
    function  GetFieldByName(const AName: String): String;
    Function LoadFromStream(Stream : TStream; IncludeCommand : Boolean) : integer;
    Function LoadFromStrings(Strings: TStrings; IncludeCommand : Boolean) : integer; virtual;
    // Common access
    // This is an internal table. We should try to get rid of it,
    // It requires a lot of duplication.
    property FieldCount: Integer read GetFieldCount;
    property Fields[AIndex: Integer]: String read GetSetField;
    property FieldNames[AIndex: Integer]: String read GetSetFieldName;
    property FieldValues[AIndex: Integer]: String read GetSetFieldValue;
    // Various properties.
    Property HttpVersion : String Index 0 Read GetFieldValue Write SetFieldValue;
    Property ProtocolVersion : String Index 0 Read GetFieldValue Write SetFieldValue;
    property Accept: String Index 1 read GetFieldValue write SetFieldValue;
    property AcceptCharset: String Index 2 Read GetFieldValue Write SetFieldValue;
    property AcceptEncoding: String Index 3 Read GetFieldValue Write SetFieldValue;
    property AcceptLanguage: String Index 4 Read GetFieldValue Write SetFieldValue;
    property Authorization: String Index 5 Read GetFieldValue Write SetFieldValue;
    property Connection: String Index 6 Read GetFieldValue Write SetFieldValue;
    property ContentEncoding: String Index 7 Read GetFieldValue Write SetFieldValue;
    property ContentLanguage: String Index 8 Read GetFieldValue Write SetFieldValue;
    property ContentLength: Integer Read GetContentLength Write SetContentLength; // Index 9
    property ContentType: String Index 10 Read GetFieldValue Write SetFieldValue;
    property Cookie: String Index 11 Read GetFieldValue Write SetFieldValue;
    property Date: String Index 12 Read GetFieldValue Write SetFieldValue;
    property Expires: String Index 13 Read GetFieldValue Write SetFieldValue;
    property From: String Index 14 Read GetFieldValue Write SetFieldValue;
    property IfModifiedSince: String Index 15 Read GetFieldValue Write SetFieldValue;
    property LastModified: String Index 16 Read GetFieldValue Write SetFieldValue;
    property Location: String Index 17 Read GetFieldValue Write SetFieldValue;
    property Pragma: String Index 18 Read GetFieldValue Write SetFieldValue;
    property Referer: String Index 19 Read GetFieldValue Write SetFieldValue;
    property RetryAfter: String Index 20 Read GetFieldValue Write SetFieldValue;
    property Server: String Index 21 Read GetFieldValue Write SetFieldValue;
    property SetCookie: String Index 22 Read GetFieldValue Write SetFieldValue;
    property UserAgent: String Index 23 Read GetFieldValue Write SetFieldValue;
    property WWWAuthenticate: String Index 24 Read GetFieldValue Write SetFieldValue;
    // Various aliases, for compatibility
    Property PathInfo : String index 25 read GetFieldValue Write SetFieldValue;
    Property PathTranslated : String Index 26 read GetFieldValue Write SetFieldValue;
    Property RemoteAddress : String Index 27 read GetFieldValue Write SetFieldValue;
    Property RemoteAddr : String Index 27 read GetFieldValue Write SetFieldValue; // Alias, Delphi-compat
    Property RemoteHost : String Index 28 read  GetFieldValue Write SetFieldValue;
    Property ScriptName : String Index 29 read  GetFieldValue Write SetFieldValue;
    Property ServerPort : Word Read GetServerPort Write SetServerPort; // Index 30
    Property HTTPAccept : String Index 1 read GetFieldValue Write SetFieldValue;
    Property HTTPAcceptCharset : String Index 2 read GetFieldValue Write SetFieldValue;
    Property HTTPAcceptEncoding : String Index 3 read GetFieldValue Write SetFieldValue;
    Property HTTPIfModifiedSince : String Index 15 read GetFieldValue Write SetFieldValue; // Maybe change to TDateTime ??
    Property HTTPReferer : String Index 19 read GetFieldValue Write SetFieldValue;
    Property HTTPUserAgent : String Index 23 read GetFieldValue Write SetFieldValue;
    Property Method : String Index 31 read GetFieldValue Write SetFieldValue;
    Property URL : String Index 32 read GetFieldValue Write SetFieldValue;
    Property Query : String Index 33 read GetFieldValue Write SetFieldValue;
    Property Host : String Index 34 Read GetFieldValue Write SetFieldValue;
    Property Content : String Index 35 Read GetFieldValue Write SetFieldValue;
    Property HTTPXRequestedWith : String Index 36 read GetFieldValue Write SetFieldValue;
    // Lists
    Property CookieFields : TStrings Read FCookieFields Write SetCookieFields;
    Property ContentFields: TStrings read FContentFields;
    property QueryFields : TStrings read FQueryFields;
  end;

  TOnUnknownEncodingEvent = Procedure (Sender : TRequest; Const ContentType : String;Stream : TStream) of object;
  { TRequest }

  TRequest = class(THttpHeader)
  private
    FCommand: String;
    FCommandLine: String;
    FHandleGetOnPost: Boolean;
    FOnUnknownEncoding: TOnUnknownEncodingEvent;
    FPathInfo,
    FHost : string;
    FRequestedWith : String;
    FURI: String;
    FFiles : TUploadedFiles;
    FReturnedPathInfo : String;
    FLocalPathPrefix : string;
    FServerPort : String;
    FContentRead : Boolean;
    FContent : String;
    function GetLocalPathPrefix: string;
    function GetFirstHeaderLine: String;
  Protected
    Function AllowReadContent : Boolean; virtual;
    Function CreateUploadedFiles : TUploadedFiles; virtual;
    Function CreateMimeItems : TMimeItems; virtual;
    procedure HandleUnknownEncoding(Const AContentType : String;Stream : TStream); virtual;
    procedure ParseFirstHeaderLine(const line: String);override;
    procedure ReadContent; virtual;
    Function GetFieldValue(AIndex : Integer) : String; override;
    Procedure SetFieldValue(Index : Integer; Value : String); override;
    Procedure ProcessMultiPart(Stream : TStream; Const Boundary : String;SL:TStrings); virtual;
    Procedure ProcessQueryString(Const FQueryString : String; SL:TStrings); virtual;
    procedure ProcessURLEncoded(Stream : TStream;SL:TStrings); virtual;
    Function RequestUploadDir : String; virtual;
    Function GetTempUploadFileName(Const AName, AFileName : String; ASize : Int64) : String; virtual;
    // This will free any TUPloadedFile.Streams that may exist, as they may lock the files and thus prevent them
    Procedure DeleteTempUploadedFiles; virtual;
    Procedure InitRequestVars; virtual;
    Procedure InitPostVars; virtual;
    Procedure InitGetVars; virtual;
    Procedure InitContent(Var AContent : String);
    Property ContentRead : Boolean Read FContentRead Write FContentRead;
  public
    constructor Create; override;
    destructor destroy; override;
    Function  GetNextPathInfo : String;
    Property  ReturnedPathInfo : String Read FReturnedPathInfo Write FReturnedPathInfo;
    Property  LocalPathPrefix : string Read GetLocalPathPrefix;
    Property  CommandLine : String Read FCommandLine;
    Property  Command : String read FCommand;
    Property  URI : String read FURI;                // Uniform Resource Identifier
    Property  QueryString : String Index 33 read GetFieldValue Write SetFieldValue; // Alias
    Property  HeaderLine : String read GetFirstHeaderLine;
    Property  Files : TUploadedFiles Read FFiles;
    Property  HandleGetOnPost : Boolean Read FHandleGetOnPost Write FHandleGetOnPost;
    Property  OnUnknownEncoding : TOnUnknownEncodingEvent Read FOnUnknownEncoding Write FOnUnknownEncoding;
  end;


  { TResponse }

  TResponse = class(THttpHeader)
  private
    FCacheControl: String;
    FContents: TStrings;
    FContentStream : TStream;
    FCode: Integer;
    FCodeText: String;
    FFreeContentStream: Boolean;
    FHeadersSent: Boolean;
    FContentSent: Boolean;
    FRequest : TRequest;
    FCookies : TCookies;
    FCustomHeaders : TStringList;
    function GetContent: String;
    procedure SetContent(const AValue: String);
    procedure SetContents(AValue: TStrings);
    procedure SetContentStream(const AValue: TStream);
    procedure SetFirstHeaderLine(const line: String);
    function  GetFirstHeaderLine: String;
    procedure ContentsChanged(Sender : TObject);
  Protected
    Procedure DoSendHeaders(Headers : TStrings); virtual; abstract;
    Procedure DoSendContent; virtual; abstract;
    Procedure CollectHeaders(Headers : TStrings); virtual;
  public
    constructor Create(ARequest : TRequest); overload;
    destructor destroy; override;
    Procedure SendContent;
    Procedure SendHeaders;
    Procedure SendResponse; // Delphi compatibility
    Function GetCustomHeader(const Name: String) : String;
    Procedure SetCustomHeader(const Name, Value: String);
    Procedure SendRedirect(const TargetURL:String);
    Property Request : TRequest Read FRequest;
    Property Code: Integer Read FCode Write FCode;
    Property CodeText: String Read FCodeText Write FCodeText;
    Property CacheControl : String Read FCacheControl Write FCacheControl;
    Property FirstHeaderLine : String Read GetFirstHeaderLine Write SetFirstHeaderLine;
    Property ContentStream : TStream Read FContentStream Write SetContentStream;
    Property Content : String Read GetContent Write SetContent;
    property Contents : TStrings read FContents Write SetContents;
    Property HeadersSent : Boolean Read FHeadersSent;
    Property ContentSent : Boolean Read FContentSent;
    property Cookies: TCookies read FCookies;
    Property CustomHeaders: TStringList read FCustomHeaders;
    Property FreeContentStream : Boolean Read FFreeContentStream Write FFreeContentStream;
  end;
  
  { TSessionVariable }


  { TCustomSession }

  TCustomSession = Class(TComponent)
  Private
    FSessionCookie: String;
    FSessionCookiePath: String;
    FTimeOut: Integer;
  Protected
    // Can be overridden to provide custom behaviour.
    procedure SetSessionCookie(const AValue: String); virtual;
    procedure SetSessionCookiePath(const AValue: String); virtual;
    // When called, generates a new GUID. Override to retrieve GUID from cookie/URL/...
    Function GetSessionID : String; virtual;
    // These must be overridden to actually store/retrieve variables.
    Function GetSessionVariable(VarName : String) : String; Virtual; abstract;
    procedure SetSessionVariable(VarName : String; const AValue: String);Virtual;abstract;
  Public
    Constructor Create(AOwner : TComponent); override;
    // Init session from request.
    Procedure InitSession(ARequest : TRequest; OnNewSession,OnExpired : TNotifyEvent); virtual;
    // Init response from session (typically, add cookie to response).
    Procedure InitResponse(AResponse : TResponse); virtual;
    // Update response from session (typically, change cookie to response and write session data).
    Procedure UpdateResponse(AResponse : TResponse); virtual; Abstract;
    // Remove variable from list of variables.
    Procedure RemoveVariable(VariableName : String); virtual; abstract;
    // Terminate session
    Procedure Terminate; virtual; abstract;
    // Session timeout in minutes
    Property TimeOutMinutes : Integer Read FTimeOut Write FTimeOut default 15;
    // ID of this session.
    Property SessionID : String Read GetSessionID;
    // Name of cookie used when tracing session. (may or may not be used)
    property SessionCookie : String Read FSessionCookie Write SetSessionCookie;
    // Path of cookie used when tracing session. (may or may not be used)
    Property SessionCookiePath : String Read FSessionCookiePath write SetSessionCookiePath;
    // Variables, tracked in session.
    Property Variables[VarName : String] : String Read GetSessionVariable Write SetSessionVariable;
  end;

  TRequestEvent = Procedure (Sender: TObject; ARequest : TRequest) of object;
  TResponseEvent = Procedure (Sender: TObject; AResponse : TResponse) of object;

  { EHTTP }

  EHTTP = Class(Exception)
  private
    FStatusCode: Integer;
    FStatusText: String;
    function GetStatusCode: Integer;virtual;
  Public
    // These are transformed to the HTTP status code and text. Helpcontext is taken as the default for statuscode.
    Property StatusCode : Integer Read GetStatusCode Write FStatusCode;
    Property StatusText : String Read FStatusText Write FStatusText;
  end;

  HTTPError = EHTTP;

Function HTTPDecode(const AStr: String): String;
Function HTTPEncode(const AStr: String): String;
Function IncludeHTTPPathDelimiter(const AStr: String): String;

Var
  // Default classes used when instantiating the collections.
  UploadedFilesClass : TUploadedFilesClass = TUploadedFiles;
  UploadedFileClass : TUploadedFileClass = TUploadedFile;
  MimeItemsClass : TMimeItemsClass = TMimeItems;
  MimeItemClass : TMimeItemClass = nil;

implementation

uses
{$ifdef CGIDEBUG}
  dbugintf,
{$endif}
  strutils;

Resourcestring
  SErrContentAlreadySent        = 'HTTP Response content was already sent';
  SErrHeadersAlreadySent        = 'HTTP headers were already sent';
  SErrInternalUploadedFileError = 'Internal uploaded file configuration error';
  SErrNoSuchUploadedFile        = 'No such uploaded file : "%s"';
  SErrUnknownCookie             = 'Unknown cookie: "%s"';
  SErrUnsupportedContentType    = 'Unsupported content type: "%s"';
  SErrNoRequestMethod           = 'No REQUEST_METHOD passed from server.';
  SErrInvalidRequestMethod      = 'Invalid REQUEST_METHOD passed from server: %s.';

const
   hexTable = '0123456789ABCDEF';

{ ---------------------------------------------------------------------
  Auxiliary functions
  ---------------------------------------------------------------------}
  
Function GetFieldNameIndex(AName : String) : Integer;

var
  Name: String;
begin
  Name := UpperCase(AName);
  Result:=NoHTTPFields;
  While (Result>0) and (UpperCase(HTTPFieldNames[Result])<>Name) do
    Dec(Result);
  If Result>0 then
    Result:=HTTPFieldIndexes[Result];
end;

function HTTPDecode(const AStr: String): String;

var
  S,SS, R : PChar;
  H : String[3];
  L,C : Integer;

begin
  L:=Length(Astr);
  SetLength(Result,L);
  If (L=0) then
    exit;
  S:=PChar(AStr);
  SS:=S;
  R:=PChar(Result);
  while (S-SS)<L do
    begin
    case S^ of
      '+': R^ := ' ';
      '%': begin
           Inc(S);
           if ((S-SS)<L) then
             begin
             if (S^='%') then
               R^:='%'
             else
               begin
               H:='$00';
               H[2]:=S^;
               Inc(S);
               If (S-SS)<L then
                 begin
                 H[3]:=S^;
                 Val(H,PByte(R)^,C);
                 If (C<>0) then
                   R^:=' ';
                 end;
               end;
             end;
           end;
      else
        R^ := S^;
      end;
    Inc(R);
    Inc(S);
    end;
  SetLength(Result,R-PChar(Result));
end;

function HTTPEncode(const AStr: String): String;

const
  HTTPAllowed = ['A'..'Z','a'..'z',
                 '*','@','.','_','-',
                 '0'..'9',
                 '$','!','''','(',')'];
                 
var
  SS,S,R: PChar;
  H : String[2];
  L : Integer;
  
begin
  L:=Length(AStr);
  SetLength(Result,L*3); // Worst case scenario
  if (L=0) then
    exit;
  R:=PChar(Result);
  S:=PChar(AStr);
  SS:=S; // Avoid #0 limit !!
  while ((S-SS)<L) do
    begin
    if S^ in HTTPAllowed then
      R^:=S^
    else if (S^=' ') then
      R^:='+'
    else
      begin
      R^:='%';
      H:=HexStr(Ord(S^),2);
      Inc(R);
      R^:=H[1];
      Inc(R);
      R^:=H[2];
      end;
    Inc(R);
    Inc(S);
    end;
  SetLength(Result,R-PChar(Result));
end;

function IncludeHTTPPathDelimiter(const AStr: String): String;

Var
  l : Integer;

begin
  Result:=AStr;
  L:=Length(Result);
  If (L>0) and (Result[L]<>'/') then
    Result:=Result+'/';
end;

{ -------------------------------------------------------------------
  THTTPMimeItem, default used by TRequest to process Multipart-encoded data.
  -------------------------------------------------------------------}

Type
  { THTTPMimeItem }

  THTTPMimeItem = Class(TMimeItem)
  private
    FData : Array[0..5] of string;
  protected
    Procedure SetHeader(AIndex: Integer; Const AValue: String); override;
    function GetDataSize: Int64; override;
    function GetHeader(AIndex: Integer): String; override;
    function GetIsFile: Boolean; override;
  public
    Procedure Process(Stream : TStream); override;
  end;

{ EHTTP }

function EHTTP.GetStatusCode: Integer;
begin
  Result:=FStatusCode;
  if Result=0 then
    Result:=HelpContext;
end;


procedure THTTPMimeItem.SetHeader(AIndex: Integer; const AValue: String);
begin
  FData[AIndex]:=Avalue;
end;

function THTTPMimeItem.GetDataSize: int64;
begin
  Result:=Length(Data);
end;

function THTTPMimeItem.GetHeader(AIndex: Integer): String;
begin
  Result:=FData[AIndex];
end;

function THTTPMimeItem.GetIsFile: Boolean;
begin
  Result:=inherited GetIsFile;
end;

procedure THTTPMimeItem.Process(Stream: TStream);

  Function GetLine(Var S : String) : String;

  Var
    P : Integer;

  begin
    P:=Pos(#13#10,S);
    If (P<>0) then
      begin
      Result:=Copy(S,1,P-1);
      Delete(S,1,P+1);
      end;
  end;

  Function GetWord(Var S : String) : String;

  Var
    I,len : Integer;
    Quoted : Boolean;
    C : Char;

  begin
    len:=length(S);
    quoted:=false;
    Result:='';
    for i:=1 to len do
      Begin
      c:=S[i];
      if (c='"') then
        Quoted:=Not Quoted
      else
        begin
        if not (c in [' ','=',';',':']) or Quoted then
          Result:=Result+C;
        if (c in [';',':','=']) and (not quoted) then
          begin
          Delete(S,1,I);
          Exit;
          end;
        end;
      end;
     S:='';
  end;

Var
  Line : String;
  len : integer;
  S : string;
  D : String;

begin
  {$ifdef CGIDEBUG}SendMethodEnter('THTTPMimeItem.Process');{$ENDIF}
  If Stream is TStringStream then
    D:=TStringStream(Stream).Datastring
  else
    begin
    SetLength(D,Stream.Size);
    Stream.ReadBuffer(D[1],Stream.Size);
    end;
  Line:=GetLine(D);
  While (Line<>'') do
    begin
    {$ifdef CGIDEBUG}SendDebug('Process data line: '+line);{$ENDIF}
    S:=GetWord(Line);
    While (S<>'') do
      begin
      ProcessHeader(lowercase(S),GetWord(Line));
      S:=GetWord(Line);
      end;
    Line:=GetLine(D);
    end;
  // Now Data contains the rest of the data, plus a CR/LF. Strip the CR/LF
  Len:=Length(D);
  If (len>2) then
    Data:=Copy(D,1,Len-2)
  else
    Data:='';
  {$ifdef CGIDEBUG}SendMethodExit('THTTPMimeItem.Process');{$ENDIF}
end;

{ ---------------------------------------------------------------------
  THTTPHeader
  ---------------------------------------------------------------------}

function THttpHeader.GetFieldCount: Integer;

Var
  I : Integer;

begin
  Result:=0;
  For I:=1 to NoHTTPFields do
    If (GetFieldValue(i)<>'') then
      Inc(Result);
end;

function THTTPHeader.GetContentLength: Integer;
begin
  Result:=StrToIntDef(GetFieldValue(9),0);
end;

procedure THTTPHeader.SetContentLength(Value: Integer);
begin
  SetFieldValue(9,IntToStr(Value));
end;


Function THttpHeader.GetFieldIndex(AIndex : Integer) : Integer;

var
  I : Integer;
   
begin
  I:=1;
  While (I<=NoHTTPFields) and (AIndex>=0) do
    begin
    If (GetFieldValue(i)<>'') then
      Dec(AIndex);
    Inc(I);
    end;
  If (AIndex=-1) then
    Result:=I-1
  else
    Result:=-1;  
end;

function THTTPHeader.GetServerPort: Word;
begin
  Result:=StrToIntDef(GetFieldValue(30),0);
end;

Procedure THTTPHeader.SetServerPort(AValue : Word);

begin
  SetFieldValue(30,IntToStr(AValue));
end;
    
function THTTPHeader.GetSetFieldValue(Index: Integer): String;

Var
  I : Integer;

begin
  I:=GetFieldIndex(Index);
  If (I<>-1) then
    Result:=GetFieldValue(I);
end;

function THTTPHeader.GetSetField(AIndex: Integer): String;
var
  I : Integer;

begin
  I:=GetFieldIndex(AIndex);
  If (I<>-1) then
    Result := HTTPFieldNames[I] + ': ' + GetFieldValue(I);
end;

function THTTPHeader.GetSetFieldName(AIndex: Integer): String;
var
  I : Integer;

begin
  I:=GetFieldIndex(AIndex);
  if (I<>-1) then
    Result:=HTTPFieldNames[I];
end;


Function THttpHeader.GetFieldValue(Index : Integer) : String;

begin
  if (Index>=1) and (Index<=NoHTTPFields) then
    Result:=FFields[Index]
  else
    case Index of
      0  : Result:=FHTTPVersion;
      36 : Result:=FHTTPXRequestedWith;
    else
      Result := '';
    end;
end;

procedure THTTPHeader.SetCookieFields(const AValue: TStrings);
begin
  FCookieFields.Assign(AValue);
end;


Procedure THttpHeader.SetFieldValue(Index : Integer; Value : String);

begin
  if (Index>=1) and (Index<=NoHTTPFields) then
    begin
    FFields[Index]:=Value;
    If (Index=11) then
      ParseCookies;
    end
  else
    case Index of
      0  : FHTTPVersion:=Value;
      25 : ; // Property PathInfo : String index 25 read GetFieldValue Write SetFieldValue;
      26 : ; // Property PathTranslated : String Index 26 read GetFieldValue Write SetFieldValue;
      27 : ; // Property RemoteAddress : String Index 27 read GetFieldValue Write SetFieldValue;
      28 : ; // Property RemoteHost : String Index 28 read  GetFieldValue Write SetFieldValue;
      29 : ; // Property ScriptName : String Index 29 read  GetFieldValue Write SetFieldValue;
      30 : ; // Property ServerPort : Word Read GetServerPort; // Index 30 in TRequest
      36 : FHTTPXRequestedWith:=Value;
    end;
end;

procedure THTTPHeader.ParseFirstHeaderLine(const line: String);
begin
  // Do nothing.
end;

procedure THTTPHeader.ParseCookies;

Var
  P : Integer;
  S,C : String;
  
begin
{$ifdef cgidebug}  SendMethodEnter('Parsecookies');{$endif}
  S:=Cookie;
  While (S<>'') do
    begin
    P:=Pos(';',S);
    If (P=0) then
      P:=length(S)+1;
    C:=Copy(S,1,P-1);
    While (P<Length(S)) and (S[P+1]=' ') do
      Inc(P);
    System.Delete(S,1,P);
    FCookieFields.Add(HTTPDecode(C));
    end;
{$ifdef cgidebug}  SendMethodExit('Parsecookies done');{$endif}
end;

constructor THttpHeader.Create;
begin
  FCookieFields:=TStringList.Create;
  FQueryFields:=TStringList.Create;
  FContentFields:=TStringList.Create;
  FHttpVersion := '1.1';
end;

destructor THttpHeader.Destroy;

begin
  FreeAndNil(FContentFields);
  FreeAndNil(FQueryFields);
  FreeAndNil(FCookieFields);
  inherited Destroy;
end;


function THttpHeader.GetFieldByName(const AName: String): String;
var
  i: Integer;

begin
  I:=GetFieldNameIndex(AName);
  If (I<>0) then
    Result:=self.GetFieldValue(i)
  else
    Result:='';
end;

Function THTTPHeader.LoadFromStream(Stream: TStream; IncludeCommand : Boolean) : Integer;

Var
  S : TStrings;

begin
  S:=TStringList.Create;
  Try
    S.LoadFromStream(Stream);
    Result:=LoadFromStrings(S,IncludeCommand);
  Finally
    S.Free;
  end;
end;

Function THTTPHeader.LoadFromStrings(Strings: TStrings; IncludeCommand : Boolean) : integer;

Var
  P  : Integer;
  S,VN : String;

begin
  Result:=0;
  if (Strings.Count>0) then
    begin
    if IncludeCommand then
      begin
      ParseFirstHeaderLine(Strings[0]);
      Inc(Result);
      end;
    While (Result<Strings.Count) and (Strings[Result]<>'') do
      begin
      S:=Strings[Result];
      P:=Pos(':',S);
      if (P<>0) then
        begin
        VN:=Copy(S,1,P-1);
        Delete(S,1,P);
        P:=GetFieldNameIndex(VN);
        If (P<>-1) then
          SetFieldValue(P,S);
        end;
      Inc(Result);
      end;
    end;
end;

procedure THttpHeader.SetFieldByName(const AName, AValue: String);
var
  i: Integer;

begin
  I:=GetFieldNameIndex(AName);
  If (I<>0) then
    SetFieldValue(i,AValue);
end;

{ ---------------------------------------------------------------------
  TMimeItems
  ---------------------------------------------------------------------}

function TMimeItems.GetP(AIndex : Integer): TMimeItem;
begin
  Result:=TMimeItem(Items[Aindex]);
end;

procedure TMimeItems.CreateUploadFiles(Files: TUploadedFiles; Vars : TStrings);

Var
  I,j : Integer;
  P : TMimeItem;
  LFN,Name,Value : String;
  U : TUploadedFile;

begin
  For I:=Count-1 downto 0 do
    begin
    P:=GetP(i);
    If (P.Name='') then
      P.Name:='DummyFileItem'+IntToStr(i);
      //Raise Exception.CreateFmt('Invalid multipart encoding: %s',[FI.Data]);
{$ifdef CGIDEBUG}
    With P Do
      begin
      SendSeparator;
      SendDebug  ('PMP item Name        : '+Name);
      SendDebug  ('PMP item Disposition : '+Disposition);
      SendDebug  ('PMP item FileName    : '+FileName);
      SendBoolean('PMP item IsFile      : ',IsFile);
      SendDebug  ('PMP item ContentType : '+ContentType);
      SendDebug  ('PMP item Description : '+Description);
      SendInteger('PMP item DLen        : ',Datasize);
      SendDebug  ('PMP item Data        : '+Data);
      end;
{$endif CGIDEBUG}
    Name:=P.Name;
    If Not P.IsFile Then
      Value:=P.Data
    else
      begin
      Value:=P.FileName;
      P.CreateUploadedFile(Files);
      end;
    Vars.Add(Name+'='+Value)
    end;
end;

function TMimeItem.GetIsFile: Boolean;
begin
  Result:=(FileName<>'');
end;

function TMimeItem.ProcessHeader(const AHeader, AValue: String): Boolean;

begin
  Result:=True;
  Case AHeader of
   'content-disposition' : Disposition:=Avalue;
   'name': Name:=Avalue;
   'filename' : FileName:=AValue;
   'content-description' :  description:=AValue;
   'content-type' : ContentType:=AValue;
  else
    Result:=False;
  end;
end;

Procedure TMimeItem.SaveToFile(Const AFileName: String);

Var
  D : String;
  F : TFileStream;

begin
  F:=TFileStream.Create(AFileName,fmCreate);
  Try
    D:=Data;
    F.Write(D[1],DataSize);
  finally
    F.Free;
  end;
end;

function TMimeItem.CreateUploadedFile(Files: TUploadedFiles): TUploadedFile;

Var
  J : Int64;
  D,LFN : String;

begin
  Result:=Nil;
  D:=Data;
  J:=DataSize;
  if (J=0){zero lenght file} or
     ((J=2)and (D=#13#10)){empty files come as a simple empty line} then
    LFN:='' //No tmp file will be created for empty files
  else
    begin
    LFN:=Files.GetTempUploadFileName(Name,FileName,J);
    SaveToFile(LFN);
    end;
  if (LFN<>'') then
   begin
   Result:=Files.Add as TUploadedFile;
   with Result do
     begin
     FieldName:=Self.Name;
     FileName:=Self.FileName;
     ContentType:=Self.ContentType;
     Disposition:=Self.Disposition;
     Size:=Self.Datasize;
     LocalFileName:=LFN;
     Description:=Self.Description;
     end;
   end;
end;


{
  This needs MASSIVE improvements for large files.
  Best would be to do this directly from the input stream
  and save the files at once if needed. (e.g. when a
  certain size is reached.)
}

procedure TMimeItems.FormSplit(var Cnt : String; boundary: String);

// Splits the form into items
var
  Sep : string;
  Clen,slen, p:longint;
  FI : TMimeItem;
  S : TStringStream;

begin
  {$ifdef CGIDEBUG}SendMethodEnter('TMimeItems.FormSplit');{$ENDIF}
  Sep:='--'+boundary+#13+#10;
  Slen:=length(Sep);
  CLen:=Pos('--'+Boundary+'--',Cnt);
  // Cut last marker
  Cnt:=Copy(Cnt,1,Clen-1);
  // Cut first marker
  system.Delete(Cnt,1,Slen);
  Clen:=Length(Cnt);
  While Clen>0 do
    begin
    P:=pos(Sep,Cnt);
    If (P=0) then
      P:=CLen+1;
    S:=TStringStream.Create(Copy(Cnt,1,P-1));
    try
      FI:=Add as TMimeItem;
      FI.Process(S)
    finally
      S.Free;
    end;
    system.delete(Cnt,1,P+SLen-1);
    CLen:=Length(Cnt);
    end;
  {$ifdef CGIDEBUG}SendMethodExit('TMimeItems.FormSplit');{$ENDIF}
end;

Function TMimeItems.First: TMimeItem;
begin
  If Count = 0 then
    Result := Nil
  else
    Result := Parts[0];
end;

Function TMimeItems.Last: TMimeItem;
begin
  If Count = 0 then
    Result := nil
  else
    Result := Parts[Count - 1];
end;

{ -------------------------------------------------------------------
  TRequest
  -------------------------------------------------------------------}
  
constructor TRequest.Create;
begin
  inherited create;
  FHandleGetOnPost:=True;
  FFiles:=CreateUploadedFiles;
  FFiles.FRequest:=Self;
  FLocalPathPrefix:='-';
end;

function TRequest.CreateUploadedFiles: TUploadedFiles;

Var
  CC : TUploadedFilesClass;
  CI : TUploadedFileClass;

begin
  CC:=UploadedFilesClass;
  CI:=UploadedFileClass;
  if (CC=Nil) then
    CC:=TUploadedFiles;
  if (CI=Nil) then
    CI:=TUploadedFile;
  Result:=CC.Create(CI);
end;

function TRequest.CreateMimeItems: TMimeItems;

Var
  CC : TMimeItemsClass;
  CI : TMimeItemClass;

begin
  CC:=MimeItemsClass;
  CI:=MimeItemClass;
  if (CC=Nil) then
    CC:=TMimeItems;
  if (CI=Nil) then
    CI:=TMimeItem;
  Result:=CC.Create(CI);
end;

destructor TRequest.destroy;
begin
  FreeAndNil(FFiles);
  inherited destroy;
end;

function TRequest.GetNextPathInfo: String;

Var
  P : String;
  i : Integer;
  
begin
  P:=PathInfo;
{$ifdef CGIDEBUG}SendDebug(Format('Pathinfo: "%s" "%s"',[P,FReturnedPathInfo]));{$ENDIF}
  if (P <> '') and (P[length(P)] = '/') then
    Delete(P, length(P), 1);//last char is '/'
  If (P<>'') and (P[1]='/') then
    Delete(P,1,1);
  Delete(P,1,Length(IncludeHTTPPathDelimiter(FReturnedPathInfo)));
 {$ifdef CGIDEBUG}SendDebug(Format('Pathinfo: "%s" "%s"',[P,FReturnedPathInfo]));{$ENDIF}
  I:=Pos('/',P);
  If (I=0) then
    I:=Length(P)+1;
  Result:=Copy(P,1,I-1);
  FReturnedPathInfo:=IncludeHTTPPathDelimiter(FReturnedPathInfo)+Result;
 {$ifdef CGIDEBUG}SendDebug(Format('Pathinfo: "%s" "%s" : %s',[P,FReturnedPathInfo,Result]));{$ENDIF}
end;

procedure TRequest.ParseFirstHeaderLine(const line: String);
var
  i: Integer;
begin
  FCommandLine := line;
  i := Pos(' ', line);
  FCommand := UpperCase(Copy(line, 1, i - 1));
  FURI := Copy(line, i + 1, Length(line));

  // Extract HTTP version
  i := Pos(' ', URI);
  if i > 0 then
  begin
    FHttpVersion := Copy(URI, i + 1, Length(URI));
    FURI := Copy(URI, 1, i - 1);
    FHttpVersion := Copy(HttpVersion, Pos('/', HttpVersion) + 1, Length(HttpVersion));
  end;

  // Extract query string
  i := Pos('?', URI);
  if i > 0 then
  begin
    Query:= Copy(URI, i + 1, Length(URI));
    FURI := Copy(URI, 1, i - 1);
  end;
end;

function TRequest.GetLocalPathPrefix: string;
var
  pi: String;
  i: Cardinal;
begin
  if FLocalPathPrefix='-' then
    begin
    pi := PathInfo;
    FLocalPathPrefix := '';
    i := 0;
    repeat
    i := PosEx('/',PI,i+1);
    if i > 0 then
      FLocalPathPrefix := FLocalPathPrefix + '../';
    until i=0;
    end;
  result := FLocalPathPrefix;
end;

function TRequest.GetFieldValue(AIndex: Integer): String;
begin
  Case AIndex of
    25 : Result:=FPathInfo;
    31 : Result:=FCommand;
    32 : Result:=FURI;
    34 : Result:=FHost;
    36 : Result:=FRequestedWith;
    35 : begin
         If Not FContentRead and AllowReadContent then
           begin
           ReadContent;
           FContentRead:=True; // in case InitContent was not called.
           end;
         Result:=FContent;
         end
  else
    Result:=inherited GetFieldValue(AIndex);
  end;
end;

procedure TRequest.SetFieldValue(Index: Integer; Value: String);
begin
  Case Index of
    25 : FPathInfo:=Value;
    30 : FServerPort:=Value;
    31 : FCommand:=Value;
    32 : FURI:=Value;
    34 : FHost:=Value;
    36 : FRequestedWith:=Value;
  else
    inherited SetFieldValue(Index, Value);
  end
end;

function TRequest.GetFirstHeaderLine: String;
begin
  Result := Command + ' ' + URI;
  if Length(HttpVersion) > 0 then
    Result := Result + ' HTTP/' + HttpVersion;
end;

function TRequest.AllowReadContent: Boolean;
begin
  Result:=True;
end;

procedure TRequest.HandleUnknownEncoding(const AContentType: String;
  Stream: TStream);
begin
  If Assigned(FOnUnknownEncoding) then
    FOnUnknownEncoding(Self,AContentType,Stream);
end;

procedure TRequest.ReadContent;
begin
  // Implement in descendents
end;

procedure TRequest.ProcessQueryString(const FQueryString: String; SL: TStrings);


var
  queryItem : String;
  delimiter : Char;
  aString : String;
  aSepStr : String;
  aPos    : Integer;
  aLenStr : Integer;
  aLenSep : Integer;

  function hexConverter(h1, h2 : Char) : Char;

  var
    B : Byte;

  begin
    B:=(Pos(upcase(h1),hexTable)-1)*16;
    B:=B+Pos(upcase(h2),hexTable)-1;
    Result:=chr(B);
  end;


  procedure InitToken(aStr, aSep : String);

  begin
    aString := aStr;
    aSepStr := aSep;
    aPos    := 1;
    aLenStr := Length(aString);
    aLenSep := Length(aSepStr);
  end;

  function NextToken(out aToken : String; out aSepChar : Char) : Boolean;

  var
    i : Integer;
    j : Integer;
    BoT : Integer;
    EoT : Integer;
    isSep : Boolean;

  begin
    BoT:=aPos;
    EoT:=aPos;
    for i:=aPos to aLenStr do
      begin
      IsSep := false;
      for j := 1 to aLenSep do
        begin
        if aString[i] = aSepStr[j] then
          begin
          IsSep := true;
          Break;
          end;
        end;
      if IsSep then
        begin
        EoT  := i;
        aPos := i + 1;
        aSepChar := aString[i];
        Break;
        end
      else
        begin
        if i = aLenStr then
          begin
          EoT  := i;
          aPos := i;
          Break;
          end;
        end;
      end;
    if aPos < aLenStr then
      begin
      aToken := Copy(aString, BoT, EoT - BoT);
      Result := true;
      end
    else
      begin
      if aPos = aLenStr then
        begin
        aToken := Copy(aString, BoT, EoT - BoT + 1);
        Result := true;
        aPos   := aPos + 1;
        end
      else
        begin
        Result := false;
       end;
    end;
  end;


begin
{$ifdef CGIDEBUG}SendMethodEnter('ProcessQueryString');{$endif CGIDEBUG}
  InitToken(FQueryString, '&');
  while NextToken(QueryItem, delimiter) do
    begin
    if (QueryItem<>'') then
      begin
      QueryItem:=HTTPDecode(QueryItem);
      SL.Add(QueryItem);
      end;
    end;
{$ifdef CGIDEBUG}SendMethodExit('ProcessQueryString');{$endif CGIDEBUG}
end;

function TRequest.RequestUploadDir: String;

begin
  Result:='';
end;

function TRequest.GetTempUploadFileName(const AName, AFileName: String;
  ASize: Int64): String;

Var
  D : String;

begin
  D:=RequestUploadDir;
  if (D='') then
    D:=GetTempDir; // Note that this may require a TEMP environment variable to be set by the webserver.
  Result:=GetTempFileName(D, 'CGI');
end;

procedure TRequest.DeleteTempUploadedFiles;
begin
  FFiles.DeleteTempUploadedFiles;
end;

procedure TRequest.InitRequestVars;

var
  R : String;

begin
{$ifdef CGIDEBUG}
  SendMethodEnter('TRequest.InitRequestVars');
{$endif}
  R:=Method;
  if (R='') then
    Raise EHTTP.CreateHelp(SErrNoRequestMethod,400);
  // Always process QUERYSTRING.
  InitGetVars;
  // POST and PUT, force post var treatment.
  // To catch other methods we do not treat specially, we'll do the same if contentlength>0
  if (CompareText(R,'POST')=0) or (CompareText(R,'PUT')=0) or (ContentLength>0) then
    InitPostVars;
{$ifdef CGIDEBUG}
  SendMethodExit('TRequest.InitRequestVars');
{$endif}
end;

Type
  TCapacityStream = Class(TMemoryStream)
  Public
    Property Capacity;
  end;

procedure TRequest.InitPostVars;

Var
  M  : TCapacityStream;
  Cl : Integer;
  CT : String;

begin
{$ifdef CGIDEBUG}
  SendMethodEnter('InitPostVars');
{$endif}
  CL:=ContentLength;
  if CL<>0 then
    begin
    M:=TCapacityStream.Create;
    Try
      if CL<>0 then
        begin
        M.Capacity:=Cl;
        M.WriteBuffer(Content[1], Cl);
        end;
      M.Position:=0;
      CT:=ContentType;
      if Pos('MULTIPART/FORM-DATA',Uppercase(CT))<>0 then
        ProcessMultiPart(M,CT, ContentFields)
      else if Pos('APPLICATION/X-WWW-FORM-URLENCODED',Uppercase(CT))<>0 then
        ProcessUrlEncoded(M, ContentFields)
      else
        HandleUnknownEncoding(CT,M)
    finally
     M.Free;
    end;
    end;
{$ifdef CGIDEBUG}
  SendMethodExit('InitPostVars');
{$endif}
end;

procedure TRequest.InitGetVars;
Var
  FQueryString : String;

begin
{$ifdef CGIDEBUG}
  SendMethodEnter('InitGetVars');
{$endif}
  FQueryString:=QueryString;
  If (FQueryString<>'') then
    ProcessQueryString(FQueryString, QueryFields);
{$ifdef CGIDEBUG}
  SendMethodExit('InitGetVars');
{$endif}
end;

procedure TRequest.InitContent(var AContent: String);
begin
  FContent:=AContent;
  FContentRead:=True;
end;


procedure TRequest.ProcessMultiPart(Stream: TStream; const Boundary: String;
  SL: TStrings);

Var
  L : TMimeItems;
  B : String;
  I,J : Integer;
  S,FF,key, Value : String;
  FI : TMimeItem;
  F : TStream;

begin
{$ifdef CGIDEBUG} SendMethodEnter('ProcessMultiPart');{$endif CGIDEBUG}
  i:=Pos('=',Boundary);
  B:=Copy(Boundary,I+1,Length(Boundary)-I);
  I:=Length(B);
  If (I>0) and (B[1]='"') then
    B:=Copy(B,2,I-2);
  L:=CreateMimeItems;
  Try
    if Stream is TStringStream then
      S:=TStringStream(Stream).DataString
    else
      begin
      SetLength(S,Stream.Size);
      If Length(S)>0 then
        if Stream is TCustomMemoryStream then
          // Faster.
          Move(TCustomMemoryStream(Stream).Memory^,S[1],Length(S))
        else
          begin
          Stream.Read(S[1],Length(S));
          Stream.Position:=0;
          end;
      end;
    L.FormSplit(S,B);
    L.CreateUploadFiles(Files,SL);
  Finally
    L.Free;
  end;
{$ifdef CGIDEBUG}  SendMethodExit('ProcessMultiPart');{$endif CGIDEBUG}
end;

procedure TRequest.ProcessURLEncoded(Stream: TStream; SL: TStrings);

var
  S : String;

begin
{$ifdef CGIDEBUG} SendMethodEnter('ProcessURLEncoded');{$endif CGIDEBUG}
  SetLength(S,Stream.Size); // Skip added Null.
  Stream.ReadBuffer(S[1],Stream.Size);
{$ifdef CGIDEBUG}SendDebugFmt('Query string : %s',[s]);{$endif CGIDEBUG}
  ProcessQueryString(S,SL);
{$ifdef CGIDEBUG} SendMethodEnter('ProcessURLEncoded');{$endif CGIDEBUG}
end;

{ ---------------------------------------------------------------------
  TUploadedFiles
  ---------------------------------------------------------------------}

function TUploadedFiles.GetFile(Index : Integer): TUploadedFile;
begin
  Result:=TUPloadedFile(Items[Index]);
end;

procedure TUploadedFiles.SetFile(Index : Integer; const AValue: TUploadedFile);
begin
  Items[Index]:=AValue;
end;

function TUploadedFiles.GetTempUploadFileName(const AName, AFileName: String;
  ASize: Int64): String;
begin
  If Assigned(FRequest) then
    Result:=FRequest.GetTempUploadFileName(AName,AFileName,ASize)
  else
    Result:=GetTempFileName;
end;

function TUploadedFiles.IndexOfFile(AName: String): Integer;

begin
  Result:=Count-1;
  While (Result>=0) and (CompareText(Files[Result].FieldName,AName)<>0) do
    Dec(Result);
end;

function TUploadedFiles.FileByName(AName: String): TUploadedFile;


begin
  Result:=FindFile(AName);
  If (Result=Nil) then
    Raise HTTPError.CreateFmt(SErrNoSuchUploadedFile,[AName]);
end;

Function TUploadedFiles.FindFile(AName: String): TUploadedFile;

Var
  I : Integer;
  
begin
  I:=IndexOfFile(AName);
  If (I=-1) then
    Result:=Nil
  else
    Result:=Files[I];
end;

Procedure TUPloadedFiles.DeleteTempUploadedFiles;

var
  i: Integer;

begin
  //delete all temporary uploaded files created for this request if there are any
  for i := Count-1 downto 0 do
    Files[i].DeleteTempUploadedFile;
end;

Function TUploadedFiles.First: TUploadedFile;
begin
  If Count = 0 then
    Result := Nil
  else
    Result := Files[0];
end;

Function TUploadedFiles.Last: TUploadedFile;
begin
  If Count = 0 then
    Result := nil
  else
    Result := Files[Count - 1];
end;


{ ---------------------------------------------------------------------
  TUploadedFile
  ---------------------------------------------------------------------}

procedure TUploadedFile.DeleteTempUploadedFile;

Var
  s: String;

begin
  if (FStream is TFileStream) then
    FreeStream;
  if (LocalFileName<>'') and FileExists(LocalFileName) then
    DeleteFile(LocalFileName);
end;


function TUploadedFile.GetStream: TStream;

begin
  If (FStream=Nil) then
    begin
    If (FLocalFileName='') then
      Raise HTTPError.Create(SErrInternalUploadedFileError);
    FStream:=TFileStream.Create(FLocalFileName,fmOpenRead or fmShareDenyWrite);
    end;
  Result:=FStream;
end;

Procedure TUploadedFile.FreeStream;

begin
  FreeAndNil(FStream);
end;

destructor TUploadedFile.Destroy;
begin
  FreeStream;
  Inherited;
end;

{ ---------------------------------------------------------------------
  TResponse
  ---------------------------------------------------------------------}

constructor TResponse.Create(ARequest : TRequest);
begin
  inherited Create;
  FRequest:=ARequest;
  FCode := 200;
  FCodeText := 'OK';
  ContentType:='text/html';
  FContents:=TStringList.Create;
  TStringList(FContents).OnChange:=@ContentsChanged;
  FCookies:=TCookies.Create(TCookie);
  FCustomHeaders:=TStringList.Create;
end;

destructor TResponse.destroy;
begin
  if FreeContentStream then
    FreeAndNil(FContentStream);
  FreeAndNil(FCookies);
  FreeAndNil(FContents);
  FreeAndNil(FCustomHeaders);
  inherited destroy;
end;


procedure TResponse.SendContent;
begin
  if ContentSent then
    Raise HTTPError.Create(SErrContentAlreadySent);
  if Not HeadersSent then
    SendHeaders;
  DoSendContent;
  FContentSent:=True;
end;

procedure TResponse.SendHeaders;

Var
  FHeaders  : TStringList;

begin
  if HeadersSent then
    Raise HTTPError.Create(SErrHeadersAlreadySent);
  FHeaders:=TStringList.Create;
  CollectHeaders(FHeaders);
  With Fheaders do
  If (Count>0) and (Strings[Count-1]<>'') then
    Add('');
  Try
    DoSendHeaders(FHeaders);
    FHeadersSent:=True;
  Finally
    FHeaders.Free;
  end;
end;

procedure TResponse.SendResponse;
begin
  SendContent;
end;

function TResponse.GetCustomHeader(const Name: String): String;
begin
  Result := FCustomHeaders.Values[Name];
end;

procedure TResponse.SetCustomHeader(const Name, Value: String);
begin
  if GetCustomHeader(Name) = '' then
    FCustomHeaders.Add(Name + '=' + Value)
  else
    FCustomHeaders.Values[Name] := Value;
end;

procedure TResponse.SendRedirect(const TargetURL: String);
begin
  Location := TargetURL;
  if FHttpVersion = '1.1' then
    begin
    Code := 307;// HTTP/1.1 307 HTTP_TEMPORARY_REDIRECT -> 'Temporary Redirect'
    CodeText := 'Temporary Redirect';
    end
  else
    begin
    Code := 302;// HTTP/1.0 302 HTTP_MOVED_TEMPORARILY -> 'Found'
    CodeText := 'Moved Temporarily';
    end;
end;

procedure TResponse.SetFirstHeaderLine(const line: String);
var
  i: Integer;
  s: String;
begin
  i := Pos('/', line);
  s := Copy(line, i + 1, Length(line));
  i := Pos(' ', s);
  FHttpVersion := Copy(s, 1, i - 1);
  s := Copy(s, i + 1, Length(s));
  i := Pos(' ', s);
  if i > 0 then begin
    FCodeText := Copy(s, i + 1, Length(s));
    s := Copy(s, 1, i - 1);
  end;
  FCode := StrToInt(s);
end;

procedure TResponse.SetContents(AValue: TStrings);
begin
  FContentStream:=Nil;
  FContents.Assign(AValue);
end;

function TResponse.GetContent: String;
begin
  Result:=Contents.Text;
end;

procedure TResponse.SetContent(const AValue: String);
begin
  FContentStream:=Nil;
  FContents.Text:=AValue;
end;

procedure TResponse.SetContentStream(const AValue: TStream);
begin
  If (FContentStream<>AValue) then
    begin
    if (FContentStream<>Nil) and FreeContentStream then
      FreeAndNil(FContentStream);
    FContentStream:=AValue;
    If (FContentStream<>Nil) then
      ContentLength:=FContentStream.Size
    else
      ContentLength:=0;
    end;
end;

function TResponse.GetFirstHeaderLine: String;
begin
  Result := Format('HTTP/%s %d %s', [HttpVersion, Code, CodeText]);
end;

procedure TResponse.ContentsChanged(Sender: TObject);

Var
  I,L,LE : Integer;

begin
  L:=0;
  LE:=Length(LineEnding);
  For I:=0 to FContents.Count-1 do
    L:=L+Length(FContents[i])+LE;
  ContentLength:=L;
end;

procedure TResponse.CollectHeaders(Headers: TStrings);

Var
  I : Integer;

begin
  Headers.add(Format('Status: %d %s',[Code,CodeText]));
{$ifdef cgidebug}
  SendMethodEnter('Collectheaders');
  If Not Assigned(FCookies) then
    SendDebug('No cookies')
  else
    SendInteger('Nr of cookies',FCookies.Count);
{$endif}
  For I:=0 to FCookies.Count-1 do
    Headers.Add('Set-Cookie: '+FCookies[i].AsString);
  For I:=0 to FieldCount-1 do
    Headers.Add(Fields[i]);
  For I:=0 to FCustomHeaders.Count - 1 do if FCustomHeaders[I] <> '' then
    Headers.Add(FCustomHeaders.Names[I] + ': ' + FCustomHeaders.ValueFromIndex[I]);
  Headers.Add('');
{$ifdef cgidebug} SendMethodExit('Collectheaders');{$endif}
end;


{ ---------------------------------------------------------------------
  TCookie
  ---------------------------------------------------------------------}

function TCookie.GetAsString: string;

  Procedure AddToResult(S : String);
  
  begin
    Result:=Result+';'+S;
  end;

Var
  Y,M,D : Word;

begin
{$ifdef cgidebug}SendMethodEnter('TCookie.GetAsString');{$endif}
  try
    Result:=Format('%s=%s',[HTTPEncode(FName),HTTPEncode(FValue)]);
    if (Length(FDomain)>0) then
      AddToResult(Format(SCookieDomain,[FDomain]));
    if (Length(FPath)>0) then
      AddToResult(Format(SCookiePath,[FPath]));
    if (FExpires>-1) then
      begin
      DecodeDate(Expires,Y,M,D);
      AddToResult(Format(FormatDateTime(SCookieExpire,Expires),
                         [HTTPDays[DayOfWeek(Expires)],HTTPMonths[M]]));
      end;
    if FHttpOnly then
      AddToResult(SCookieHttpOnly);
    if FSecure then
      AddToResult(SCookieSecure);
  except
{$ifdef cgidebug}
    On E : Exception do
      SendDebug('Exception in cookie AsString: '+E.Message)
{$endif}
  end;
{$ifdef cgidebug}SendMethodExit('TCookie.GetAsString');{$endif}
end;

constructor TCookie.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FExpires:=-1;
end;

procedure TCookie.Assign(Source: TPersistent);
begin
  if Source is TCookie then
    with TCookie(Source) do
      begin
      Self.FName:=Name;
      Self.FValue:=Value;
      Self.FDomain:=Domain;
      Self.FPath:=Path;
      Self.FExpires:=Expires;
      Self.FHttpOnly:=HttpOnly;
      Self.FSecure:=Secure;
      end
  else
    inherited Assign(Source);
end;

procedure TCookie.Expire;
begin
  FExpires := EncodeDate(1970, 1, 1);
end;

{ ---------------------------------------------------------------------
  TCookies
  ---------------------------------------------------------------------}

function TCookies.GetCookie(Index: Integer): TCookie;
begin
{$ifdef cgidebug}SendMethodExit('TCookies.GetCookie');{$endif}
  Result:=TCookie(inherited Items[Index]);
{$ifdef cgidebug}SendMethodExit('TCookies.GetCookie');{$endif}
end;

procedure TCookies.SetCookie(Index: Integer; Value: TCookie);
begin
  Items[Index]:=Value
end;

function TCookies.Add: TCookie;
begin
  Result:=TCookie(Inherited Add);
end;

function TCookies.CookieByName(AName: String): TCookie;
begin
  Result:=FindCookie(AName);
  If (Result=Nil) then
    Raise HTTPError.CreateFmt(SErrUnknownCookie,[AName]);
end;

function TCookies.FindCookie(AName: String): TCookie;
Var
  I : Integer;

begin
  I:=IndexOfCookie(AName);
  If (I=-1) then
    Result:=Nil
  else
    Result:=GetCookie(I);
end;

function TCookies.IndexOfCookie(AName: String): Integer;

begin
  Result:=Count-1;
  While (Result>=0) and (CompareText(GetCookie(Result).Name,AName)<>0) do
    Dec(Result);
end;

{ ---------------------------------------------------------------------
  TCustomSession
  ---------------------------------------------------------------------}

procedure TCustomSession.SetSessionCookie(const AValue: String);
begin
  FSessionCookie:=AValue;
end;

procedure TCustomSession.SetSessionCookiePath(const AValue: String);
begin
  FSessionCookiePath:=AValue;
end;

function TCustomSession.GetSessionID: String;

Var
  G : TGUID;

begin
  CreateGUID(G);
  Result:=GuiDToString(G);
  Result:=Copy(Result,2,36);
end;

constructor TCustomSession.Create(AOwner: TComponent);
begin
  FTimeOut:=DefaultTimeOut;
  inherited Create(AOwner);
end;

procedure TCustomSession.InitResponse(AResponse: TResponse);
begin
  // do nothing
end;

procedure TCustomSession.InitSession(ARequest: TRequest; OnNewSession,
  OnExpired: TNotifyEvent);
begin
  // Do nothing
end;

initialization
  MimeItemClass:=THTTPMimeItem;
end.
