{
    This file is part of the Free Pascal fcl-web package
    Copyright (c) 1999-2022 by the Free Pascal development team

    http protocol

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit httpprotocol;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

Type
  // HTTP 1.1 defined headers.
  THeader = (hhUnknown,
     hhAccept,hhAcceptCharset,hhAcceptEncoding, hhAcceptLanguage,
     hhAcceptRanges, hhAge, hhAllow, hhAuthorization, hhCacheControl,
     hhConnection, hhContentEncoding, hhContentLanguage,
     hhContentLength,hhContentLocation, hhContentMD5, hhContentRange,
     hhContentType, hhDate, hhETag, hhExpires, hhExpect,
     hhFrom, hhHost, hhIfMatch, hhIfModifiedSince, hhIfNoneMatch,
     hhIfRange, hhIfUnModifiedSince, hhLastModified, hhLocation, hhMaxForwards,
     hhPragma, hhProxyAuthenticate, hhProxyAuthorization, hhRange, hhReferer,
     hhRetryAfter, hhServer, hhTE, hhTrailer,
     hhTransferEncoding, hhUpgrade , hhUserAgent, hhVary,
     hhVia, hhWarning, hhWWWAuthenticate);
  THeaders = Set of THeader;
  THeaderDirection = (hdRequest,hdResponse);
  THeaderDirections = Set of THeaderDirection;

  THeadersArray = Array[THeader] of string;

Const
  HeaderAccept          = 'Accept';
  HeaderAcceptCharset   = 'Accept-Charset';
  HeaderAcceptEncoding  = 'Accept-Encoding';
  HeaderAcceptLanguage  = 'Accept-Language';
  HeaderAcceptRanges    = 'Accept-Ranges';
  HeaderAge             = 'Age';
  HeaderAllow           = 'Allow';
  HeaderAuthorization   = 'Authorization';
  HeaderCacheControl    = 'Cache-Control';
  HeaderConnection      = 'Connection';
  HeaderContentEncoding = 'Content-Encoding';
  HeaderContentLanguage = 'Content-Language';
  HeaderContentLength   = 'Content-Length';
  HeaderContentLocation = 'Content-Location';
  HeaderContentMD5      = 'Content-MD5';
  HeaderContentRange    = 'Content-Range';
  HeaderContentType     = 'Content-Type';
  HeaderDate            = 'Date';
  HeaderETag            = 'ETag';
  HeaderExpires         = 'Expires';
  HeaderExpect          = 'Expect';
  HeaderFrom            = 'From';
  HeaderHost            = 'Host';
  HeaderIfMatch         = 'If-Match';
  HeaderIfModifiedSince = 'If-Modified-Since';
  HeaderIfNoneMatch     = 'If-None-Match';
  HeaderIfRange         = 'If-Range';
  HeaderIfUnModifiedSince = 'If-Unmodified-Since';
  HeaderLastModified    = 'Last-Modified';
  HeaderLocation        = 'Location';
  HeaderMaxForwards     = 'Max-Forwards';
  HeaderPragma          = 'Pragma';
  HeaderProxyAuthenticate = 'Proxy-Authenticate';
  HeaderProxyAuthorization = 'Proxy-Authorization';
  HeaderRange           = 'Range';
  HeaderReferer         = 'Referer';
  HeaderRetryAfter      = 'Retry-After';
  HeaderServer          = 'Server';
  HeaderTE              = 'TE';
  HeaderTrailer         = 'Trailer';
  HeaderTransferEncoding = 'Transfer-Encoding';
  HeaderUpgrade         = 'Upgrade';
  HeaderUserAgent       = 'User-Agent';
  HeaderVary            = 'Vary';
  HeaderVia             = 'Via';
  HeaderWarning         = 'Warning';
  HeaderWWWAuthenticate = 'WWW-Authenticate';

  // These Headers are NOT in the HTTP 1.1 definition.
  HeaderXRequestedWith  = 'X-Requested-With';
  HeaderCookie          = 'Cookie';
  HeaderSetCookie       = 'Set-Cookie';

  HTTPDateFmt     = '"%s", dd "%s" yyyy hh:mm:ss'; // For use in FormatDateTime
  SCookieExpire   = ' "Expires="'+HTTPDateFmt+' "GMT"';
  SCookieDomain   = ' Domain=%s';
  SCookiePath     = ' Path=%s';
  SCookieSecure   = ' Secure';
  SCookieHttpOnly = ' HttpOnly';
  SCookieSameSite = ' SameSite';

  HTTPMonths: array[1..12] of string[3] = (
    'Jan', 'Feb', 'Mar', 'Apr',
    'May', 'Jun', 'Jul', 'Aug',
    'Sep', 'Oct', 'Nov', 'Dec');
  HTTPDays: array[1..7] of string[3] = (
    'Sun', 'Mon', 'Tue', 'Wed',
    'Thu', 'Fri', 'Sat');


Const
  HTTPHeaderDirections : Array[THeader] of THeaderDirections = (
   [],
   [hdRequest],[hdRequest],[hdRequest], [hdRequest],
   [hdResponse], [hdResponse], [hdResponse], [hdRequest], [hdRequest,hdResponse],
   [hdRequest,hdResponse], [hdRequest,hdResponse], [hdRequest,hdResponse],
   [hdRequest,hdResponse],[hdRequest,hdResponse], [hdRequest,hdResponse], [hdRequest,hdResponse],
   [hdRequest,hdResponse], [hdRequest,hdResponse], [hdResponse], [hdRequest,hdResponse], [hdRequest],
   [hdRequest], [hdRequest], [hdRequest], [hdRequest], [hdRequest],
   [hdRequest], [hdRequest], [hdRequest,hdResponse], [hdResponse], [hdRequest],
   [hdRequest, hdResponse] , [hdResponse], [hdRequest], [hdRequest,hdResponse], [hdRequest],
   [hdResponse], [hdResponse], [hdRequest], [hdRequest,hdResponse],
   [hdRequest,hdResponse], [hdRequest,hdResponse], [hdRequest], [hdRequest,hdResponse],
   [hdRequest,hdResponse], [hdRequest,hdResponse], [hdResponse]);

  HTTPHeaderNames : THeadersArray
                 = ('',
                    HeaderAccept,HeaderAcceptCharset,HeaderAcceptEncoding, HeaderAcceptLanguage,
                    HeaderAcceptRanges, HeaderAge, HeaderAllow, HeaderAuthorization, HeaderCacheControl,
                    HeaderConnection, HeaderContentEncoding, HeaderContentLanguage,
                    HeaderContentLength,HeaderContentLocation, HeaderContentMD5, HeaderContentRange,
                    HeaderContentType, HeaderDate, HeaderETag, HeaderExpires, HeaderExpect,
                    HeaderFrom, HeaderHost, HeaderIfMatch, HeaderIfModifiedSince, HeaderIfNoneMatch,
                    HeaderIfRange, HeaderIfModifiedSince, HeaderLastModified, HeaderLocation, HeaderMaxForwards ,
                    HeaderPragma, HeaderProxyAuthenticate, HeaderProxyAuthorization, HeaderRange, HeaderReferer,
                    HeaderRetryAfter, HeaderServer, HeaderTE, HeaderTrailer,
                    HeaderTransferEncoding, HeaderUpgrade , HeaderUserAgent, HeaderVary,
                    HeaderVia, HeaderWarning, HeaderWWWAuthenticate);

Function HeaderName(AHeader : THeader) : String;
Function HeaderType(AHeader : String) : THeader;
Function HTTPDecode(const AStr: String): String;
Function HTTPEncode(const AStr: String): String;
Function IncludeHTTPPathDelimiter(const AStr: String): String;
Function ExcludeHTTPPathDelimiter(const AStr: String): String;
Function GetHTTPStatusText (ACode: Cardinal; aUppercase : Boolean = False) : String;

implementation

function HeaderName(AHeader: THeader): String;

begin
  Result:=HTTPHeaderNames[AHeader];
end;

function HeaderType(AHeader: String): THeader;

begin
  Result:=High(THeader);
  While (Result>hhUnknown) and (CompareText(HTTPHeaderNames[Result],AHeader)<>0) do
    Result:=Pred(Result);
end;

function HTTPDecode(const AStr: String): String;

var
  S,SS, R : PChar;
  H : String[3];
  L,C : Integer;

begin
  L:=Length(Astr);
  Result:='';
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
  Result:='';
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

function ExcludeHTTPPathDelimiter(const AStr: String): String;

Var
  l : Integer;

begin
  L:=Length(AStr);
  If (L>0) and (AStr[L]='/') then
    Result:=Copy(AStr,1,L-1)
  else
    Result:=AStr;
end;

Function GetHTTPStatusText (ACode: Cardinal; aUppercase : Boolean = False) : String;

begin
  Case ACode of
    100 :  Result:='Continue';
    101 :  Result:='Switching Protocols';
    200 :  Result:='OK';
    201 :  Result:='Created';
    202 :  Result:='Accepted';
    203 :  Result:='Non-Authoritative Information';
    204 :  Result:='No Content';
    205 :  Result:='Reset Content';
    206 :  Result:='Partial Content';
    300 :  Result:='Multiple Choices';
    301 :  Result:='Moved Permanently';
    302 :  Result:='Found';
    303 :  Result:='See Other';
    304 :  Result:='Not Modified';
    305 :  Result:='Use Proxy';
    307 :  Result:='Temporary Redirect';
    400 :  Result:='Bad Request';
    401 :  Result:='Unauthorized';
    402 :  Result:='Payment Required';
    403 :  Result:='Forbidden';
    404 :  Result:='Not Found';
    405 :  Result:='Method Not Allowed';
    406 :  Result:='Not Acceptable';
    407 :  Result:='Proxy Authentication Required';
    408 :  Result:='Request Time-out';
    409 :  Result:='Conflict';
    410 :  Result:='Gone';
    411 :  Result:='Length Required';
    412 :  Result:='Precondition Failed';
    413 :  Result:='Request Entity Too Large';
    414 :  Result:='Request-URI Too Large';
    415 :  Result:='Unsupported Media Type';
    416 :  Result:='Requested range not satisfiable';
    417 :  Result:='Expectation Failed';
    418 :  Result:='I''m a teapot';
    421 :  Result:='Misdirected Request';
    422 :  Result:='Unprocessable Entity';
    423 :  Result:='Locked';
    424 :  Result:='Failed Dependency';
    425 :  Result:='Too Early';
    426 :  Result:='Upgrade Required';
    428 :  Result:='Precondition Required';
    429 :  Result:='Too Many Requests';
    431 :  Result:='Request Header Fields Too Large';
    451 :  Result:='Unavailable For Legal Reasons';

    500 :  Result:='Internal Server Error';
    501 :  Result:='Not Implemented';
    502 :  Result:='Bad Gateway';
    503 :  Result:='Service Unavailable';
    504 :  Result:='Gateway Time-out';
    505 :  Result:='HTTP Version not supported';
  else
    Result:=Format('Unknown status: %d',[aCode]);
  end;
  If aUpperCase then
    Result:=Uppercase(Result);
end;

end.

