{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2017-2018 by the Free Pascal development team

    Windows HTTP Server API based TCustomWebApplication

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  Header of original file http.h:

/*++

Copyright (c) 1998-2002 Microsoft Corporation

Module Name:

    Http.h

Abstract:

    This header corresponds to the HTTP API specification

Revision History:

--*/

}

unit HttpApi;
interface

{
  Automatically converted by H2Pas 1.0.0 from .\packages\fcl-web\examples\echo\httpsys\http.h
  The following command line parameters were used:
    -D
    -w
    -u
    HttpApi
    -l
    httpapi.dll
    -o
    .\packages\fcl-web\examples\echo\httpsys\httpapi.pp
    -s
    .\packages\fcl-web\examples\echo\httpsys\http.h
}

{$MODE OBJFPC}

  uses
    Windows, WinSock2;

  const
    External_library='httpapi.dll'; {Setup as you need}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

  const
    HTTP_INITIALIZE_SERVER = $00000001;    
    HTTP_INITIALIZE_CONFIG = $00000002;    
    HTTP_DEMAND_CBT = $00000004;

{%REGION Windows Vista and newer }

  type
    _HTTP_SERVER_PROPERTY = (HttpServerAuthenticationProperty := 0,
      HttpServerLoggingProperty := 1,HttpServerQosProperty := 2,
      HttpServerTimeoutsProperty := 3,HttpServerQueueLengthProperty := 4,
      HttpServerStateProperty := 5,HttpServer503VerbosityProperty := 6,
      HttpServerBindingProperty := 7,HttpServerExtendedAuthenticationProperty := 8,
      HttpServerListenEndpointProperty := 9,
      HttpServerChannelBindProperty := 10,
      HttpServerProtectionLevelProperty := 11
      );
    HTTP_SERVER_PROPERTY = _HTTP_SERVER_PROPERTY;
    PHTTP_SERVER_PROPERTY = ^_HTTP_SERVER_PROPERTY;

  const
    HTTP_MAX_SERVER_QUEUE_LENGTH = $7FFFFFFF;    
    HTTP_MIN_SERVER_QUEUE_LENGTH = 1;    

  type
    _HTTP_PROPERTY_FLAGS = record
        flag0 : word;
      end;
    HTTP_PROPERTY_FLAGS = _HTTP_PROPERTY_FLAGS;
    PHTTP_PROPERTY_FLAGS = ^_HTTP_PROPERTY_FLAGS;

  const
    bm__HTTP_PROPERTY_FLAGS_Present = $1;
    bp__HTTP_PROPERTY_FLAGS_Present = 0;

  function Present(var a : _HTTP_PROPERTY_FLAGS) : ULONG;
  procedure set_Present(var a : _HTTP_PROPERTY_FLAGS; __Present : ULONG);

  type
    _HTTP_ENABLED_STATE = (HttpEnabledStateActive,HttpEnabledStateInactive
      );
    HTTP_ENABLED_STATE = _HTTP_ENABLED_STATE;
    PHTTP_ENABLED_STATE = ^_HTTP_ENABLED_STATE;

    _HTTP_STATE_INFO = record
        Flags : HTTP_PROPERTY_FLAGS;
        State : HTTP_ENABLED_STATE;
      end;
    HTTP_STATE_INFO = _HTTP_STATE_INFO;
    PHTTP_STATE_INFO = ^_HTTP_STATE_INFO;

    _HTTP_503_RESPONSE_VERBOSITY = (Http503ResponseVerbosityBasic,Http503ResponseVerbosityLimited,
      Http503ResponseVerbosityFull);
    HTTP_503_RESPONSE_VERBOSITY = _HTTP_503_RESPONSE_VERBOSITY;
    PHTTP_503_RESPONSE_VERBOSITY = ^_HTTP_503_RESPONSE_VERBOSITY;

    _HTTP_QOS_SETTING_TYPE = (HttpQosSettingTypeBandwidth,HttpQosSettingTypeConnectionLimit,
      HttpQosSettingTypeFlowRate);
    HTTP_QOS_SETTING_TYPE = _HTTP_QOS_SETTING_TYPE;
    PHTTP_QOS_SETTING_TYPE = ^_HTTP_QOS_SETTING_TYPE;

    _HTTP_QOS_SETTING_INFO = record
        QosType : HTTP_QOS_SETTING_TYPE;
        QosSetting : PVOID;
      end;
    HTTP_QOS_SETTING_INFO = _HTTP_QOS_SETTING_INFO;
    PHTTP_QOS_SETTING_INFO = ^_HTTP_QOS_SETTING_INFO;

    _HTTP_CONNECTION_LIMIT_INFO = record
        Flags : HTTP_PROPERTY_FLAGS;
        MaxConnections : ULONG;
      end;
    HTTP_CONNECTION_LIMIT_INFO = _HTTP_CONNECTION_LIMIT_INFO;
    PHTTP_CONNECTION_LIMIT_INFO = ^_HTTP_CONNECTION_LIMIT_INFO;

    _HTTP_BANDWIDTH_LIMIT_INFO = record
        Flags : HTTP_PROPERTY_FLAGS;
        MaxBandwidth : ULONG;
      end;
    HTTP_BANDWIDTH_LIMIT_INFO = _HTTP_BANDWIDTH_LIMIT_INFO;
    PHTTP_BANDWIDTH_LIMIT_INFO = ^_HTTP_BANDWIDTH_LIMIT_INFO;

    _HTTP_FLOWRATE_INFO = record
        Flags : HTTP_PROPERTY_FLAGS;
        MaxBandwidth : ULONG;
        MaxPeakBandwidth : ULONG;
        BurstSize : ULONG;
      end;
    HTTP_FLOWRATE_INFO = _HTTP_FLOWRATE_INFO;
    PHTTP_FLOWRATE_INFO = ^_HTTP_FLOWRATE_INFO;

  const
    HTTP_MIN_ALLOWED_BANDWIDTH_THROTTLING_RATE: ULONG = 1024;

    HTTP_LIMIT_INFINITE: ULONG = ULONG(-1);


  type
    _HTTP_SERVICE_CONFIG_TIMEOUT_KEY = (IdleConnectionTimeout := 0,HeaderWaitTimeout
      );
    HTTP_SERVICE_CONFIG_TIMEOUT_KEY = _HTTP_SERVICE_CONFIG_TIMEOUT_KEY;
    PHTTP_SERVICE_CONFIG_TIMEOUT_KEY = ^_HTTP_SERVICE_CONFIG_TIMEOUT_KEY;

    HTTP_SERVICE_CONFIG_TIMEOUT_PARAM = USHORT;
    PHTTP_SERVICE_CONFIG_TIMEOUT_PARAM = ^HTTP_SERVICE_CONFIG_TIMEOUT_PARAM;

    _HTTP_SERVICE_CONFIG_TIMEOUT_SET = record
        KeyDesc : HTTP_SERVICE_CONFIG_TIMEOUT_KEY;
        ParamDesc : HTTP_SERVICE_CONFIG_TIMEOUT_PARAM;
      end;
    HTTP_SERVICE_CONFIG_TIMEOUT_SET = _HTTP_SERVICE_CONFIG_TIMEOUT_SET;
    PHTTP_SERVICE_CONFIG_TIMEOUT_SET = ^_HTTP_SERVICE_CONFIG_TIMEOUT_SET;

    _HTTP_TIMEOUT_LIMIT_INFO = record
        Flags : HTTP_PROPERTY_FLAGS;
        EntityBody : USHORT;
        DrainEntityBody : USHORT;
        RequestQueue : USHORT;
        IdleConnection : USHORT;
        HeaderWait : USHORT;
        MinSendRate : ULONG;
      end;
    HTTP_TIMEOUT_LIMIT_INFO = _HTTP_TIMEOUT_LIMIT_INFO;
    PHTTP_TIMEOUT_LIMIT_INFO = ^_HTTP_TIMEOUT_LIMIT_INFO;

    _HTTP_LISTEN_ENDPOINT_INFO = record
        Flags : HTTP_PROPERTY_FLAGS;
        EnableSharing : BOOLEAN;
      end;
    HTTP_LISTEN_ENDPOINT_INFO = _HTTP_LISTEN_ENDPOINT_INFO;
    PHTTP_LISTEN_ENDPOINT_INFO = ^_HTTP_LISTEN_ENDPOINT_INFO;

    _HTTP_SERVER_AUTHENTICATION_DIGEST_PARAMS = record
        DomainNameLength : USHORT;
        DomainName : PWSTR;
        RealmLength : USHORT;
        Realm : PWSTR;
      end;
    HTTP_SERVER_AUTHENTICATION_DIGEST_PARAMS = _HTTP_SERVER_AUTHENTICATION_DIGEST_PARAMS;
    PHTTP_SERVER_AUTHENTICATION_DIGEST_PARAMS = ^_HTTP_SERVER_AUTHENTICATION_DIGEST_PARAMS;

    _HTTP_SERVER_AUTHENTICATION_BASIC_PARAMS = record
        RealmLength : USHORT;
        Realm : PWSTR;
      end;
    HTTP_SERVER_AUTHENTICATION_BASIC_PARAMS = _HTTP_SERVER_AUTHENTICATION_BASIC_PARAMS;
    PHTTP_SERVER_AUTHENTICATION_BASIC_PARAMS = ^_HTTP_SERVER_AUTHENTICATION_BASIC_PARAMS;

  const
    HTTP_AUTH_ENABLE_BASIC = $00000001;    
    HTTP_AUTH_ENABLE_DIGEST = $00000002;    
    HTTP_AUTH_ENABLE_NTLM = $00000004;    
    HTTP_AUTH_ENABLE_NEGOTIATE = $00000008;    
    HTTP_AUTH_ENABLE_KERBEROS = $00000010;    
    HTTP_AUTH_ENABLE_ALL = (((HTTP_AUTH_ENABLE_BASIC or HTTP_AUTH_ENABLE_DIGEST) or HTTP_AUTH_ENABLE_NTLM) or HTTP_AUTH_ENABLE_NEGOTIATE) or HTTP_AUTH_ENABLE_KERBEROS;    

    HTTP_AUTH_EX_FLAG_ENABLE_KERBEROS_CREDENTIAL_CACHING = $01;
    HTTP_AUTH_EX_FLAG_CAPTURE_CREDENTIAL = $02;    

  type
    _HTTP_SERVER_AUTHENTICATION_INFO = record
        Flags : HTTP_PROPERTY_FLAGS;
        AuthSchemes : ULONG;
        ReceiveMutualAuth : BOOLEAN;
        ReceiveContextHandle : BOOLEAN;
        DisableNTLMCredentialCaching : BOOLEAN;
        ExFlags : UCHAR;
        DigestParams : HTTP_SERVER_AUTHENTICATION_DIGEST_PARAMS;
        BasicParams : HTTP_SERVER_AUTHENTICATION_BASIC_PARAMS;
      end;
    HTTP_SERVER_AUTHENTICATION_INFO = _HTTP_SERVER_AUTHENTICATION_INFO;
    PHTTP_SERVER_AUTHENTICATION_INFO = ^_HTTP_SERVER_AUTHENTICATION_INFO;

{%REGION Windows 7 and newer }

  type
    _HTTP_SERVICE_BINDING_TYPE = (HttpServiceBindingTypeNone := 0,HttpServiceBindingTypeW,
      HttpServiceBindingTypeA);
    HTTP_SERVICE_BINDING_TYPE = _HTTP_SERVICE_BINDING_TYPE;

    _HTTP_SERVICE_BINDING_BASE = record
        _Type : HTTP_SERVICE_BINDING_TYPE;
      end;
    HTTP_SERVICE_BINDING_BASE = _HTTP_SERVICE_BINDING_BASE;
    PHTTP_SERVICE_BINDING_BASE = ^_HTTP_SERVICE_BINDING_BASE;

    _HTTP_SERVICE_BINDING_A = record
        Base : HTTP_SERVICE_BINDING_BASE;
        Buffer : PCHAR;
        BufferSize : ULONG;
      end;
    HTTP_SERVICE_BINDING_A = _HTTP_SERVICE_BINDING_A;
    PHTTP_SERVICE_BINDING_A = ^_HTTP_SERVICE_BINDING_A;

    _HTTP_SERVICE_BINDING_W = record
        Base : HTTP_SERVICE_BINDING_BASE;
        Buffer : PWCHAR;
        BufferSize : ULONG;
      end;
    HTTP_SERVICE_BINDING_W = _HTTP_SERVICE_BINDING_W;
    PHTTP_SERVICE_BINDING_W = ^_HTTP_SERVICE_BINDING_W;

    _HTTP_AUTHENTICATION_HARDENING_LEVELS = (HttpAuthenticationHardeningLegacy := 0,
      HttpAuthenticationHardeningMedium,HttpAuthenticationHardeningStrict
      );
    HTTP_AUTHENTICATION_HARDENING_LEVELS = _HTTP_AUTHENTICATION_HARDENING_LEVELS;

  const
    HTTP_CHANNEL_BIND_PROXY = $1;    
    HTTP_CHANNEL_BIND_PROXY_COHOSTING = $20;    
    HTTP_CHANNEL_BIND_NO_SERVICE_NAME_CHECK = $2;    
    HTTP_CHANNEL_BIND_DOTLESS_SERVICE = $4;    
    HTTP_CHANNEL_BIND_SECURE_CHANNEL_TOKEN = $8;    
    HTTP_CHANNEL_BIND_CLIENT_SERVICE = $10;    

  type
    _HTTP_CHANNEL_BIND_INFO = record
        Hardening : HTTP_AUTHENTICATION_HARDENING_LEVELS;
        Flags : ULONG;
        ServiceNames : ^PHTTP_SERVICE_BINDING_BASE;
        NumberOfServiceNames : ULONG;
      end;
    HTTP_CHANNEL_BIND_INFO = _HTTP_CHANNEL_BIND_INFO;
    PHTTP_CHANNEL_BIND_INFO = ^_HTTP_CHANNEL_BIND_INFO;

    _HTTP_REQUEST_CHANNEL_BIND_STATUS = record
        ServiceName : PHTTP_SERVICE_BINDING_BASE;
        ChannelToken : PUCHAR;
        ChannelTokenSize : ULONG;
        Flags : ULONG;
      end;
    HTTP_REQUEST_CHANNEL_BIND_STATUS = _HTTP_REQUEST_CHANNEL_BIND_STATUS;
    PHTTP_REQUEST_CHANNEL_BIND_STATUS = ^_HTTP_REQUEST_CHANNEL_BIND_STATUS;

{%ENDREGION}

  type
    _HTTP_REQUEST_TOKEN_BINDING_INFO = record
        TokenBinding : PUCHAR;
        TokenBindingSize : ULONG;
        TlsUnique : PUCHAR;
        TlsUniqueSize : ULONG;
        KeyType : PWSTR;
      end;
    HTTP_REQUEST_TOKEN_BINDING_INFO = _HTTP_REQUEST_TOKEN_BINDING_INFO;
    PHTTP_REQUEST_TOKEN_BINDING_INFO = ^_HTTP_REQUEST_TOKEN_BINDING_INFO;

  const
    HTTP_LOG_FIELD_DATE = $00000001;    
    HTTP_LOG_FIELD_TIME = $00000002;    
    HTTP_LOG_FIELD_CLIENT_IP = $00000004;    
    HTTP_LOG_FIELD_USER_NAME = $00000008;    
    HTTP_LOG_FIELD_SITE_NAME = $00000010;    
    HTTP_LOG_FIELD_COMPUTER_NAME = $00000020;    
    HTTP_LOG_FIELD_SERVER_IP = $00000040;    
    HTTP_LOG_FIELD_METHOD = $00000080;    
    HTTP_LOG_FIELD_URI_STEM = $00000100;    
    HTTP_LOG_FIELD_URI_QUERY = $00000200;    
    HTTP_LOG_FIELD_STATUS = $00000400;    
    HTTP_LOG_FIELD_WIN32_STATUS = $00000800;    
    HTTP_LOG_FIELD_BYTES_SENT = $00001000;    
    HTTP_LOG_FIELD_BYTES_RECV = $00002000;    
    HTTP_LOG_FIELD_TIME_TAKEN = $00004000;    
    HTTP_LOG_FIELD_SERVER_PORT = $00008000;    
    HTTP_LOG_FIELD_USER_AGENT = $00010000;    
    HTTP_LOG_FIELD_COOKIE = $00020000;    
    HTTP_LOG_FIELD_REFERER = $00040000;    
    HTTP_LOG_FIELD_VERSION = $00080000;    
    HTTP_LOG_FIELD_HOST = $00100000;    
    HTTP_LOG_FIELD_SUB_STATUS = $00200000;    
    HTTP_LOG_FIELD_STREAM_ID = $08000000;    
    HTTP_LOG_FIELD_CLIENT_PORT = $00400000;    
    HTTP_LOG_FIELD_URI = $00800000;    
    HTTP_LOG_FIELD_SITE_ID = $01000000;    
    HTTP_LOG_FIELD_REASON = $02000000;    
    HTTP_LOG_FIELD_QUEUE_NAME = $04000000;    

  type
    _HTTP_LOGGING_TYPE = (HttpLoggingTypeW3C,HttpLoggingTypeIIS,
      HttpLoggingTypeNCSA,HttpLoggingTypeRaw
      );
    HTTP_LOGGING_TYPE = _HTTP_LOGGING_TYPE;
    PHTTP_LOGGING_TYPE = ^_HTTP_LOGGING_TYPE;

    _HTTP_LOGGING_ROLLOVER_TYPE = (HttpLoggingRolloverSize,HttpLoggingRolloverDaily,
      HttpLoggingRolloverWeekly,HttpLoggingRolloverMonthly,
      HttpLoggingRolloverHourly);
    HTTP_LOGGING_ROLLOVER_TYPE = _HTTP_LOGGING_ROLLOVER_TYPE;
    PHTTP_LOGGING_ROLLOVER_TYPE = ^_HTTP_LOGGING_ROLLOVER_TYPE;

  const
    HTTP_MIN_ALLOWED_LOG_FILE_ROLLOVER_SIZE : ULONG = ULONG((1*1024)*1024);

    HTTP_LOGGING_FLAG_LOCAL_TIME_ROLLOVER = $00000001;
    HTTP_LOGGING_FLAG_USE_UTF8_CONVERSION = $00000002;    
    HTTP_LOGGING_FLAG_LOG_ERRORS_ONLY = $00000004;    
    HTTP_LOGGING_FLAG_LOG_SUCCESS_ONLY = $00000008;    

  type
    _HTTP_LOGGING_INFO = record
        Flags : HTTP_PROPERTY_FLAGS;
        LoggingFlags : ULONG;
        SoftwareName : PCWSTR;
        SoftwareNameLength : USHORT;
        DirectoryNameLength : USHORT;
        DirectoryName : PCWSTR;
        Format : HTTP_LOGGING_TYPE;
        Fields : ULONG;
        pExtFields : PVOID;
        NumOfExtFields : USHORT;
        MaxRecordSize : USHORT;
        RolloverType : HTTP_LOGGING_ROLLOVER_TYPE;
        RolloverSize : ULONG;
        pSecurityDescriptor : PSECURITY_DESCRIPTOR;
      end;
    HTTP_LOGGING_INFO = _HTTP_LOGGING_INFO;
    PHTTP_LOGGING_INFO = ^_HTTP_LOGGING_INFO;

    _HTTP_BINDING_INFO = record
        Flags : HTTP_PROPERTY_FLAGS;
        RequestQueueHandle : HANDLE;
      end;
    HTTP_BINDING_INFO = _HTTP_BINDING_INFO;
    PHTTP_BINDING_INFO = ^_HTTP_BINDING_INFO;

{%ENDREGION}

{%REGION Windows 7 and newer}

  type
    _HTTP_PROTECTION_LEVEL_TYPE = (HttpProtectionLevelUnrestricted,HttpProtectionLevelEdgeRestricted,
      HttpProtectionLevelRestricted);
    HTTP_PROTECTION_LEVEL_TYPE = _HTTP_PROTECTION_LEVEL_TYPE;
    PHTTP_PROTECTION_LEVEL_TYPE = ^_HTTP_PROTECTION_LEVEL_TYPE;

    _HTTP_PROTECTION_LEVEL_INFO = record
        Flags : HTTP_PROPERTY_FLAGS;
        Level : HTTP_PROTECTION_LEVEL_TYPE;
      end;
    HTTP_PROTECTION_LEVEL_INFO = _HTTP_PROTECTION_LEVEL_INFO;
    PHTTP_PROTECTION_LEVEL_INFO = ^_HTTP_PROTECTION_LEVEL_INFO;
{%ENDREGION}

{%REGION Windows Vista and newer }

  const
    HTTP_CREATE_REQUEST_QUEUE_FLAG_OPEN_EXISTING = $00000001;    
    HTTP_CREATE_REQUEST_QUEUE_FLAG_CONTROLLER = $00000002;

{%ENDREGION}

  const
    HTTP_RECEIVE_REQUEST_FLAG_COPY_BODY = $00000001;    
    HTTP_RECEIVE_REQUEST_FLAG_FLUSH_BODY = $00000002;

{%REGION Windows Vista and newer}

  const
    HTTP_RECEIVE_REQUEST_ENTITY_BODY_FLAG_FILL_BUFFER = $00000001;

{%ENDREGION}

  const
    HTTP_SEND_RESPONSE_FLAG_DISCONNECT = $00000001;    
    HTTP_SEND_RESPONSE_FLAG_MORE_DATA = $00000002;    
    HTTP_SEND_RESPONSE_FLAG_BUFFER_DATA = $00000004;    
    HTTP_SEND_RESPONSE_FLAG_ENABLE_NAGLING = $00000008;    
    HTTP_SEND_RESPONSE_FLAG_PROCESS_RANGES = $00000020;    
    HTTP_SEND_RESPONSE_FLAG_OPAQUE = $00000040;    
    HTTP_FLUSH_RESPONSE_FLAG_RECURSIVE = $00000001;    

  type
    HTTP_OPAQUE_ID = ULONGLONG;
    PHTTP_OPAQUE_ID = ^HTTP_OPAQUE_ID;

    HTTP_REQUEST_ID = HTTP_OPAQUE_ID;
    PHTTP_REQUEST_ID = ^HTTP_REQUEST_ID;

    HTTP_CONNECTION_ID = HTTP_OPAQUE_ID;
    PHTTP_CONNECTION_ID = ^HTTP_CONNECTION_ID;

    HTTP_RAW_CONNECTION_ID = HTTP_OPAQUE_ID;
    PHTTP_RAW_CONNECTION_ID = ^HTTP_RAW_CONNECTION_ID;

{%REGION Windows Vista and newer}

  type
    HTTP_URL_GROUP_ID = HTTP_OPAQUE_ID;
    PHTTP_URL_GROUP_ID = ^HTTP_URL_GROUP_ID;

    HTTP_SERVER_SESSION_ID = HTTP_OPAQUE_ID;
    PHTTP_SERVER_SESSION_ID = ^HTTP_SERVER_SESSION_ID;

{%ENDREGION}

  const
    HTTP_NULL_ID = QWord(0);

function HTTP_IS_NULL_ID(pid: HTTP_OPAQUE_ID): Boolean; inline;
procedure HTTP_SET_NULL_ID(var pid: HTTP_OPAQUE_ID); inline;
(* error
#define HTTP_NULL_ID            (0ui64)
in define line 892 *)
(* error 
#define HTTP_IS_NULL_ID(pid)    (HTTP_NULL_ID == *(pid))
in define line 893 *)
(* error 
#define HTTP_SET_NULL_ID(pid)   ( *(pid) = HTTP_NULL_ID)
in define line 894 *)

    { was #define dname def_expr }
  const
    HTTP_BYTE_RANGE_TO_EOF : ULONGLONG = ULONGLONG(-(1));


  type
    _HTTP_BYTE_RANGE = record
        StartingOffset : ULARGE_INTEGER;
        Length : ULARGE_INTEGER;
      end;
    HTTP_BYTE_RANGE = _HTTP_BYTE_RANGE;
    PHTTP_BYTE_RANGE = ^_HTTP_BYTE_RANGE;

    _HTTP_VERSION = record
        MajorVersion : USHORT;
        MinorVersion : USHORT;
      end;
    HTTP_VERSION = _HTTP_VERSION;
    PHTTP_VERSION = ^_HTTP_VERSION;

const
  HTTP_VERSION_UNKNOWN: HTTP_VERSION = ( MajorVersion: 0; MinorVersion: 0 );
  HTTP_VERSION_0_9: HTTP_VERSION = ( MajorVersion: 0; MinorVersion: 9 );
  HTTP_VERSION_1_0: HTTP_VERSION = ( MajorVersion: 1; MinorVersion: 0 );
  HTTP_VERSION_1_1: HTTP_VERSION = ( MajorVersion: 1; MinorVersion: 1 );
  HTTP_VERSION_2_0: HTTP_VERSION = ( MajorVersion: 2; MinorVersion: 0 );

procedure HTTP_SET_VERSION(out version: HTTP_VERSION; major, minor: USHORT); inline;
function HTTP_EQUAL_VERSION(constref version: HTTP_VERSION; major, minor: USHORT): Boolean; inline;
function HTTP_GREATER_VERSION(constref version: HTTP_VERSION; major, minor: USHORT): Boolean; inline;
function HTTP_LESS_VERSION(constref version: HTTP_VERSION; major, minor: USHORT): Boolean; inline;
function HTTP_NOT_EQUAL_VERSION(constref version: HTTP_VERSION; major, minor: USHORT): Boolean; inline;
function HTTP_GREATER_EQUAL_VERSION(constref version: HTTP_VERSION; major, minor : USHORT) : Boolean;
function HTTP_LESS_EQUAL_VERSION(constref version: HTTP_VERSION; major, minor : USHORT) : Boolean;

type
  _HTTP_VERB = (HttpVerbUnparsed,HttpVerbUnknown,HttpVerbInvalid,
    HttpVerbOPTIONS,HttpVerbGET,HttpVerbHEAD,
    HttpVerbPOST,HttpVerbPUT,HttpVerbDELETE,
    HttpVerbTRACE,HttpVerbCONNECT,HttpVerbTRACK,
    HttpVerbMOVE,HttpVerbCOPY,HttpVerbPROPFIND,
    HttpVerbPROPPATCH,HttpVerbMKCOL,HttpVerbLOCK,
    HttpVerbUNLOCK,HttpVerbSEARCH,HttpVerbMaximum
    );
  HTTP_VERB = _HTTP_VERB;
  PHTTP_VERB = ^_HTTP_VERB;

  _HTTP_HEADER_ID = (HttpHeaderCacheControl := 0,HttpHeaderConnection := 1,
    HttpHeaderDate := 2,HttpHeaderKeepAlive := 3,
    HttpHeaderPragma := 4,HttpHeaderTrailer := 5,
    HttpHeaderTransferEncoding := 6,HttpHeaderUpgrade := 7,
    HttpHeaderVia := 8,HttpHeaderWarning := 9,
    HttpHeaderAllow := 10,HttpHeaderContentLength := 11,
    HttpHeaderContentType := 12,HttpHeaderContentEncoding := 13,
    HttpHeaderContentLanguage := 14,HttpHeaderContentLocation := 15,
    HttpHeaderContentMd5 := 16,HttpHeaderContentRange := 17,
    HttpHeaderExpires := 18,HttpHeaderLastModified := 19,
    HttpHeaderAccept := 20,HttpHeaderAcceptCharset := 21,
    HttpHeaderAcceptEncoding := 22,HttpHeaderAcceptLanguage := 23,
    HttpHeaderAuthorization := 24,HttpHeaderCookie := 25,
    HttpHeaderExpect := 26,HttpHeaderFrom := 27,
    HttpHeaderHost := 28,HttpHeaderIfMatch := 29,
    HttpHeaderIfModifiedSince := 30,HttpHeaderIfNoneMatch := 31,
    HttpHeaderIfRange := 32,HttpHeaderIfUnmodifiedSince := 33,
    HttpHeaderMaxForwards := 34,HttpHeaderProxyAuthorization := 35,
    HttpHeaderReferer := 36,HttpHeaderRange := 37,
    HttpHeaderTe := 38,HttpHeaderTranslate := 39,
    HttpHeaderUserAgent := 40,HttpHeaderRequestMaximum := 41
    );
  HTTP_HEADER_ID = _HTTP_HEADER_ID;
  PHTTP_HEADER_ID = ^_HTTP_HEADER_ID;

const
  HttpHeaderAcceptRanges = HttpHeaderAccept;
  HttpHeaderAge = HttpHeaderAcceptCharset;
  HttpHeaderEtag = HttpHeaderAcceptEncoding;
  HttpHeaderLocation = HttpHeaderAcceptLanguage;
  HttpHeaderProxyAuthenticate = HttpHeaderAuthorization;
  HttpHeaderRetryAfter = HttpHeaderCookie;
  HttpHeaderServer = HttpHeaderExpect;
  HttpHeaderSetCookie = HttpHeaderFrom;
  HttpHeaderVary = HttpHeaderHost;
  HttpHeaderWwwAuthenticate = HttpHeaderIfMatch;
  HttpHeaderResponseMaximum = HttpHeaderIfModifiedSince;
  HttpHeaderMaximum = HttpHeaderRequestMaximum;

type
  _HTTP_KNOWN_HEADER = record
      RawValueLength : USHORT;
      pRawValue : PCSTR;
    end;
  HTTP_KNOWN_HEADER = _HTTP_KNOWN_HEADER;
  PHTTP_KNOWN_HEADER = ^_HTTP_KNOWN_HEADER;

  _HTTP_UNKNOWN_HEADER = record
      NameLength : USHORT;
      RawValueLength : USHORT;
      pName : PCSTR;
      pRawValue : PCSTR;
    end;
  HTTP_UNKNOWN_HEADER = _HTTP_UNKNOWN_HEADER;
  PHTTP_UNKNOWN_HEADER = ^_HTTP_UNKNOWN_HEADER;

{%REGION Windows Vista and newer}

type
  _HTTP_LOG_DATA_TYPE = (HttpLogDataTypeFields := 0);
  HTTP_LOG_DATA_TYPE = _HTTP_LOG_DATA_TYPE;
  PHTTP_LOG_DATA_TYPE = ^_HTTP_LOG_DATA_TYPE;

  _HTTP_LOG_DATA = record
      _Type : HTTP_LOG_DATA_TYPE;
    end;
  HTTP_LOG_DATA = _HTTP_LOG_DATA;
  PHTTP_LOG_DATA = ^_HTTP_LOG_DATA;

  _HTTP_LOG_FIELDS_DATA = record
      Base : HTTP_LOG_DATA;
      UserNameLength : USHORT;
      UriStemLength : USHORT;
      ClientIpLength : USHORT;
      ServerNameLength : USHORT;
      ServiceNameLength : USHORT;
      ServerIpLength : USHORT;
      MethodLength : USHORT;
      UriQueryLength : USHORT;
      HostLength : USHORT;
      UserAgentLength : USHORT;
      CookieLength : USHORT;
      ReferrerLength : USHORT;
      UserName : PWCHAR;
      UriStem : PWCHAR;
      ClientIp : PCHAR;
      ServerName : PCHAR;
      ServiceName : PCHAR;
      ServerIp : PCHAR;
      Method : PCHAR;
      UriQuery : PCHAR;
      Host : PCHAR;
      UserAgent : PCHAR;
      Cookie : PCHAR;
      Referrer : PCHAR;
      ServerPort : USHORT;
      ProtocolStatus : USHORT;
      Win32Status : ULONG;
      MethodNum : HTTP_VERB;
      SubStatus : USHORT;
    end;
  HTTP_LOG_FIELDS_DATA = _HTTP_LOG_FIELDS_DATA;
  PHTTP_LOG_FIELDS_DATA = ^_HTTP_LOG_FIELDS_DATA;

{%ENDREGION}

type
  _HTTP_DATA_CHUNK_TYPE = (HttpDataChunkFromMemory,HttpDataChunkFromFileHandle,
    HttpDataChunkFromFragmentCache,HttpDataChunkFromFragmentCacheEx,
    HttpDataChunkMaximum);
  HTTP_DATA_CHUNK_TYPE = _HTTP_DATA_CHUNK_TYPE;
  PHTTP_DATA_CHUNK_TYPE = ^_HTTP_DATA_CHUNK_TYPE;

  HTTP_DATA_CHUNK_FROM_MEMORY = record
    pBuffer : PVOID;
    BufferLength : ULONG;
  end;

  HTTP_DATA_CHUNK_FROM_FILE_HANDLE = record
    ByteRange : HTTP_BYTE_RANGE;
    FileHandle : HANDLE;
  end;

  HTTP_DATA_CHUNK_FROM_FRAGMENT_CACHE = record
    FragmentNameLength : USHORT;
    pFragmentName : PCWSTR;
  end;

  HTTP_DATA_CHUNK_FROM_FRAGMENT_CACHE_EX = record
    ByteRange : HTTP_BYTE_RANGE;
    pFragmentName : PCWSTR;
  end;

  _HTTP_DATA_CHUNK = record
      DataChunkType : HTTP_DATA_CHUNK_TYPE;
      case LongInt of
        1: (FromMemory : HTTP_DATA_CHUNK_FROM_MEMORY);
        2: (FromFileHandle : HTTP_DATA_CHUNK_FROM_FILE_HANDLE);
        3: (FromFragmentCache : HTTP_DATA_CHUNK_FROM_FRAGMENT_CACHE);
        4: (FromFragmentCacheEx : HTTP_DATA_CHUNK_FROM_FRAGMENT_CACHE_EX);
    end;
  HTTP_DATA_CHUNK = _HTTP_DATA_CHUNK;
  PHTTP_DATA_CHUNK = ^_HTTP_DATA_CHUNK;

  _HTTP_REQUEST_HEADERS = record
      UnknownHeaderCount : USHORT;
      pUnknownHeaders : PHTTP_UNKNOWN_HEADER;
      TrailerCount : USHORT;
      pTrailers : PHTTP_UNKNOWN_HEADER;
      KnownHeaders : array[0..Ord(HttpHeaderRequestMaximum)-1] of HTTP_KNOWN_HEADER;
    end;
  HTTP_REQUEST_HEADERS = _HTTP_REQUEST_HEADERS;
  PHTTP_REQUEST_HEADERS = ^_HTTP_REQUEST_HEADERS;

  _HTTP_RESPONSE_HEADERS = record
      UnknownHeaderCount : USHORT;
      pUnknownHeaders : PHTTP_UNKNOWN_HEADER;
      TrailerCount : USHORT;
      pTrailers : PHTTP_UNKNOWN_HEADER;
      KnownHeaders : array[0..Ord(HttpHeaderResponseMaximum)-1] of HTTP_KNOWN_HEADER;
    end;
  HTTP_RESPONSE_HEADERS = _HTTP_RESPONSE_HEADERS;
  PHTTP_RESPONSE_HEADERS = ^_HTTP_RESPONSE_HEADERS;

  _HTTP_TRANSPORT_ADDRESS = record
      pRemoteAddress : PSOCKADDR;
      pLocalAddress : PSOCKADDR;
    end;
  HTTP_TRANSPORT_ADDRESS = _HTTP_TRANSPORT_ADDRESS;
  PHTTP_TRANSPORT_ADDRESS = ^_HTTP_TRANSPORT_ADDRESS;

  _HTTP_COOKED_URL = record
      FullUrlLength : USHORT;
      HostLength : USHORT;
      AbsPathLength : USHORT;
      QueryStringLength : USHORT;
      pFullUrl : PCWSTR;
      pHost : PCWSTR;
      pAbsPath : PCWSTR;
      pQueryString : PCWSTR;
    end;
  HTTP_COOKED_URL = _HTTP_COOKED_URL;
  PHTTP_COOKED_URL = ^_HTTP_COOKED_URL;

  HTTP_URL_CONTEXT = ULONGLONG;

{%REGION Windows Vista and newer}

const
  HTTP_URL_FLAG_REMOVE_ALL = $00000001;  

type
  _HTTP_AUTH_STATUS = (HttpAuthStatusSuccess,HttpAuthStatusNotAuthenticated,
    HttpAuthStatusFailure);
  HTTP_AUTH_STATUS = _HTTP_AUTH_STATUS;
  PHTTP_AUTH_STATUS = ^_HTTP_AUTH_STATUS;

  _HTTP_REQUEST_AUTH_TYPE = (HttpRequestAuthTypeNone := 0,HttpRequestAuthTypeBasic,
    HttpRequestAuthTypeDigest,HttpRequestAuthTypeNTLM,
    HttpRequestAuthTypeNegotiate,HttpRequestAuthTypeKerberos
    );
  HTTP_REQUEST_AUTH_TYPE = _HTTP_REQUEST_AUTH_TYPE;
  PHTTP_REQUEST_AUTH_TYPE = ^_HTTP_REQUEST_AUTH_TYPE;

{%ENDREGION}

type
  _HTTP_SSL_CLIENT_CERT_INFO = record
      CertFlags : ULONG;
      CertEncodedSize : ULONG;
      pCertEncoded : PUCHAR;
      Token : HANDLE;
      CertDeniedByMapper : BOOLEAN;
    end;
  HTTP_SSL_CLIENT_CERT_INFO = _HTTP_SSL_CLIENT_CERT_INFO;
  PHTTP_SSL_CLIENT_CERT_INFO = ^_HTTP_SSL_CLIENT_CERT_INFO;

{%REGION Windows 7 and newer }

const
  HTTP_RECEIVE_SECURE_CHANNEL_TOKEN = $1;

{%ENDREGION}

type
  _HTTP_SSL_INFO = record
      ServerCertKeySize : USHORT;
      ConnectionKeySize : USHORT;
      ServerCertIssuerSize : ULONG;
      ServerCertSubjectSize : ULONG;
      pServerCertIssuer : PCSTR;
      pServerCertSubject : PCSTR;
      pClientCertInfo : PHTTP_SSL_CLIENT_CERT_INFO;
      SslClientCertNegotiated : ULONG;
    end;
  HTTP_SSL_INFO = _HTTP_SSL_INFO;
  PHTTP_SSL_INFO = ^_HTTP_SSL_INFO;

  _HTTP_SSL_PROTOCOL_INFO = record
      Protocol : ULONG;
      CipherType : ULONG;
      CipherStrength : ULONG;
      HashType : ULONG;
      HashStrength : ULONG;
      KeyExchangeType : ULONG;
      KeyExchangeStrength : ULONG;
    end;
  HTTP_SSL_PROTOCOL_INFO = _HTTP_SSL_PROTOCOL_INFO;
  PHTTP_SSL_PROTOCOL_INFO = ^_HTTP_SSL_PROTOCOL_INFO;

{%REGION Windows Vista and newer }

type
  _HTTP_REQUEST_INFO_TYPE = (HttpRequestInfoTypeAuth,HttpRequestInfoTypeChannelBind,
    HttpRequestInfoTypeSslProtocol,HttpRequestInfoTypeSslTokenBinding
    );
  HTTP_REQUEST_INFO_TYPE = _HTTP_REQUEST_INFO_TYPE;
  PHTTP_REQUEST_INFO_TYPE = ^_HTTP_REQUEST_INFO_TYPE;

  _HTTP_REQUEST_INFO = record
      InfoType : HTTP_REQUEST_INFO_TYPE;
      InfoLength : ULONG;
      pInfo : PVOID;
    end;
  HTTP_REQUEST_INFO = _HTTP_REQUEST_INFO;
  PHTTP_REQUEST_INFO = ^_HTTP_REQUEST_INFO;
{$ifndef __SECSTATUS_DEFINED__}

type
  SECURITY_STATUS = LONG;
{$define __SECSTATUS_DEFINED__}
{$endif}

const
  HTTP_REQUEST_AUTH_FLAG_TOKEN_FOR_CACHED_CRED = $00000001;  

type
  _HTTP_REQUEST_AUTH_INFO = record
      AuthStatus : HTTP_AUTH_STATUS;
      SecStatus : SECURITY_STATUS;
      Flags : ULONG;
      AuthType : HTTP_REQUEST_AUTH_TYPE;
      AccessToken : HANDLE;
      ContextAttributes : ULONG;
      PackedContextLength : ULONG;
      PackedContextType : ULONG;
      PackedContext : PVOID;
      MutualAuthDataLength : ULONG;
      pMutualAuthData : PCHAR;
      PackageNameLength : USHORT;
      pPackageName : PWSTR;
    end;
  HTTP_REQUEST_AUTH_INFO = _HTTP_REQUEST_AUTH_INFO;
  PHTTP_REQUEST_AUTH_INFO = ^_HTTP_REQUEST_AUTH_INFO;

{%ENDREGION}

type
  _HTTP_REQUEST_V1 = object
      Flags : ULONG;
      ConnectionId : HTTP_CONNECTION_ID;
      RequestId : HTTP_REQUEST_ID;
      UrlContext : HTTP_URL_CONTEXT;
      Version : HTTP_VERSION;
      Verb : HTTP_VERB;
      UnknownVerbLength : USHORT;
      RawUrlLength : USHORT;
      pUnknownVerb : PCSTR;
      pRawUrl : PCSTR;
      CookedUrl : HTTP_COOKED_URL;
      Address : HTTP_TRANSPORT_ADDRESS;
      Headers : HTTP_REQUEST_HEADERS;
      BytesReceived : ULONGLONG;
      EntityChunkCount : USHORT;
      pEntityChunks : PHTTP_DATA_CHUNK;
      RawConnectionId : HTTP_RAW_CONNECTION_ID;
      pSslInfo : PHTTP_SSL_INFO;
    end;
  HTTP_REQUEST_V1 = _HTTP_REQUEST_V1;
  PHTTP_REQUEST_V1 = ^_HTTP_REQUEST_V1;

{%REGION Windows Vista and newer }

type
  { the C code uses an anonymous struct here to avoid duplication... :/ }
  _HTTP_REQUEST_V2 = object(_HTTP_REQUEST_V1)
      RequestInfoCount : USHORT;
      pRequestInfo : PHTTP_REQUEST_INFO;
    end;
  HTTP_REQUEST_V2 = _HTTP_REQUEST_V2;
  PHTTP_REQUEST_V2 = ^_HTTP_REQUEST_V2;

  HTTP_REQUEST = HTTP_REQUEST_V2;
{%ENDREGION}

{%REGION Older than Windows Vista }

{type
  HTTP_REQUEST = HTTP_REQUEST_V1;}

{%ENDREGION}

type
  PHTTP_REQUEST = ^HTTP_REQUEST;

const
  HTTP_REQUEST_FLAG_MORE_ENTITY_BODY_EXISTS = $00000001;  
  HTTP_REQUEST_FLAG_IP_ROUTED = $00000002;  
  HTTP_REQUEST_FLAG_HTTP2 = $00000004;  

type
  _HTTP_RESPONSE_V1 = object
      Flags : ULONG;
      Version : HTTP_VERSION;
      StatusCode : USHORT;
      ReasonLength : USHORT;
      pReason : PCSTR;
      Headers : HTTP_RESPONSE_HEADERS;
      EntityChunkCount : USHORT;
      pEntityChunks : PHTTP_DATA_CHUNK;
    end;
  HTTP_RESPONSE_V1 = _HTTP_RESPONSE_V1;
  PHTTP_RESPONSE_V1 = ^_HTTP_RESPONSE_V1;

{%REGION Windows Vista and newer }

const
  HTTP_RESPONSE_FLAG_MULTIPLE_ENCODINGS_AVAILABLE = $00000001;  

type
  _HTTP_RESPONSE_INFO_TYPE = (HttpResponseInfoTypeMultipleKnownHeaders,
    HttpResponseInfoTypeAuthenticationProperty,
    HttpResponseInfoTypeQoSProperty,HttpResponseInfoTypeChannelBind
    );
  HTTP_RESPONSE_INFO_TYPE = _HTTP_RESPONSE_INFO_TYPE;
  PHTTP_RESPONSE_INFO_TYPE = _HTTP_RESPONSE_INFO_TYPE;

  _HTTP_RESPONSE_INFO = record
      _Type : HTTP_RESPONSE_INFO_TYPE;
      Length : ULONG;
      pInfo : PVOID;
    end;
  HTTP_RESPONSE_INFO = _HTTP_RESPONSE_INFO;
  PHTTP_RESPONSE_INFO = ^_HTTP_RESPONSE_INFO;

const
  HTTP_RESPONSE_INFO_FLAGS_PRESERVE_ORDER = $00000001;  

type
  _HTTP_MULTIPLE_KNOWN_HEADERS = record
      HeaderId : HTTP_HEADER_ID;
      Flags : ULONG;
      KnownHeaderCount : USHORT;
      KnownHeaders : PHTTP_KNOWN_HEADER;
    end;
  HTTP_MULTIPLE_KNOWN_HEADERS = _HTTP_MULTIPLE_KNOWN_HEADERS;
  PHTTP_MULTIPLE_KNOWN_HEADERS = ^_HTTP_MULTIPLE_KNOWN_HEADERS;

  { the C code uses an anonymous struct here to avoid duplication... :/ }
  _HTTP_RESPONSE_V2 = object(_HTTP_RESPONSE_V1)
      ResponseInfoCount : USHORT;
      pResponseInfo : PHTTP_RESPONSE_INFO;
    end;
  HTTP_RESPONSE_V2 = _HTTP_RESPONSE_V2;
  PHTTP_RESPONSE_V2 = ^_HTTP_RESPONSE_V2;

  HTTP_RESPONSE = HTTP_RESPONSE_V2;

{%ENDREGION}

{%REGION Older than Windows Vista}

{type
  HTTP_RESPONSE = HTTP_RESPONSE_V1;}

{%ENDREGION}

type
  PHTTP_RESPONSE = ^HTTP_RESPONSE;

  _HTTPAPI_VERSION = record
      HttpApiMajorVersion : USHORT;
      HttpApiMinorVersion : USHORT;
    end;
  HTTPAPI_VERSION = _HTTPAPI_VERSION;
  PHTTPAPI_VERSION = ^_HTTPAPI_VERSION;

const
  HTTPAPI_VERSION_2: HTTPAPI_VERSION = ( HttpApiMajorVersion: 2; HttpApiMinorVersion: 0 );
  HTTPAPI_VERSION_1: HTTPAPI_VERSION = ( HttpApiMajorVersion: 1; HttpApiMinorVersion: 0 );

    function HTTPAPI_EQUAL_VERSION(constref version: HTTPAPI_VERSION; major, minor: USHORT): Boolean; inline;
    function HTTPAPI_GREATER_VERSION(constref version: HTTPAPI_VERSION; major, minor: USHORT): Boolean; inline;
    function HTTPAPI_LESS_VERSION(constref version: HTTPAPI_VERSION; major, minor: USHORT): Boolean; inline;
    function HTTPAPI_VERSION_GREATER_OR_EQUAL(constref version: HTTPAPI_VERSION; major, minor: USHORT) : Boolean; inline;


  type
    _HTTP_CACHE_POLICY_TYPE = (HttpCachePolicyNocache,HttpCachePolicyUserInvalidates,
      HttpCachePolicyTimeToLive,HttpCachePolicyMaximum
      );
    HTTP_CACHE_POLICY_TYPE = _HTTP_CACHE_POLICY_TYPE;
    PHTTP_CACHE_POLICY_TYPE = ^_HTTP_CACHE_POLICY_TYPE;

    _HTTP_CACHE_POLICY = record
        Policy : HTTP_CACHE_POLICY_TYPE;
        SecondsToLive : ULONG;
      end;
    HTTP_CACHE_POLICY = _HTTP_CACHE_POLICY;
    PHTTP_CACHE_POLICY = ^_HTTP_CACHE_POLICY;

  type
    _HTTP_SERVICE_CONFIG_ID = (
      HttpServiceConfigIPListenList,
      HttpServiceConfigSSLCertInfo,
      HttpServiceConfigUrlAclInfo,
      HttpServiceConfigTimeout,
      HttpServiceConfigCache,
{%REGION Windows 8 and newer }
      HttpServiceConfigSslSniCertInfo,
      HttpServiceConfigSslCcsCertInfo,
{%ENDREGION}
      HttpServiceConfigMax
      );
    HTTP_SERVICE_CONFIG_ID = _HTTP_SERVICE_CONFIG_ID;
    PHTTP_SERVICE_CONFIG_ID = ^_HTTP_SERVICE_CONFIG_ID;

    _HTTP_SERVICE_CONFIG_QUERY_TYPE = (HttpServiceConfigQueryExact,HttpServiceConfigQueryNext,
      HttpServiceConfigQueryMax);
    HTTP_SERVICE_CONFIG_QUERY_TYPE = _HTTP_SERVICE_CONFIG_QUERY_TYPE;
    PHTTP_SERVICE_CONFIG_QUERY_TYPE = ^_HTTP_SERVICE_CONFIG_QUERY_TYPE;

    _HTTP_SERVICE_CONFIG_SSL_KEY = record
        pIpPort : PSOCKADDR;
      end;
    HTTP_SERVICE_CONFIG_SSL_KEY = _HTTP_SERVICE_CONFIG_SSL_KEY;
    PHTTP_SERVICE_CONFIG_SSL_KEY = ^_HTTP_SERVICE_CONFIG_SSL_KEY;

{%REGION Windows 8 and newer }

  type
    _HTTP_SERVICE_CONFIG_SSL_SNI_KEY = record
        IpPort : SOCKADDR_STORAGE;
        Host : PWSTR;
      end;
    HTTP_SERVICE_CONFIG_SSL_SNI_KEY = _HTTP_SERVICE_CONFIG_SSL_SNI_KEY;
    PHTTP_SERVICE_CONFIG_SSL_SNI_KEY = ^_HTTP_SERVICE_CONFIG_SSL_SNI_KEY;

    _HTTP_SERVICE_CONFIG_SSL_CCS_KEY = record
        LocalAddress : SOCKADDR_STORAGE;
      end;
    HTTP_SERVICE_CONFIG_SSL_CCS_KEY = _HTTP_SERVICE_CONFIG_SSL_CCS_KEY;
    PHTTP_SERVICE_CONFIG_SSL_CCS_KEY = ^_HTTP_SERVICE_CONFIG_SSL_CCS_KEY;

{%ENDREGION}

  type
    _HTTP_SERVICE_CONFIG_SSL_PARAM = record
        SslHashLength : ULONG;
        pSslHash : PVOID;
        AppId : GUID;
        pSslCertStoreName : PWSTR;
        DefaultCertCheckMode : DWORD;
        DefaultRevocationFreshnessTime : DWORD;
        DefaultRevocationUrlRetrievalTimeout : DWORD;
        pDefaultSslCtlIdentifier : PWSTR;
        pDefaultSslCtlStoreName : PWSTR;
        DefaultFlags : DWORD;
      end;
    HTTP_SERVICE_CONFIG_SSL_PARAM = _HTTP_SERVICE_CONFIG_SSL_PARAM;
    PHTTP_SERVICE_CONFIG_SSL_PARAM = ^_HTTP_SERVICE_CONFIG_SSL_PARAM;

  const
    HTTP_SERVICE_CONFIG_SSL_FLAG_USE_DS_MAPPER = $00000001;    
    HTTP_SERVICE_CONFIG_SSL_FLAG_NEGOTIATE_CLIENT_CERT = $00000002;

{%REGION Older than Windows Vista }

  const
    HTTP_SERVICE_CONFIG_SSL_FLAG_NO_RAW_FILTER = $00000004;    

{%ENDREGION}

  const
    HTTP_SERVICE_CONFIG_SSL_FLAG_REJECT = $00000008;    

  type
    _HTTP_SERVICE_CONFIG_SSL_SET = record
        KeyDesc : HTTP_SERVICE_CONFIG_SSL_KEY;
        ParamDesc : HTTP_SERVICE_CONFIG_SSL_PARAM;
      end;
    HTTP_SERVICE_CONFIG_SSL_SET = _HTTP_SERVICE_CONFIG_SSL_SET;
    PHTTP_SERVICE_CONFIG_SSL_SET = ^_HTTP_SERVICE_CONFIG_SSL_SET;

{%REGION Windows 8 and newer}

  type
    _HTTP_SERVICE_CONFIG_SSL_SNI_SET = record
        KeyDesc : HTTP_SERVICE_CONFIG_SSL_SNI_KEY;
        ParamDesc : HTTP_SERVICE_CONFIG_SSL_PARAM;
      end;
    HTTP_SERVICE_CONFIG_SSL_SNI_SET = _HTTP_SERVICE_CONFIG_SSL_SNI_SET;
    PHTTP_SERVICE_CONFIG_SSL_SNI_SET = ^_HTTP_SERVICE_CONFIG_SSL_SNI_SET;

    _HTTP_SERVICE_CONFIG_SSL_CCS_SET = record
        KeyDesc : HTTP_SERVICE_CONFIG_SSL_CCS_KEY;
        ParamDesc : HTTP_SERVICE_CONFIG_SSL_PARAM;
      end;
    HTTP_SERVICE_CONFIG_SSL_CCS_SET = _HTTP_SERVICE_CONFIG_SSL_CCS_SET;
    PHTTP_SERVICE_CONFIG_SSL_CCS_SET = ^_HTTP_SERVICE_CONFIG_SSL_CCS_SET;

{%ENDREGION}

  type
    _HTTP_SERVICE_CONFIG_SSL_QUERY = record
        QueryDesc : HTTP_SERVICE_CONFIG_QUERY_TYPE;
        KeyDesc : HTTP_SERVICE_CONFIG_SSL_KEY;
        dwToken : DWORD;
      end;
    HTTP_SERVICE_CONFIG_SSL_QUERY = _HTTP_SERVICE_CONFIG_SSL_QUERY;
    PHTTP_SERVICE_CONFIG_SSL_QUERY = ^_HTTP_SERVICE_CONFIG_SSL_QUERY;

{%REGION Windows 8 and newer }

  type
    _HTTP_SERVICE_CONFIG_SSL_SNI_QUERY = record
        QueryDesc : HTTP_SERVICE_CONFIG_QUERY_TYPE;
        KeyDesc : HTTP_SERVICE_CONFIG_SSL_SNI_KEY;
        dwToken : DWORD;
      end;
    HTTP_SERVICE_CONFIG_SSL_SNI_QUERY = _HTTP_SERVICE_CONFIG_SSL_SNI_QUERY;
    PHTTP_SERVICE_CONFIG_SSL_SNI_QUERY = ^_HTTP_SERVICE_CONFIG_SSL_SNI_QUERY;

    _HTTP_SERVICE_CONFIG_SSL_CCS_QUERY = record
        QueryDesc : HTTP_SERVICE_CONFIG_QUERY_TYPE;
        KeyDesc : HTTP_SERVICE_CONFIG_SSL_CCS_KEY;
        dwToken : DWORD;
      end;
    HTTP_SERVICE_CONFIG_SSL_CCS_QUERY = _HTTP_SERVICE_CONFIG_SSL_CCS_QUERY;
    PHTTP_SERVICE_CONFIG_SSL_CCS_QUERY = ^_HTTP_SERVICE_CONFIG_SSL_CCS_QUERY;

{%ENDREGION}

  type
    _HTTP_SERVICE_CONFIG_IP_LISTEN_PARAM = record
        AddrLength : USHORT;
        pAddress : PSOCKADDR;
      end;
    HTTP_SERVICE_CONFIG_IP_LISTEN_PARAM = _HTTP_SERVICE_CONFIG_IP_LISTEN_PARAM;
    PHTTP_SERVICE_CONFIG_IP_LISTEN_PARAM = ^_HTTP_SERVICE_CONFIG_IP_LISTEN_PARAM;

    _HTTP_SERVICE_CONFIG_IP_LISTEN_QUERY = record
        AddrCount : ULONG;
        AddrList : array[0..(ANYSIZE_ARRAY)-1] of SOCKADDR_STORAGE;
      end;
    HTTP_SERVICE_CONFIG_IP_LISTEN_QUERY = _HTTP_SERVICE_CONFIG_IP_LISTEN_QUERY;
    PHTTP_SERVICE_CONFIG_IP_LISTEN_QUERY = ^_HTTP_SERVICE_CONFIG_IP_LISTEN_QUERY;

    _HTTP_SERVICE_CONFIG_URLACL_KEY = record
        pUrlPrefix : PWSTR;
      end;
    HTTP_SERVICE_CONFIG_URLACL_KEY = _HTTP_SERVICE_CONFIG_URLACL_KEY;
    PHTTP_SERVICE_CONFIG_URLACL_KEY = ^_HTTP_SERVICE_CONFIG_URLACL_KEY;

    _HTTP_SERVICE_CONFIG_URLACL_PARAM = record
        pStringSecurityDescriptor : PWSTR;
      end;
    HTTP_SERVICE_CONFIG_URLACL_PARAM = _HTTP_SERVICE_CONFIG_URLACL_PARAM;
    PHTTP_SERVICE_CONFIG_URLACL_PARAM = ^_HTTP_SERVICE_CONFIG_URLACL_PARAM;

    _HTTP_SERVICE_CONFIG_URLACL_SET = record
        KeyDesc : HTTP_SERVICE_CONFIG_URLACL_KEY;
        ParamDesc : HTTP_SERVICE_CONFIG_URLACL_PARAM;
      end;
    HTTP_SERVICE_CONFIG_URLACL_SET = _HTTP_SERVICE_CONFIG_URLACL_SET;
    PHTTP_SERVICE_CONFIG_URLACL_SET = ^_HTTP_SERVICE_CONFIG_URLACL_SET;

    _HTTP_SERVICE_CONFIG_URLACL_QUERY = record
        QueryDesc : HTTP_SERVICE_CONFIG_QUERY_TYPE;
        KeyDesc : HTTP_SERVICE_CONFIG_URLACL_KEY;
        dwToken : DWORD;
      end;
    HTTP_SERVICE_CONFIG_URLACL_QUERY = _HTTP_SERVICE_CONFIG_URLACL_QUERY;
    PHTTP_SERVICE_CONFIG_URLACL_QUERY = ^_HTTP_SERVICE_CONFIG_URLACL_QUERY;

    _HTTP_SERVICE_CONFIG_CACHE_KEY = (MaxCacheResponseSize := 0,CacheRangeChunkSize
      );
    HTTP_SERVICE_CONFIG_CACHE_KEY = _HTTP_SERVICE_CONFIG_CACHE_KEY;
    PHTTP_SERVICE_CONFIG_CACHE_KEY = ^_HTTP_SERVICE_CONFIG_CACHE_KEY;

    HTTP_SERVICE_CONFIG_CACHE_PARAM = ULONG;
    PHTTP_SERVICE_CONFIG_CACHE_PARAM = ^HTTP_SERVICE_CONFIG_CACHE_PARAM;

    HTTP_SERVICE_CONFIG_CACHE_SET = record
        KeyDesc : HTTP_SERVICE_CONFIG_CACHE_KEY;
        ParamDesc : HTTP_SERVICE_CONFIG_CACHE_PARAM;
      end;
    PHTTP_SERVICE_CONFIG_CACHE_SET = ^HTTP_SERVICE_CONFIG_CACHE_SET;

{$ifdef InLazIDE}
{$macro on}
{$define winapi:=stdcall}
{$endif}

function HttpInitialize(Version: HTTPAPI_VERSION; Flags: ULONG; pReserved: PVOID): ULONG; WinApi; external External_library name 'HttpInitialize';
function HttpTerminate(Flags: ULONG; pReserved: PVOID): ULONG; WinApi; external External_library name 'HttpTerminate';
function HttpCreateHttpHandle(RequestQueueHandle: PHANDLE; Reserved: ULONG): ULONG; WinApi; external External_library name 'HttpCreateHttpHandle';
function HttpCreateHttpHandle(out RequestQueueHandle: HANDLE; Reserved: ULONG): ULONG; WinApi; external External_library name 'HttpCreateHttpHandle';

{%REGION Windows Vista and newer }

function HttpCreateRequestQueue(Version: HTTPAPI_VERSION; Name: PCWSTR; SecurityAttributes: PSECURITYATTRIBUTES; Flags: ULONG; RequestQueueHandle: PHANDLE): ULONG; WinApi; external External_library name 'HttpCreateRequestQueue';
function HttpCreateRequestQueue(Version: HTTPAPI_VERSION; Name: PCWSTR; SecurityAttributes: PSECURITYATTRIBUTES; Flags: ULONG; out RequestQueueHandle: HANDLE): ULONG; WinApi; external External_library name 'HttpCreateRequestQueue';
function HttpCloseRequestQueue(RequestQueueHandle: HANDLE): ULONG; WinApi; external External_library name 'HttpCloseRequestQueue';
function HttpSetRequestQueueProperty(RequestQueueHandle: HANDLE; _Property: HTTP_SERVER_PROPERTY; PropertyInformation: PVOID; PropertyInformationLength: ULONG; Reserved1: ULONG; Reserved2: PVOID): ULONG; WinApi; external External_library name 'HttpSetRequestQueueProperty';
function HttpQueryRequestQueueProperty(RequestQueueProperty: HANDLE; _Property: HTTP_SERVER_PROPERTY; PropertyInformation: PVOID; PropertyInformationLength: ULONG; Reserved1: ULONG; ReturnLength: PULONG; Reserved2: PVOID): ULONG; WinApi; external External_library name 'HttpQueryRequestQueueProperty';
function HttpShutdownRequestQueue(RequestQueueHandle: HANDLE): ULONG; WinApi; external External_library name 'HttpShutdownRequestQueue';

{%ENDREGION}

function HttpReceiveClientCertificate(RequestQueueHandle: HANDLE; ConnectionId: HTTP_CONNECTION_ID; Flags: ULONG; SslClientCertInfo: PHTTP_SSL_CLIENT_CERT_INFO; SslClientCertInfoSize: ULONG; BytesReceived: PULONG; Overlapped: LPOVERLAPPED): ULONG; WinApi; external External_library name 'HttpReceiveClientCertificate';

{%REGION Windows Vista and newer }

function HttpCreateServerSession(Version: HTTPAPI_VERSION; ServerSessionId: PHTTP_SERVER_SESSION_ID; Reserved: ULONG): ULONG; WinApi; external External_library name 'HttpCreateServerSession';
function HttpCreateServerSession(Version: HTTPAPI_VERSION; out ServerSessionId: HTTP_SERVER_SESSION_ID; Reserved: ULONG): ULONG; WinApi; external External_library name 'HttpCreateServerSession';
function HttpCloseServerSession(ServerSessionId: HTTP_SERVER_SESSION_ID): ULONG; WinApi; external External_library name 'HttpCloseServerSession';
function HttpQueryServerSessionProperty(ServerSessionId: HTTP_SERVER_SESSION_ID; _Property: HTTP_SERVER_PROPERTY; PropertyInformation: PVOID; PropertyInformationLength: ULONG; ReturnLength: PULONG): ULONG; WinApi; external External_library name 'HttpQueryServerSessionProperty';
function HttpSetServerSessionProperty(ServerSessionId: HTTP_SERVER_SESSION_ID; _Property: HTTP_SERVER_PROPERTY; PropertyInformation: PVOID; PropertyInformationLength: ULONG): ULONG; WinApi; external External_library name 'HttpSetServerSessionProperty';

{%ENDREGION}

function HttpAddUrl(RequestQueueHandle: HANDLE; FullyQualifiedUrl: PCWSTR; Reserved: PVOID): ULONG; WinApi; external External_library name 'HttpAddUrl';
function HttpRemoveUrl(RequestQueueHandle: HANDLE; FullyQualifiedUrl: PCWSTR): ULONG; WinApi; external External_library name 'HttpRemoveUrl';

{%REGION Windows Vista and newer }

function HttpCreateUrlGroup(ServerSessionId: HTTP_SERVER_SESSION_ID; pUrlGroupId: PHTTP_URL_GROUP_ID; Reserved: ULONG): ULONG; WinApi; external External_library name 'HttpCreateUrlGroup';
function HttpCreateUrlGroup(ServerSessionId: HTTP_SERVER_SESSION_ID; out pUrlGroupId: HTTP_URL_GROUP_ID; Reserved: ULONG): ULONG; WinApi; external External_library name 'HttpCreateUrlGroup';
function HttpCloseUrlGroup(UrlGroupId: HTTP_URL_GROUP_ID): ULONG; WinApi; external External_library name 'HttpCloseUrlGroup';
function HttpAddUrlToUrlGroup(UrlGroupId: HTTP_URL_GROUP_ID; pFullyQualifiedUrl: PCWSTR; UrlContext: HTTP_URL_CONTEXT; Reserved: ULONG): ULONG; WinApi; external External_library name 'HttpAddUrlToUrlGroup';
function HttpRemoveUrlFromUrlGroup(UrlGroupId: HTTP_URL_GROUP_ID; pFullyQualifiedUrl: PCWSTR; Flags: ULONG): ULONG; WinApi; external External_library name 'HttpRemoveUrlFromUrlGroup';
function HttpSetUrlGroupProperty(UrlGroupId: HTTP_URL_GROUP_ID; _Property: HTTP_SERVER_PROPERTY; PropertyInformation: PVOID; PropertyInformationLength: ULONG): ULONG; WinApi; external External_library name 'HttpSetUrlGroupProperty';
function HttpQueryUrlGroupProperty(UrlGroupId: HTTP_URL_GROUP_ID; _Property: HTTP_SERVER_PROPERTY; PropertyInformation: PVOID; PropertyInformationLength: ULONG; ReturnLength: PULONG): ULONG; WinApi; external External_library name 'HttpQueryUrlGroupProperty';

{%ENDREGION}

{%REGION Windows 8 and newer }

function HttpPrepareUrl(Reserved: PVOID; Flags: ULONG; Url: PCWSTR; out PreparedUrl: PWSTR): ULONG; WinApi; external External_library name 'HttpPrepareUrl';

{%ENDREGION}

function HttpReceiveHttpRequest(RequestQueueHandle: HANDLE; RequestId: HTTP_REQUEST_ID; Flags: ULONG; RequestBuffer: PHTTP_REQUEST; RequestBufferLength: ULONG; BytesReturned: PULONG; Overlapped: LPOVERLAPPED): ULONG; WinApi; external External_library name 'HttpReceiveHttpRequest';
function HttpReceiveRequestEntityBody(RequestQueueHandle: HANDLE; RequestId: HTTP_REQUEST_ID; Flags: ULONG; EntityBuffer: PVOID; EntityBufferLength: ULONG; BytesReturned: PULONG; Overlapped: LPOVERLAPPED): ULONG; WinApi; external External_library name 'HttpReceiveRequestEntityBody';

{%REGION Windows Vista and newer }

function HttpSendHttpResponse(RequestQueueHandle: HANDLE; RequestId: HTTP_REQUEST_ID; Flags: ULONG; HttpResponse: PHTTP_RESPONSE; CachePolicy: PHTTP_CACHE_POLICY; BytesSent: PULONG; Reserved1: PVOID; Reserved2: ULONG; Overlapped: LPOVERLAPPED; LogData: PHTTP_LOG_DATA): ULONG; WinApi; external External_library name 'HttpSendHttpResponse';

{%ENDREGION}

{%REGION Older than Windows Vista }

//function HttpSendHttpResponse(ReqQueueHandle: HANDLE; RequestId: HTTP_REQUEST_ID; Flags: ULONG; pHttpResponse: PHTTP_RESPONSE; pReserved1: PVOID; pBytesSent: PULONG; pReserved2: PVOID; Reserved3: ULONG; Overlapped: LPOVERLAPPED; pReserved4: PVOID): ULONG; WinApi; external External_library name 'HttpSendHttpResponse';

{%ENDREGION}

{%REGION Windows Vista and newer }

function HttpSendResponseEntityBody(RequestQueueHandle: HANDLE; RequestId: HTTP_REQUEST_ID; Flags: ULONG; EntityChunkCount: USHORT; EntityChunks: PHTTP_DATA_CHUNK; BytesSent: PULONG; Reserved1: PVOID; Reserved2: PULONG; Overlapped: LPOVERLAPPED; LogData: PHTTP_LOG_DATA): ULONG; WinApi; external External_library name 'HttpSendResponseEntityBody';

{%ENDREGION}

{%REGION Older than Windows Vista }

//function HttpSendResponseEntityBody(ReqQueueHandle: HANDLE; RequestId: HTTP_REQUEST_ID; Flags: ULONG; EntityChunkCount: USHORT; pEntityChunks: PHTTP_DATA_CHUNK; pBytesSent: PULONG; pReserved1: PVOID; Reserved2: PULONG; Overlapped: LPOVERLAPPED; pReserved3: PVOID): ULONG; WinApi; external External_library name 'HttpSendResponseEntityBody';

{%ENDREGION}

function HttpDeclarePush(RequestQueueHandle: HANDLE; RequestId: HTTP_REQUEST_ID; Verb: HTTP_VERB; Path: PCWSTR; Query: PCSTR; Headers: PHTTP_REQUEST_HEADERS): ULONG; WinApi; external External_library name 'HttpDeclarePush';
function HttpWaitForDisconnect(RequestQueueHandle: HANDLE; ConnectionId: HTTP_CONNECTION_ID; Overlapped: LPOVERLAPPED): ULONG; WinApi; external External_library name 'HttpWaitForDisconnect';

{%REGION Windows Vista and newer }

function HttpWaitForDisconnectEx(RequestQueueHandle: HANDLE; ConnectionId: HTTP_CONNECTION_ID; Reserved: ULONG; Overlapped: LPOVERLAPPED): ULONG; WinApi; external External_library name 'HttpWaitForDisconnectEx';
function HttpCancelHttpRequest(RequestQueueHandle: HANDLE; RequestId: HTTP_REQUEST_ID; Overlapped: LPOVERLAPPED): ULONG; WinApi; external External_library name 'HttpCancelHttpRequest';
function HttpWaitForDemandStart(RequestQueueHandle: HANDLE; Overlapped: LPOVERLAPPED): ULONG; WinApi; external External_library name 'HttpWaitForDemandStart';

{%ENDREGION}

function HttpFlushResponseCache(RequestQueueHandle: HANDLE; UrlPrefix: PCWSTR; Flags: ULONG; Overlapped: LPOVERLAPPED): ULONG; WinApi; external External_library name 'HttpFlushResponseCache';
function HttpAddFragmentToCache(RequestQueueHandle: HANDLE; UrlPrefix: PCWSTR; DataChunk: PHTTP_DATA_CHUNK; CachePolicy: PHTTP_CACHE_POLICY; Overlapped: LPOVERLAPPED): ULONG; WinApi; external External_library name 'HttpAddFragmentToCache';
function HttpReadFragmentFromCache(RequestQueueHandle: HANDLE; UrlPrefix: PCWSTR; ByteRange: PHTTP_BYTE_RANGE; Buffer: PVOID; BufferLength: ULONG; BytesRead: PULONG; Overlapped: LPOVERLAPPED): ULONG; WinApi; external External_library name 'HttpReadFragmentFromCache';

function HttpSetServiceConfiguration(ServiceHandle: HANDLE; ConfigId: HTTP_SERVICE_CONFIG_ID; pConfigInformation: PVOID; ConfigInformationLength: ULONG; pOverlapped: LPOVERLAPPED): ULONG; WinApi; external External_library name 'HttpSetServiceConfiguration';
function HttpDeleteServiceConfiguration(ServiceHandle: HANDLE; ConfigId: HTTP_SERVICE_CONFIG_ID; pConfigInformation: PVOID; ConfigInformationLength: ULONG; pOverlapped: LPOVERLAPPED): ULONG; WinApi; external External_library name 'HttpDeleteServiceConfiguration';
function HttpQueryServiceConfiguration(ServiceHandle: HANDLE; ConfigId: HTTP_SERVICE_CONFIG_ID; pInput: PVOID; InputLength: ULONG; pOutput: PVOID; OutputLength: ULONg; pReturnLength: PULONG; pOverlapped: LPOVERLAPPED): ULONG; WinApi; external External_library name 'HttpQueryServiceConfiguration';
{ this is only available from Windows 10 version 1703 on, so handle that in the
  implementation; ideally this would be marked with "delayed" }
function HttpUpdateServiceConfiguration(ServiceHandle: HANDLE; ConfigId: HTTP_SERVICE_CONFIG_ID; ConfigInfo: PVOID; ConfigInfoLength: ULONG; Overlapped: LPOVERLAPPED): ULONG; WinApi;

implementation

  uses
    SysUtils;

  function Present(var a : _HTTP_PROPERTY_FLAGS) : ULONG;
    begin
      Present:=(a.flag0 and bm__HTTP_PROPERTY_FLAGS_Present) shr bp__HTTP_PROPERTY_FLAGS_Present;
    end;

  procedure set_Present(var a : _HTTP_PROPERTY_FLAGS; __Present : ULONG);
    begin
      a.flag0:=a.flag0 or ((__Present shl bp__HTTP_PROPERTY_FLAGS_Present) and bm__HTTP_PROPERTY_FLAGS_Present);
    end;

  function HTTP_IS_NULL_ID(pid: HTTP_OPAQUE_ID): Boolean;
    begin
      HTTP_IS_NULL_ID := pid = HTTP_NULL_ID;
    end;

  procedure HTTP_SET_NULL_ID(var pid: HTTP_OPAQUE_ID);
    begin
      pid := HTTP_NULL_ID;
    end;

  procedure HTTP_SET_VERSION(out version: HTTP_VERSION; major, minor: USHORT);
    begin
      version.MajorVersion := major;
      version.MinorVersion := minor;
    end;

  function HTTP_EQUAL_VERSION(constref version: HTTP_VERSION; major, minor: USHORT): Boolean;
    begin
      HTTP_EQUAL_VERSION := (version.MajorVersion = major) and (version.MinorVersion = minor);
    end;

  function HTTP_GREATER_VERSION(constref version: HTTP_VERSION; major, minor: USHORT): Boolean;
    begin
      HTTP_GREATER_VERSION := (version.MajorVersion > major) or ((version.MajorVersion = major) and (version.MinorVersion > minor));
    end;

  function HTTP_LESS_VERSION(constref version: HTTP_VERSION; major, minor: USHORT): Boolean;
    begin
      HTTP_LESS_VERSION := (version.MajorVersion < major) or ((version.MajorVersion = major) and (version.MinorVersion < minor));
    end;

  function HTTP_NOT_EQUAL_VERSION(constref version: HTTP_VERSION; major, minor : USHORT) : Boolean;
    begin
      HTTP_NOT_EQUAL_VERSION := not (HTTP_EQUAL_VERSION(version, major, minor));
    end;

  function HTTP_GREATER_EQUAL_VERSION(constref version: HTTP_VERSION; major, minor : USHORT) : Boolean;
    begin
      HTTP_GREATER_EQUAL_VERSION := not (HTTP_LESS_VERSION(version, major, minor));
    end;

  function HTTP_LESS_EQUAL_VERSION(constref version: HTTP_VERSION; major, minor : USHORT) : Boolean;
    begin
      HTTP_LESS_EQUAL_VERSION := not (HTTP_GREATER_VERSION(version, major, minor));
    end;

  function HTTPAPI_EQUAL_VERSION(constref version: HTTPAPI_VERSION; major, minor : USHORT) : Boolean;
    begin
      HTTPAPI_EQUAL_VERSION := (version.HttpApiMajorVersion = major) and (version.HttpApiMinorVersion = minor);
    end;

  function HTTPAPI_GREATER_VERSION(constref version: HTTPAPI_VERSION; major, minor: USHORT): Boolean;
    begin
      HTTPAPI_GREATER_VERSION := (version.HttpApiMajorVersion > major) or ((version.HttpApiMajorVersion = major) and (version.HttpApiMinorVersion > minor));
    end;

  function HTTPAPI_LESS_VERSION(constref version: HTTPAPI_VERSION; major, minor: USHORT): Boolean;
    begin
      HTTPAPI_LESS_VERSION := (version.HttpApiMajorVersion < major) or ((version.HttpApiMajorVersion = major) and (version.HttpApiMinorVersion < minor));
    end;

  function HTTPAPI_VERSION_GREATER_OR_EQUAL(constref version: HTTPAPI_VERSION; major, minor : USHORT) : Boolean;
    begin
      HTTPAPI_VERSION_GREATER_OR_EQUAL := not (HTTPAPI_LESS_VERSION(version,major,minor));
    end;

  type
    TUpdateServiceConfigurationFunc = function(ServiceHandle: HANDLE; ConfigId: HTTP_SERVICE_CONFIG_ID; ConfigInfo: PVOID; ConfigInfoLength: ULONG; Overlapped: LPOVERLAPPED): ULONG; WinApi;

  var
    gLibCS: CRITICAL_SECTION;
    gLibHandle: THandle = NilHandle;
    gUpdateServiceConfigurationChecked: Boolean = False;
    gUpdateServiceConfigurationFunc: TUpdateServiceConfigurationFunc = Nil;

  function HttpUpdateServiceConfiguration(ServiceHandle: HANDLE; ConfigId: HTTP_SERVICE_CONFIG_ID; ConfigInfo: PVOID; ConfigInfoLength: ULONG; Overlapped: LPOVERLAPPED): ULONG; WinApi;
    begin
      if not gUpdateServiceConfigurationChecked then begin
        EnterCriticalSection(gLibCS);
        try
          if not gUpdateServiceConfigurationChecked then begin
            gLibHandle := LoadLibrary(External_library);
            if gLibHandle <> NilHandle then
              gUpdateServiceConfigurationFunc := TUpdateServiceConfigurationFunc(GetProcAddress(gLibHandle, 'HttpUpdateServiceConfiguration'))
            else begin
              FreeLibrary(gLibHandle);
              gLibHandle := NilHandle;
            end;
            gUpdateServiceConfigurationChecked := True;
          end;
        finally
          LeaveCriticalSection(gLibCS);
        end;
      end;
      if not Assigned(gUpdateServiceConfigurationFunc) then
        raise EOSError.Create(SysErrorMessage(ERROR_PROC_NOT_FOUND));
      Result := gUpdateServiceConfigurationFunc(ServiceHandle, ConfigId, ConfigInfo, ConfigInfoLength, Overlapped);
    end;

initialization
  InitializeCriticalSection(gLibCS);
finalization
  DoneCriticalSection(gLibCS);
  if gLibHandle <> NilHandle then
    FreeLibrary(gLibHandle);
end.
