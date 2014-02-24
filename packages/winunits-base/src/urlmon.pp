{$mode objfpc}
{$h+}
{ urlmon.h translation. 
  Original urlmon.h copyright:
  Microsoft Windows
  Copyright (c) Microsoft Corporation. All rights reserved.
}

Unit urlmon;

Interface

Uses
  windows, activex;

Const
  liburlmon = 'urlmon.dll';

  SZ_URLCONTEXT: POLEStr   = 'URL Context';
  SZ_ASYNC_CALLEE: POLEStr = 'AsyncCallee';
  MKSYS_URLMONIKER = 6;

  IID_IAuthenticate :  TGUID = '{79eac9d0-baf9-11ce-8c82-00aa004ba90b}';
  IID_IBindHost :  TGUID = '{fc4801a1-2ba9-11cf-a229-00aa003d7352}';
  IID_IBinding :  TGUID = '{79eac9c0-baf9-11ce-8c82-00aa004ba90b}';
  IID_IBindStatusCallback : TGUID = '{79eac9c1-baf9-11ce-8c82-00aa004ba90b}';
  IID_ICodeInstall :  TGUID = '{79eac9d1-baf9-11ce-8c82-00aa004ba90b}';
  IID_IDataFilter : TGUID = '{69d14c80-c18e-11d0-a9ce-006097942311}';
  IID_IEncodingFilterFactory : TGUID = '{70bdde00-c18e-11d0-a9ce-006097942311}';
  IID_IHttpNegotiate : TGUID = '{79eac9d2-baf9-11ce-8c82-00aa004ba90b}';
  IID_IHttpSecurity : TGUID = '{79eac9d7-bafa-11ce-8c82-00aa004ba90b}';
  IID_IInternet :  TGUID = '{79eac9e0-baf9-11ce-8c82-00aa004ba90b}';
  IID_IInternetBindInfo : TGUID = '{79eac9e1-baf9-11ce-8c82-00aa004ba90b}';
  IID_IInternetHostSecurityManager : TGUID = '{3af280b6-cb3f-11d0-891e-00c04fb6bfc4}';
  IID_IInternetPriority : TGUID = '{79eac9eb-baf9-11ce-8c82-00aa004ba90b}';
  IID_IInternetProtocol : TGUID = '{79eac9e4-baf9-11ce-8c82-00aa004ba90b}';
  IID_IInternetProtocolInfo : TGUID = '{79eac9ec-baf9-11ce-8c82-00aa004ba90b}';
  IID_IInternetProtocolRoot : TGUID = '{79eac9e3-baf9-11ce-8c82-00aa004ba90b}';
  IID_IInternetProtocolSink : TGUID = '{79eac9e5-baf9-11ce-8c82-00aa004ba90b}';
  IID_IInternetSecurityManager : TGUID = '{79eac9ee-baf9-11ce-8c82-00aa004ba90b}';
  IID_IInternetSecurityMgrSite : TGUID = '{79eac9ed-baf9-11ce-8c82-00aa004ba90b}';
  IID_IInternetSession : TGUID = '{79eac9e7-baf9-11ce-8c82-00aa004ba90b}';
  IID_IInternetThreadSwitch : TGUID = '{79eac9e8-baf9-11ce-8c82-00aa004ba90b}';
  IID_IInternetZoneManager : TGUID = '{79eac9ef-baf9-11ce-8c82-00aa004ba90b}';
  IID_IOInet : TGUID = '{79eac9e0-baf9-11ce-8c82-00aa004ba90b}';
  IID_IOInetBindInfo : TGUID = '{79eac9e1-baf9-11ce-8c82-00aa004ba90b}';
  IID_IOInetPriority : TGUID = '{79eac9eb-baf9-11ce-8c82-00aa004ba90b}';
  IID_IOInetProtocol : TGUID = '{79eac9e4-baf9-11ce-8c82-00aa004ba90b}';
  IID_IOInetProtocolInfo : TGUID = '{79eac9ec-baf9-11ce-8c82-00aa004ba90b}';
  IID_IOInetProtocolRoot : TGUID = '{79eac9e3-baf9-11ce-8c82-00aa004ba90b}';
  IID_IOInetProtocolSink : TGUID = '{79eac9e5-baf9-11ce-8c82-00aa004ba90b}';
  IID_IOInetSession : TGUID = '{79eac9e7-baf9-11ce-8c82-00aa004ba90b}';
  IID_IOInetThreadSwitch : TGUID = '{79eac9e8-baf9-11ce-8c82-00aa004ba90b}';
  IID_IPersistMoniker : TGUID = '{79eac9c9-baf9-11ce-8c82-00aa004ba90b}';
  IID_ISoftDistExt : TGUID = '{B15B8DC1-C7E1-11d0-8680-00AA00BDCB71}';
  IID_IWindowForBindingUI : TGUID = '{79eac9d5-bafa-11ce-8c82-00aa004ba90b}';
  IID_IWinInetHttpInfo : TGUID = '{79eac9d8-bafa-11ce-8c82-00aa004ba90b}';
  IID_IWinInetInfo : TGUID = '{79eac9d6-bafa-11ce-8c82-00aa004ba90b}';
  SID_IBindHost : TGUID = '{fc4801a1-2ba9-11cf-a229-00aa003d7352}';
  SID_IInternetHostSecurityManager : TGUID = '{3af280b6-cb3f-11d0-891e-00c04fb6bfc4}';
  SID_IInternetSecurityManager : TGUID = '{79eac9ee-baf9-11ce-8c82-00aa004ba90b}';
  SID_SBindHost :  TGUID = '{fc4801a1-2ba9-11cf-a229-00aa003d7352}';

  URLMON_OPTION_USERAGENT         = $10000001;
  URLMON_OPTION_USERAGENT_REFRESH = $10000002;

  CF_NULL                = 0;
  CFSTR_MIME_NULL        = 0;
  CFSTR_MIME_TEXT        = 'text/plain';
  CFSTR_MIME_RICHTEXT    = 'text/richtext';
  CFSTR_MIME_X_BITMAP    = 'image/x-xbitmap';
  CFSTR_MIME_POSTSCRIPT  = 'application/postscript';
  CFSTR_MIME_AIFF        = 'audio/aiff';
  CFSTR_MIME_BASICAUDIO  = 'audio/basic';
  CFSTR_MIME_WAV         = 'audio/wav';
  CFSTR_MIME_X_WAV       = 'audio/x-wav';
  CFSTR_MIME_GIF         = 'image/gif';
  CFSTR_MIME_PJPEG       = 'image/pjpeg';
  CFSTR_MIME_JPEG        = 'image/jpeg';
  CFSTR_MIME_TIFF        = 'image/tiff';
  CFSTR_MIME_X_PNG       = 'image/x-png';
  CFSTR_MIME_BMP         = 'image/bmp';
  CFSTR_MIME_X_ART       = 'image/x-jg';
  CFSTR_MIME_X_EMF       = 'image/x-emf';
  CFSTR_MIME_X_WMF       = 'image/x-wmf';
  CFSTR_MIME_AVI         = 'video/avi';
  CFSTR_MIME_MPEG        = 'video/mpeg';
  CFSTR_MIME_FRACTALS    = 'application/fractals';
  CFSTR_MIME_RAWDATA     = 'application/octet-stream';
  CFSTR_MIME_RAWDATASTRM = 'application/octet-stream';
  CFSTR_MIME_PDF         = 'application/pdf';
  CFSTR_MIME_X_AIFF      = 'audio/x-aiff';
  CFSTR_MIME_X_REALAUDIO = 'audio/x-pn-realaudio';
  CFSTR_MIME_XBM         = 'image/xbm';
  CFSTR_MIME_QUICKTIME   = 'video/quicktime';
  CFSTR_MIME_X_MSVIDEO   = 'video/x-msvideo';
  CFSTR_MIME_X_SGI_MOVIE = 'video/x-sgi-movie';
  CFSTR_MIME_HTML        = 'text/html';

  MK_S_ASYNCHRONOUS = $000401E8;
  S_ASYNCHRONOUS    = MK_S_ASYNCHRONOUS;

  E_PENDING = $8000000A;

  BINDVERB_GET                   = $00000000;
  BINDVERB_POST                  = $00000001;
  BINDVERB_PUT                   = $00000002;
  BINDVERB_CUSTOM                = $00000003;
  BINDINFOF_URLENCODESTGMEDDATA  = $00000001;
  BINDINFOF_URLENCODEDEXTRAINFO  = $00000002;
  BINDF_ASYNCHRONOUS             = $00000001;
  BINDF_ASYNCSTORAGE             = $00000002;
  BINDF_NOPROGRESSIVERENDERING   = $00000004;
  BINDF_OFFLINEOPERATION         = $00000008;
  BINDF_GETNEWESTVERSION         = $00000010;
  BINDF_NOWRITECACHE             = $00000020;
  BINDF_NEEDFILE                 = $00000040;
  BINDF_PULLDATA                 = $00000080;
  BINDF_IGNORESECURITYPROBLEM    = $00000100;
  BINDF_RESYNCHRONIZE            = $00000200;
  BINDF_HYPERLINK                = $00000400;
  BINDF_NO_UI                    = $00000800;
  BINDF_SILENTOPERATION          = $00001000;
  BINDF_PRAGMA_NO_CACHE          = $00002000;
  BINDF_FREE_THREADED            = $00010000;
  BINDF_DIRECT_READ              = $00020000;
  BINDF_FORMS_SUBMIT             = $00040000;
  BINDF_GETFROMCACHE_IF_NET_FAIL = $00080000;
  
  BINDF_DONTUSECACHE             = BINDF_GETNEWESTVERSION;
  BINDF_DONTPUTINCACHE           = BINDF_NOWRITECACHE;
  BINDF_NOCOPYDATA               = BINDF_PULLDATA;

  
  BSCF_FIRSTDATANOTIFICATION        = $00000001;
  BSCF_INTERMEDIATEDATANOTIFICATION = $00000002;
  BSCF_LASTDATANOTIFICATION         = $00000004;
  BSCF_DATAFULLYAVAILABLE           = $00000008;
  BSCF_AVAILABLEDATASIZEUNKNOWN     = $00000010;

  BINDSTATUS_FINDINGRESOURCE            = 1;
  BINDSTATUS_CONNECTING                 = BINDSTATUS_FINDINGRESOURCE+1;
  BINDSTATUS_REDIRECTING                = BINDSTATUS_CONNECTING+1;
  BINDSTATUS_BEGINDOWNLOADDATA          = BINDSTATUS_REDIRECTING+1;
  BINDSTATUS_DOWNLOADINGDATA            = BINDSTATUS_BEGINDOWNLOADDATA+1;
  BINDSTATUS_ENDDOWNLOADDATA            = BINDSTATUS_DOWNLOADINGDATA+1;
  BINDSTATUS_BEGINDOWNLOADCOMPONENTS    = BINDSTATUS_ENDDOWNLOADDATA+1;
  BINDSTATUS_INSTALLINGCOMPONENTS       = BINDSTATUS_BEGINDOWNLOADCOMPONENTS+1;
  BINDSTATUS_ENDDOWNLOADCOMPONENTS      = BINDSTATUS_INSTALLINGCOMPONENTS+1;
  BINDSTATUS_USINGCACHEDCOPY            = BINDSTATUS_ENDDOWNLOADCOMPONENTS+1;
  BINDSTATUS_SENDINGREQUEST             = BINDSTATUS_USINGCACHEDCOPY+1;
  BINDSTATUS_CLASSIDAVAILABLE           = BINDSTATUS_SENDINGREQUEST+1;
  BINDSTATUS_MIMETYPEAVAILABLE          = BINDSTATUS_CLASSIDAVAILABLE+1;
  BINDSTATUS_CACHEFILENAMEAVAILABLE     = BINDSTATUS_MIMETYPEAVAILABLE+1;
  BINDSTATUS_BEGINSYNCOPERATION         = BINDSTATUS_CACHEFILENAMEAVAILABLE+1;
  BINDSTATUS_ENDSYNCOPERATION           = BINDSTATUS_BEGINSYNCOPERATION+1;
  BINDSTATUS_BEGINUPLOADDATA            = BINDSTATUS_ENDSYNCOPERATION+1;
  BINDSTATUS_UPLOADINGDATA              = BINDSTATUS_BEGINUPLOADDATA+1;
  BINDSTATUS_ENDUPLOADDATA              = BINDSTATUS_UPLOADINGDATA+1;
  BINDSTATUS_PROTOCOLCLASSID            = BINDSTATUS_ENDUPLOADDATA+1;
  BINDSTATUS_ENCODING                   = BINDSTATUS_PROTOCOLCLASSID+1;
  BINDSTATUS_VERIFIEDMIMETYPEAVAILABLE  = BINDSTATUS_ENCODING+1;
  BINDSTATUS_CLASSINSTALLLOCATION       = BINDSTATUS_VERIFIEDMIMETYPEAVAILABLE+1;
  BINDSTATUS_DECODING                   = BINDSTATUS_CLASSINSTALLLOCATION+1;
  BINDSTATUS_LOADINGMIMEHANDLER         = BINDSTATUS_DECODING+1;
  BINDSTATUS_CONTENTDISPOSITIONATTACH   = BINDSTATUS_LOADINGMIMEHANDLER+1;
  BINDSTATUS_FILTERREPORTMIMETYPE       = BINDSTATUS_CONTENTDISPOSITIONATTACH+1;
  BINDSTATUS_CLSIDCANINSTANTIATE        = BINDSTATUS_FILTERREPORTMIMETYPE+1;
  BINDSTATUS_IUNKNOWNAVAILABLE          = BINDSTATUS_CLSIDCANINSTANTIATE+1;
  BINDSTATUS_DIRECTBIND                 = BINDSTATUS_IUNKNOWNAVAILABLE+1;
  BINDSTATUS_RAWMIMETYPE                = BINDSTATUS_DIRECTBIND+1;
  BINDSTATUS_PROXYDETECTING             = BINDSTATUS_RAWMIMETYPE+1;
  BINDSTATUS_ACCEPTRANGES               = BINDSTATUS_PROXYDETECTING+1;
  BINDSTATUS_COOKIE_SENT                = BINDSTATUS_ACCEPTRANGES+1;
  BINDSTATUS_COMPACT_POLICY_RECEIVED    = BINDSTATUS_COOKIE_SENT+1;
  BINDSTATUS_COOKIE_SUPPRESSED          = BINDSTATUS_COMPACT_POLICY_RECEIVED+1;
  BINDSTATUS_COOKIE_STATE_UNKNOWN       = BINDSTATUS_COOKIE_SUPPRESSED+1;
  BINDSTATUS_COOKIE_STATE_ACCEPT        = BINDSTATUS_COOKIE_STATE_UNKNOWN+1;
  BINDSTATUS_COOKIE_STATE_REJECT        = BINDSTATUS_COOKIE_STATE_ACCEPT+1;
  BINDSTATUS_COOKIE_STATE_PROMPT        = BINDSTATUS_COOKIE_STATE_REJECT+1;
  BINDSTATUS_COOKIE_STATE_LEASH         = BINDSTATUS_COOKIE_STATE_PROMPT+1;
  BINDSTATUS_COOKIE_STATE_DOWNGRADE     = BINDSTATUS_COOKIE_STATE_LEASH+1;
  BINDSTATUS_POLICY_HREF                = BINDSTATUS_COOKIE_STATE_DOWNGRADE+1;
  BINDSTATUS_P3P_HEADER                 = BINDSTATUS_POLICY_HREF+1;
  BINDSTATUS_SESSION_COOKIE_RECEIVED    = BINDSTATUS_P3P_HEADER+1;
  BINDSTATUS_PERSISTENT_COOKIE_RECEIVED = BINDSTATUS_SESSION_COOKIE_RECEIVED+1;
  BINDSTATUS_SESSION_COOKIES_ALLOWED    = BINDSTATUS_PERSISTENT_COOKIE_RECEIVED+1;

  BINDSTRING_HEADERS          = 1;
  BINDSTRING_ACCEPT_MIMES     = BINDSTRING_HEADERS+1;
  BINDSTRING_EXTRA_URL        = BINDSTRING_ACCEPT_MIMES+1;
  BINDSTRING_LANGUAGE         = BINDSTRING_EXTRA_URL+1;
  BINDSTRING_USERNAME         = BINDSTRING_LANGUAGE+1;
  BINDSTRING_PASSWORD         = BINDSTRING_USERNAME+1;
  BINDSTRING_UA_PIXELS        = BINDSTRING_PASSWORD+1;
  BINDSTRING_UA_COLOR         = BINDSTRING_UA_PIXELS+1;
  BINDSTRING_OS               = BINDSTRING_UA_COLOR+1;
  BINDSTRING_USER_AGENT       = BINDSTRING_OS+1;
  BINDSTRING_ACCEPT_ENCODINGS = BINDSTRING_USER_AGENT+1;
  BINDSTRING_POST_COOKIE      = BINDSTRING_ACCEPT_ENCODINGS+1;
  BINDSTRING_POST_DATA_MIME   = BINDSTRING_POST_COOKIE+1;
  BINDSTRING_URL              = BINDSTRING_POST_DATA_MIME+1;

  INET_E_INVALID_URL                 = $800C0002;
  INET_E_NO_SESSION                  = $800C0003;
  INET_E_CANNOT_CONNECT              = $800C0004;
  INET_E_RESOURCE_NOT_FOUND          = $800C0005;
  INET_E_OBJECT_NOT_FOUND            = $800C0006;
  INET_E_DATA_NOT_AVAILABLE          = $800C0007;
  INET_E_DOWNLOAD_FAILURE            = $800C0008;
  INET_E_AUTHENTICATION_REQUIRED     = $800C0009;
  INET_E_NO_VALID_MEDIA              = $800C000A;
  INET_E_CONNECTION_TIMEOUT          = $800C000B;
  INET_E_INVALID_REQUEST             = $800C000C;
  INET_E_UNKNOWN_PROTOCOL            = $800C000D;
  INET_E_SECURITY_PROBLEM            = $800C000E;
  INET_E_CANNOT_LOAD_DATA            = $800C000F;
  INET_E_CANNOT_INSTANTIATE_OBJECT   = $800C0010;
  INET_E_REDIRECT_FAILED             = $800C0014;
  INET_E_REDIRECT_TO_DIR             = $800C0015;
  INET_E_CANNOT_LOCK_REQUEST         = $800C0016;
  INET_E_ERROR_FIRST                 = $800C0002;
  INET_E_ERROR_LAST                  = INET_E_REDIRECT_TO_DIR;

  CIP_DISK_FULL                            = 0;
  CIP_ACCESS_DENIED                        = CIP_DISK_FULL+1;
  CIP_NEWER_VERSION_EXISTS                 = CIP_ACCESS_DENIED+1;
  CIP_OLDER_VERSION_EXISTS                 = CIP_NEWER_VERSION_EXISTS+1;
  CIP_NAME_CONFLICT                        = CIP_OLDER_VERSION_EXISTS+1;
  CIP_TRUST_VERIFICATION_COMPONENT_MISSING = CIP_NAME_CONFLICT+1;
  CIP_EXE_SELF_REGISTERATION_TIMEOUT       = CIP_TRUST_VERIFICATION_COMPONENT_MISSING+1;
  CIP_UNSAFE_TO_ABORT                      = CIP_EXE_SELF_REGISTERATION_TIMEOUT+1;
  CIP_NEED_REBOOT                          = CIP_UNSAFE_TO_ABORT+1;

  URLOSTRM_USECACHEDCOPY_ONLY = $00000001;   
  URLOSTRM_USECACHEDCOPY      = $00000002;
  URLOSTRM_GETNEWESTVERSION   = $00000003;

  WININETINFO_OPTION_LOCK_HANDLE   = 65534;

  PI_PARSE_URL                = $00000001;
  PI_FILTER_MODE              = $00000002;
  PI_FORCE_ASYNC              = $00000004;
  PI_USE_WORKERTHREAD         = $00000008;
  PI_MIMEVERIFICATION         = $00000010;
  PI_CLSIDLOOKUP              = $00000020;
  PI_DATAPROGRESS             = $00000040;
  PI_SYNCHRONOUS              = $00000080;
  PI_APARTMENTTHREADED        = $00000100;
  PI_CLASSINSTALL             = $00000200;
  PD_FORCE_SWITCH             = $00010000;
  PI_DOCFILECLSIDLOOKUP       = PI_CLSIDLOOKUP;
  OIBDG_APARTMENTTHREADED     = $00000100;

  PARSE_CANONICALIZE    = 1;
  PARSE_FRIENDLY        = PARSE_CANONICALIZE+1;
  PARSE_SECURITY_URL    = PARSE_FRIENDLY+1;
  PARSE_ROOTDOCUMENT    = PARSE_SECURITY_URL+1;
  PARSE_DOCUMENT        = PARSE_ROOTDOCUMENT+1;
  PARSE_ANCHOR          = PARSE_DOCUMENT+1;
  PARSE_ENCODE          = PARSE_ANCHOR+1;
  PARSE_DECODE          = PARSE_ENCODE+1;
  PARSE_PATH_FROM_URL   = PARSE_DECODE+1;
  PARSE_URL_FROM_PATH   = PARSE_PATH_FROM_URL+1;
  PARSE_MIME            = PARSE_URL_FROM_PATH+1;
  PARSE_SERVER          = PARSE_MIME+1;
  PARSE_SCHEMA          = PARSE_SERVER+1;
  PARSE_SITE            = PARSE_SCHEMA+1;
  PARSE_DOMAIN          = PARSE_SITE+1;
  PARSE_LOCATION        = PARSE_DOMAIN+1;
  PARSE_SECURITY_DOMAIN = PARSE_LOCATION+1;

  PSU_DEFAULT           = 1;
  PSU_SECURITY_URL_ONLY = PSU_DEFAULT+1;

  QUERY_EXPIRATION_DATE     = 1;
  QUERY_TIME_OF_LAST_CHANGE = QUERY_EXPIRATION_DATE+1;
  QUERY_CONTENT_ENCODING    = QUERY_TIME_OF_LAST_CHANGE+1;
  QUERY_CONTENT_TYPE        = QUERY_CONTENT_ENCODING+1;
  QUERY_REFRESH             = QUERY_CONTENT_TYPE+1;
  QUERY_RECOMBINE           = QUERY_REFRESH+1;
  QUERY_CAN_NAVIGATE        = QUERY_RECOMBINE+1;
  QUERY_USES_NETWORK        = QUERY_CAN_NAVIGATE+1;
  QUERY_IS_CACHED           = QUERY_USES_NETWORK+1;
  QUERY_IS_INSTALLEDENTRY   = QUERY_IS_CACHED+1;
  QUERY_IS_CACHED_OR_MAPPED = QUERY_IS_INSTALLEDENTRY+1;
  QUERY_USES_CACHE          = QUERY_IS_CACHED_OR_MAPPED+1;

  INET_E_USE_DEFAULT_PROTOCOLHANDLER = $800C0011;
  INET_E_USE_DEFAULT_SETTING         = $800C0012;
  INET_E_DEFAULT_ACTION              = $800C0011;
  INET_E_QUERYOPTION_UNKNOWN         = $800C0013;
  INET_E_REDIRECTING                 = $800C0014;

  PROTOCOLFLAG_NO_PICS_CHECK         = $00000001;

  URLACTION_MIN                                = $00001000;
  URLACTION_DOWNLOAD_MIN                       = $00001000;
  URLACTION_DOWNLOAD_SIGNED_ACTIVEX            = $00001001;
  URLACTION_DOWNLOAD_UNSIGNED_ACTIVEX          = $00001004;
  URLACTION_DOWNLOAD_CURR_MAX                  = $00001004;
  URLACTION_DOWNLOAD_MAX                       = $000011FF;
  URLACTION_ACTIVEX_MIN                        = $00001200;
  URLACTION_ACTIVEX_RUN                        = $00001200;
  URLACTION_ACTIVEX_OVERRIDE_OBJECT_SAFETY     = $00001201;
  URLACTION_ACTIVEX_OVERRIDE_DATA_SAFETY       = $00001202;
  URLACTION_ACTIVEX_OVERRIDE_SCRIPT_SAFETY     = $00001203;
  URLACTION_SCRIPT_OVERRIDE_SAFETY             = $00001401;
  URLACTION_ACTIVEX_CONFIRM_NOOBJECTSAFETY     = $00001204;
  URLACTION_ACTIVEX_TREATASUNTRUSTED           = $00001205;
  URLACTION_ACTIVEX_CURR_MAX                   = $00001205;
  URLACTION_ACTIVEX_MAX                        = $000013FF;
  URLACTION_SCRIPT_MIN                         = $00001400;
  URLACTION_SCRIPT_RUN                         = $00001400;
  URLACTION_SCRIPT_JAVA_USE                    = $00001402;
  URLACTION_SCRIPT_SAFE_ACTIVEX                = $00001405;
  URLACTION_SCRIPT_CURR_MAX                    = $00001405;
  URLACTION_SCRIPT_MAX                         = $000015FF;
  URLACTION_HTML_MIN                           = $00001600;
  URLACTION_HTML_SUBMIT_FORMS                  = $00001601;
  URLACTION_HTML_SUBMIT_FORMS_FROM             = $00001602;
  URLACTION_HTML_SUBMIT_FORMS_TO               = $00001603;
  URLACTION_HTML_FONT_DOWNLOAD                 = $00001604;
  URLACTION_HTML_JAVA_RUN                      = $00001605;
  URLACTION_HTML_CURR_MAX                      = $00001605;
  URLACTION_HTML_MAX                           = $000017FF;
  URLACTION_SHELL_MIN                          = $00001800;
  URLACTION_SHELL_INSTALL_DTITEMS              = $00001800;
  URLACTION_SHELL_MOVE_OR_COPY                 = $00001802;
  URLACTION_SHELL_FILE_DOWNLOAD                = $00001803;
  URLACTION_SHELL_VERB                         = $00001804;
  URLACTION_SHELL_WEBVIEW_VERB                 = $00001805;
  URLACTION_SHELL_CURR_MAX                     = $00001805;
  URLACTION_SHELL_MAX                          = $000019ff;
  URLACTION_NETWORK_MIN                        = $00001A00;
  URLACTION_CREDENTIALS_USE                    = $00001A00;
  URLPOLICY_CREDENTIALS_SILENT_LOGON_OK        = $00000000;
  URLPOLICY_CREDENTIALS_MUST_PROMPT_USER       = $00010000;
  URLPOLICY_CREDENTIALS_CONDITIONAL_PROMPT     = $00020000;
  URLPOLICY_CREDENTIALS_ANONYMOUS_ONLY         = $00030000;
  URLACTION_AUTHENTICATE_CLIENT                = $00001A01;
  URLPOLICY_AUTHENTICATE_CLEARTEXT_OK          = $00000000;
  URLPOLICY_AUTHENTICATE_CHALLENGE_RESPONSE    = $00010000;
  URLPOLICY_AUTHENTICATE_MUTUAL_ONLY           = $00030000;
  URLACTION_NETWORK_CURR_MAX                   = $00001A01;
  URLACTION_NETWORK_MAX                        = $00001BFF;
  URLACTION_JAVA_MIN                           = $00001C00;
  URLACTION_JAVA_PERMISSIONS                   = $00001C00;
  URLPOLICY_JAVA_PROHIBIT                      = $00000000;
  URLPOLICY_JAVA_HIGH                          = $00010000;
  URLPOLICY_JAVA_MEDIUM                        = $00020000;
  URLPOLICY_JAVA_LOW                           = $00030000;
  URLPOLICY_JAVA_CUSTOM                        = $00800000;
  URLACTION_JAVA_CURR_MAX                      = $00001C00;
  URLACTION_JAVA_MAX                           = $00001CFF;
  URLACTION_INFODELIVERY_MIN                       = $00001D00;
  URLACTION_INFODELIVERY_NO_ADDING_CHANNELS        = $00001D00;
  URLACTION_INFODELIVERY_NO_EDITING_CHANNELS       = $00001D01;
  URLACTION_INFODELIVERY_NO_REMOVING_CHANNELS      = $00001D02;
  URLACTION_INFODELIVERY_NO_ADDING_SUBSCRIPTIONS   = $00001D03;
  URLACTION_INFODELIVERY_NO_EDITING_SUBSCRIPTIONS  = $00001D04;
  URLACTION_INFODELIVERY_NO_REMOVING_SUBSCRIPTIONS = $00001D05;
  URLACTION_INFODELIVERY_NO_CHANNEL_LOGGING        = $00001D06;
  URLACTION_INFODELIVERY_CURR_MAX                  = $00001D06;
  URLACTION_INFODELIVERY_MAX                       = $00001Dff;
  URLACTION_CHANNEL_SOFTDIST_MIN                   = $00001E00;
  URLACTION_CHANNEL_SOFTDIST_PERMISSIONS           = $00001E05;
  URLPOLICY_CHANNEL_SOFTDIST_PROHIBIT              = $00010000;
  URLPOLICY_CHANNEL_SOFTDIST_PRECACHE              = $00020000;
  URLPOLICY_CHANNEL_SOFTDIST_AUTOINSTALL           = $00030000;
  URLACTION_CHANNEL_SOFTDIST_MAX                   = $00001EFF;

  URLPOLICY_ALLOW                = $00;
  URLPOLICY_QUERY                = $01;
  URLPOLICY_DISALLOW             = $03;
  URLPOLICY_NOTIFY_ON_ALLOW      = $10;
  URLPOLICY_NOTIFY_ON_DISALLOW   = $20;
  URLPOLICY_LOG_ON_ALLOW         = $40;
  URLPOLICY_LOG_ON_DISALLOW      = $80;

  URLPOLICY_MASK_PERMISSIONS     = $0f;
  
  MAX_SIZE_SECURITY_ID    = 512; 
  
  PUAF_DEFAULT            = $00000000;
  PUAF_NOUI               = $00000001;
  PUAF_ISFILE             = $00000002;
  PUAF_WARN_IF_DENIED     = $00000004;
  PUAF_FORCEUI_FOREGROUND = $00000008;
  PUAF_CHECK_TIFS         = $00000010;
  
  SZM_CREATE              = $00000000;
  SZM_DELETE              = $00000001;

  URLZONE_PREDEFINED_MIN = 0;
  URLZONE_LOCAL_MACHINE  = 0;
  URLZONE_INTRANET       = URLZONE_LOCAL_MACHINE+1;
  URLZONE_TRUSTED        = URLZONE_INTRANET+1;
  URLZONE_INTERNET       = URLZONE_TRUSTED+1;
  URLZONE_UNTRUSTED      = URLZONE_INTERNET+1;
  URLZONE_PREDEFINED_MAX = 999;
  URLZONE_USER_MIN       = 1000;
  URLZONE_USER_MAX       = 10000;

  URLZONEREG_DEFAULT = 0;
  URLZONEREG_HKLM    = URLZONEREG_DEFAULT+1;
  URLZONEREG_HKCU    = URLZONEREG_HKLM+1;

  URLTEMPLATE_CUSTOM         = $00000000;
  URLTEMPLATE_PREDEFINED_MIN = $00010000;
  URLTEMPLATE_LOW            = $00010000;
  URLTEMPLATE_MEDIUM         = $00011000;
  URLTEMPLATE_HIGH           = $00012000;
  URLTEMPLATE_PREDEFINED_MAX = $00020000;

  MAX_ZONE_PATH              = 260;
  MAX_ZONE_DESCRIPTION       = 200;

  ZAFLAGS_CUSTOM_EDIT            = $00000001;
  ZAFLAGS_ADD_SITES              = $00000002;
  ZAFLAGS_REQUIRE_VERIFICATION   = $00000004;
  ZAFLAGS_INCLUDE_PROXY_OVERRIDE = $00000008;
  ZAFLAGS_INCLUDE_INTRANET_SITES = $00000010;
  ZAFLAGS_NO_UI                  = $00000020;
  ZAFLAGS_SUPPORTS_VERIFICATION  = $00000040;
  ZAFLAGS_UNC_AS_INTRANET        = $00000080;

  SOFTDIST_FLAG_USAGE_EMAIL         = $00000001;
  SOFTDIST_FLAG_USAGE_PRECACHE      = $00000002;
  SOFTDIST_FLAG_USAGE_AUTOINSTALL   = $00000004;
  SOFTDIST_FLAG_DELETE_SUBSCRIPTION = $00000008;

  SOFTDIST_ADSTATE_NONE             = $00000000;
  SOFTDIST_ADSTATE_AVAILABLE        = $00000001;
  SOFTDIST_ADSTATE_DOWNLOADED       = $00000002;
  SOFTDIST_ADSTATE_INSTALLED        = $00000003;
  
Type
  PHWnd = ^HWND;
  PIunknown = ^IUnknown;
  PIEnumString = ^IENumString;
  PStgMedium = ^TStgMedium;
  
  TUrlZoneReg = Cardinal;
  TParseAction = Cardinal;
  TQueryOption = Cardinal;
  TPSUAction = Cardinal;
  
  PLPCWSTR = ^LPCWSTR;
  PPOLEStr = ^POLEStr;
  PIBindCtx = ^IBindCtx;
  PIEnumFormatEtc = ^IEnumFormatEtc;
  PIstream = ^IStream;
  
  // Forward declarations
  IInternetProtocolSink = Interface;
  IBinding = interface; 
  IInternetProtocol = interface;
  
  TBINDINFO = packed record
    cbSize : ULONG;
    szExtraInfo : LPWSTR;
    stgmedData : TStgMedium;
    grfBindInfoF : DWORD;
    dwBindVerb : DWORD;
    szCustomVerb : LPWSTR;
    cbstgmedData : DWORD;
    dwOptions : DWORD;
    dwOptionsFlags : DWORD;
    dwCodePage : DWORD;
    securityAttributes : SECURITY_ATTRIBUTES;
    iid : TGUID;
    pUnk : IUnknown;
    dwReserved : DWORD;
  end;
  BINDINFO = TBINDINFO;
  PBINDINFO = ^TBINDINFO;

  TREMSECURITY_ATTRIBUTES = packed record
    nLength : DWORD;
    lpSecurityDescriptor : DWORD;
    bInheritHandle : BOOL;
  end;
  REMSECURITY_ATTRIBUTES = TREMSECURITY_ATTRIBUTES;
  PREMSECURITY_ATTRIBUTES = ^TREMSECURITY_ATTRIBUTES;
  
  TRemBINDINFO = packed record
    cbSize : ULONG;
    szExtraInfo : LPWSTR;
    grfBindInfoF : DWORD;
    dwBindVerb : DWORD;
    szCustomVerb : LPWSTR;
    cbstgmedData : DWORD;
    dwOptions : DWORD;
    dwOptionsFlags : DWORD;
    dwCodePage : DWORD;
    securityAttributes : TREMSECURITY_ATTRIBUTES;
    iid : TGUID;
    pUnk : IUnknown;
    dwReserved : DWORD;
  end;
  PRemBINDINFO = ^TRemBINDINFO;
  RemBINDINFO = TRemBINDINFO;
  
  TRemFORMATETC = packed record
    cfFormat : DWORD;
    ptd : DWORD;
    dwAspect : DWORD;
    lindex : Longint;
    tymed : DWORD;
  end;
  RemFORMATETC = TRemFORMATETC;
  PRemFORMATETC = ^TRemFORMATETC;

  TPROTOCOLDATA = packed record
    grfFlags : DWORD;
    dwState : DWORD;
    pData : Pointer;
    cbData : ULONG;
  end;
  PROTOCOLDATA = TPROTOCOLDATA;
  PPROTOCOLDATA = ^PROTOCOLDATA;
  
  TZONEATTRIBUTES = packed record
    cbSize : ULONG;
    szDisplayName : array [0..259] of WideChar;
    szDescription : array [0..199] of WideChar;
    szIconPath : array [0..259] of WideChar;
    dwTemplateMinLevel : DWORD;
    dwTemplateRecommended : DWORD;
    dwTemplateCurrentLevel : DWORD;
    dwFlags : DWORD;
  end;
  ZONEATTRIBUTES = TZONEATTRIBUTES;
  PZONEATTRIBUTES = ^TZONEATTRIBUTES;

  TCODEBASEHOLD = packed record
    cbSize : ULONG;
    szDistUnit : LPWSTR;
    szCodeBase : LPWSTR;
    dwVersionMS : DWORD;
    dwVersionLS : DWORD;
    dwStyle : DWORD;
  end;
  CODEBASEHOLD = TCODEBASEHOLD;
  PCODEBASEHOLD = ^TCODEBASEHOLD;

  TSOFTDISTINFO = packed record
    cbSize : ULONG;
    dwFlags : DWORD;
    dwAdState : DWORD;
    szTitle : LPWSTR;
    szAbstract : LPWSTR;
    szHREF : LPWSTR;
    dwInstalledVersionMS : DWORD;
    dwInstalledVersionLS : DWORD;
    dwUpdateVersionMS : DWORD;
    dwUpdateVersionLS : DWORD;
    dwAdvertisedVersionMS : DWORD;
    dwAdvertisedVersionLS : DWORD;
    dwReserved : DWORD;
  end;
  SOFTDISTINFO = TSOFTDISTINFO;
  PSOFTDISTINFO = ^TSOFTDISTINFO;
  
  TPROTOCOLFILTERDATA = packed record
    cbSize : DWORD;
    ProtocolSink : IInternetProtocolSink;
    Protocol : IInternetProtocol;
    Unk : IUnknown;
    dwFilterFlags : DWORD;
  end;
  PROTOCOLFILTERDATA = TPROTOCOLFILTERDATA;
  PPROTOCOLFILTERDATA = ^TPROTOCOLFILTERDATA;
  
  TDATAINFO = packed record
    ulTotalSize : ULONG;
    ulavrPacketSize : ULONG;
    ulConnectSpeed : ULONG;
    ulProcessorSpeed : ULONG;
  end;
  DATAINFO = TDATAINFO;
  PDATAINFO = ^TDATAINFO;
  
  THIT_LOGGING_INFO = packed record
    dwStructSize : DWORD;
    lpszLoggedUrlName : LPSTR;
    StartTime : TSystemTime;
    EndTime : TSystemTime;
    lpszExtendedInfo : LPSTR;
  end;
  HIT_LOGGING_INFO = THIT_LOGGING_INFO;
  PHIT_LOGGING_INFO = ^THIT_LOGGING_INFO;


  IPersistMonikerRaw = interface['{79eac9c9-baf9-11ce-8c82-00aa004ba90b}']
    Function GetClassID(ClassID : PCLSID) : HResult; stdcall;
    Function IsDirty : HResult; stdcall;
    Function Load(fFullyAvailable : BOOL; pimkName : IMoniker; pibc : IBindCtx; grfMode : DWORD) : HResult; stdcall;
    Function Save(pimkName : IMoniker; pbc : IBindCtx; fRemember : BOOL) : HResult; stdcall;
    Function SaveCompleted(pimkName : IMoniker; pibc : IBindCtx) : HResult; stdcall;
    Function GetCurMoniker(ppimkName : IMoniker) : HResult; stdcall;
  end;
  PIPersistMonikerRaw = ^IPersistMonikerRaw;

  IPersistMoniker = interface['{79eac9c9-baf9-11ce-8c82-00aa004ba90b}']
    Function GetClassID(out ClassID : TCLSID) : HResult; stdcall;
    Function IsDirty : HResult; stdcall;
    Function Load(fFullyAvailable : BOOL; pimkName : IMoniker; pibc : IBindCtx; grfMode : DWORD) : HResult; stdcall;
    Function Save(pimkName : IMoniker; pbc : IBindCtx; fRemember : BOOL) : HResult; stdcall;
    Function SaveCompleted(pimkName : IMoniker; pibc : IBindCtx) : HResult; stdcall;
    Function GetCurMoniker(ppimkName : IMoniker) : HResult; stdcall;
  end;
  PIPersistMoniker = ^IPersistMoniker;


  IBindingRaw = interface ['{79eac9c0-baf9-11ce-8c82-00aa004ba90b}']
    Function Abort : HResult; stdcall;
    Function Suspend : HResult; stdcall;
    Function Resume : HResult; stdcall;
    Function SetPriority(nPriority : Longint) : HResult; stdcall;
    Function GetPriority(nPriority : PLongint) : HResult; stdcall;
    Function GetBindResult(clsidProtocol : PCLSID; dwResult : PDWORD; szResult : PPOLEStr; dwReserved : DWORD) : HResult; stdcall;
  end;
  PIBindingRaw = ^IBindingRaw;
  
  IBinding = interface ['{79eac9c0-baf9-11ce-8c82-00aa004ba90b}']
    Function Abort : HResult; stdcall;
    Function Suspend : HResult; stdcall;
    Function Resume : HResult; stdcall;
    Function SetPriority(nPriority : Longint) : HResult; stdcall;
    Function GetPriority(out nPriority : Longint) : HResult; stdcall;
    Function GetBindResult(out clsidProtocol : TCLSID; out dwResult : DWORD; out szResult : POLEStr; dwReserved : DWORD) : HResult; stdcall;
  end;
  PIBinding = ^IBinding;
  IBindProtocolRaw = interface['{79eac9cd-baf9-11ce-8c82-00aa004ba90b}']
    Function CreateBinding(szUrl : LPCWSTR; pbc : IBindCtx; ppb : PIBindingRaw) : HResult; stdcall;
  end;
  PIBindProtocolRaw = ^IBindProtocolRaw;
  
  IBindProtocol = interface['{79eac9cd-baf9-11ce-8c82-00aa004ba90b}']
    Function CreateBinding(szUrl : LPCWSTR; pbc : IBindCtx; out ppb : IBinding) : HResult; stdcall;
  end;
  PIBindProtocol = ^IBindProtocol;
  
  IBindStatusCallbackRaw = interface['{79eac9c1-baf9-11ce-8c82-00aa004ba90b}']
    Function OnStartBinding(dwReserved : DWORD; pib : IBindingRaw) : HResult; stdcall;
    Function GetPriority(nPriority  : Pointer) : HResult; stdcall;
    Function OnLowResource(reserved : DWORD) : HResult; stdcall;
    Function OnProgress(ulProgress, ulProgressMax, ulStatusCode : ULONG; szStatusText : LPCWSTR) : HResult; stdcall;
    Function OnStopBinding(hresult : HResult; szError : LPCWSTR) : HResult; stdcall;
    Function GetBindInfo(grfBINDF : PDWORD; bindinfo : PBINDINFO) : HResult; stdcall;
    Function OnDataAvailable(grfBSCF : DWORD; dwSize : DWORD; formatetc : PFormatEtc; stgmed : PStgMedium) : HResult; stdcall;
    Function OnObjectAvailable(const iid : TGUID; punk : IUnknown) : HResult; stdcall;
  end;
  PIBindStatusCallbackRaw = ^IBindStatusCallbackRaw;

  IBindStatusCallback = interface['{79eac9c1-baf9-11ce-8c82-00aa004ba90b}']
    Function OnStartBinding(dwReserved : DWORD; pib : IBinding) : HResult; stdcall;
    Function GetPriority(out nPriority) : HResult; stdcall;
    Function OnLowResource(reserved : DWORD) : HResult; stdcall;
    Function OnProgress(ulProgress, ulProgressMax, ulStatusCode : ULONG; szStatusText : LPCWSTR) : HResult; stdcall;
    Function OnStopBinding(hresult : HResult; szError : LPCWSTR) : HResult; stdcall;
    Function GetBindInfo(out grfBINDF : DWORD; var bindinfo : TBindInfo) : HResult; stdcall;
    Function OnDataAvailable(grfBSCF : DWORD; dwSize : DWORD; formatetc : PFormatEtc; stgmed : PStgMedium) : HResult; stdcall;
    Function OnObjectAvailable(const iid : TGUID; punk : IUnknown) : HResult; stdcall;
  end;
  PIBindStatusCallback = ^IBindStatusCallback;

  IAuthenticateRaw = interface['{79eac9d0-baf9-11ce-8c82-00aa004ba90b}']
    Function Authenticate(hwnd : PHWnd; szUserName, szPassWord : PLPWSTR) : HResult; stdcall;
  end;
  PIAuthenticateRaw = ^IAuthenticateRaw;

  IAuthenticate = interface['{79eac9d0-baf9-11ce-8c82-00aa004ba90b}']
    Function Authenticate(var hwnd : HWnd; var szUserName, szPassWord : LPWSTR) : HResult; stdcall;
  end;
  PIAuthenticate = ^IAuthenticate;

  IHttpNegotiateRaw = interface['{79eac9d2-baf9-11ce-8c82-00aa004ba90b}']
    Function BeginningTransaction(szURL, szHeaders : LPCWSTR; dwReserved : DWORD; szAdditionalHeaders : PLPWSTR) : HResult; stdcall;
    Function OnResponse(dwResponseCode : DWORD; szResponseHeaders, szRequestHeaders : LPCWSTR; szAdditionalRequestHeaders : PLPWSTR) : HResult; stdcall;
  end;
  PIHttpNegotiateRaw = ^IHttpNegotiateRaw;

  IHttpNegotiate = interface['{79eac9d2-baf9-11ce-8c82-00aa004ba90b}']
    Function BeginningTransaction(szURL, szHeaders : LPCWSTR; dwReserved : DWORD; out szAdditionalHeaders : LPWSTR) : HResult; stdcall;
    Function OnResponse(dwResponseCode : DWORD; szResponseHeaders, szRequestHeaders : LPCWSTR; out szAdditionalRequestHeaders : LPWSTR) : HResult; stdcall;
  end;
  PIHttpNegotiate = ^IHttpNegotiate;

  IWindowForBindingUIRaw = interface['{79eac9d5-bafa-11ce-8c82-00aa004ba90b}']
    Function GetWindow(const guidReason : TGUID; hwnd  : Pointer) : HResult; stdcall;
  end;
  PIWindowForBindingUIRaw = ^IWindowForBindingUIRaw;

  IWindowForBindingUI = interface['{79eac9d5-bafa-11ce-8c82-00aa004ba90b}']
    Function GetWindow(const guidReason : TGUID; out hwnd) : HResult; stdcall;
  end;
  PIWindowForBindingUI = ^IWindowForBindingUI;

  ICodeInstallRaw = interface(IWindowForBindingUI)['{79eac9d1-baf9-11ce-8c82-00aa004ba90b}']
    Function OnCodeInstallProblem(ulStatusCode : ULONG; szDestination, szSource : LPCWSTR; dwReserved : DWORD) : HResult; stdcall;
  end;
  ICodeInstall = ICodeInstallRaw;
  PICodeInstall = ^ICodeInstall;

  IWinInetInfoRaw = interface['{79eac9d6-bafa-11ce-8c82-00aa004ba90b}']
    Function QueryOption(dwOption : DWORD; Buffer : Pointer; cbBuf : PDWORD) : HResult; stdcall;
  end;
  PIWinInetInfoRaw = ^IWinInetInfoRaw;
  
  IWinInetInfo = interface['{79eac9d6-bafa-11ce-8c82-00aa004ba90b}']
    Function QueryOption(dwOption : DWORD; Buffer : Pointer; var cbBuf : DWORD) : HResult; stdcall;
  end;
  PIWinInetInfo = ^IWinInetInfo;

  IHttpSecurityRaw = interface(IWindowForBindingUIRaw)['{79eac9d7-bafa-11ce-8c82-00aa004ba90b}']
    Function OnSecurityProblem(dwProblem : DWORD) : HResult; stdcall;
  end;
  PIHttpSecurityRaw = ^IHttpSecurityRaw;
  IHttpSecurity = IHttpSecurityRaw;
  PIHttpSecurity = PIHttpSecurityRaw;
  
  IWinInetHttpInfoRaw = interface(IWinInetInfoRaw)['{79eac9d8-bafa-11ce-8c82-00aa004ba90b}']
    Function QueryInfo(dwOption : DWORD; Buffer : Pointer; cbBuf, dwFlags, dwReserved : PDWORD) : HResult; stdcall;
  end;
  PIWinInetHttpInfoRaw = ^IWinInetHttpInfoRaw;

  IWinInetHttpInfo = interface(IWinInetInfoRaw)['{79eac9d8-bafa-11ce-8c82-00aa004ba90b}']
    Function QueryInfo(dwOption : DWORD; Buffer : Pointer; var cbBuf, dwFlags, dwReserved : DWORD) : HResult; stdcall;
  end;
  PIWinInetHttpInfo = ^IWinInetHttpInfo;

  IBindHostRaw = interface['{fc4801a1-2ba9-11cf-a229-00aa003d7352}']
    Function CreateMoniker(szName : POLEStr; BC : IBindCtx; mk : PIMoniker; dwReserved : DWORD) : HResult; stdcall;
    Function MonikerBindToStorage(Mk : IMoniker; BC : IBindCtx; BSC : IBindStatusCallback; const iid : TGUID; pvObj : PPointer) : HResult; stdcall;
    Function MonikerBindToObject(Mk : IMoniker; BC : IBindCtx; BSC : IBindStatusCallback; const iid : TGUID; pvObj : PPointer) : HResult; stdcall;
  end;
  PIBindHostRaw = ^IBindHostRaw;

  IBindHost = interface['{fc4801a1-2ba9-11cf-a229-00aa003d7352}']
    Function CreateMoniker(szName : POLEStr; BC : IBindCtx; out mk : IMoniker; dwReserved : DWORD) : HResult; stdcall;
    Function MonikerBindToStorage(Mk : IMoniker; BC : IBindCtx; BSC : IBindStatusCallback; const iid : TGUID; out pvObj : Pointer) : HResult; stdcall;
    Function MonikerBindToObject(Mk : IMoniker; BC : IBindCtx; BSC : IBindStatusCallback; const iid : TGUID; out pvObj : Pointer) : HResult; stdcall;
  end;
  PIBindHost = ^IBindHost;

  IInternetRaw = interface['{79eac9e0-baf9-11ce-8c82-00aa004ba90b}']
  end;
  PIInternetRaw = ^IInternetRaw;
  IInternet = IInternetRaw;
  PIInternet = PIInternetRaw;

  IInternetBindInfoRaw = interface ['{79eac9e1-baf9-11ce-8c82-00aa004ba90b}']
    Function GetBindInfo(grfBINDF : PDWORD; bindinfo : PBindInfo) : HResult; stdcall;
    Function GetBindString(ulStringType : ULONG; wzStr : POLEStr; cEl : ULONG; cElFetched : PULONG) : HResult; stdcall;
  end;
  PIInternetBindInfoRaw = ^IInternetBindInfoRaw;

  IInternetBindInfo = interface ['{79eac9e1-baf9-11ce-8c82-00aa004ba90b}']
    Function GetBindInfo(out grfBINDF : DWORD; var bindinfo : TBindInfo) : HResult; stdcall; 
	Function GetBindString(ulStringType : ULONG; wzStr : POLEStr; cEl : ULONG; var cElFetched : ULONG) : HResult; stdcall;
  end;
  PIInternetBindInfo = ^IInternetBindInfo;
  
  IInternetProtocolRootRaw = interface ['{79eac9e3-baf9-11ce-8c82-00aa004ba90b}']
    Function Start(szUrl : LPCWSTR; OIProtSink : IInternetProtocolSink; OIBindInfo : IInternetBindInfo; grfPI, dwReserved : DWORD) : HResult; stdcall;
    Function Continue(const ProtocolData : TProtocolData) : HResult; stdcall;
    Function Abort(hrReason : HResult; dwOptions : DWORD) : HResult; stdcall;
    Function Terminate(dwOptions : DWORD) : HResult; stdcall;
    Function Suspend : HResult; stdcall;
    Function Resume : HResult; stdcall;
  end;
  PIInternetProtocolRootRaw = ^IInternetProtocolRootRaw;
  IInternetProtocolRoot = IInternetProtocolRootRaw;
  PIInternetProtocolRoot = PIInternetProtocolRootRaw;

  IInternetProtocolRaw = interface(IInternetProtocolRootRaw)['{79eac9e4-baf9-11ce-8c82-00aa004ba90b}']
    Function Read(pv : Pointer; cb : ULONG; cbRead : PULONG) : HResult; stdcall;
    Function Seek(dlibMove : LARGE_INTEGER; dwOrigin : DWORD; libNewPosition : PULARGE_INTEGER) : HResult; stdcall;
    Function LockRequest(dwOptions : DWORD) : HResult; stdcall;
    Function UnlockRequest : HResult; stdcall;
  end;
  PIInternetProtocolRaw = ^IInternetProtocolRaw;

  IInternetProtocol = interface(IInternetProtocolRoot)['{79eac9e4-baf9-11ce-8c82-00aa004ba90b}']
    Function Read(pv : Pointer; cb : ULONG; out cbRead : ULONG) : HResult; stdcall;
    Function Seek(dlibMove : LARGE_INTEGER; dwOrigin : DWORD; out libNewPosition : ULARGE_INTEGER) : HResult; stdcall;
    Function LockRequest(dwOptions : DWORD) : HResult; stdcall;
    Function UnlockRequest : HResult; stdcall;
  end;
  PIInternetProtocol = ^IInternetProtocol;

  IInternetProtocolSink = interface['{79eac9e5-baf9-11ce-8c82-00aa004ba90b}']
    Function Switch(const ProtocolData : TProtocolData) : HResult; stdcall;
    Function ReportProgress(ulStatusCode : ULONG; szStatusText : LPCWSTR) : HResult; stdcall;
    Function ReportData(grfBSCF : DWORD; ulProgress, ulProgressMax : ULONG) : HResult; stdcall;
    Function ReportResult(hrResult : HResult; dwError : DWORD; szResult : LPCWSTR) : HResult; stdcall;
  end;
  PIInternetProtocolSink = ^IInternetProtocolSink;
  IInternetProtocolSinkRaw = IInternetProtocolSink; // Because of the forward declaration, we must do it like this.
  PIInternetProtocolSinkRaw = PIInternetProtocolSink;

  IInternetSessionRaw = interface['{79eac9e7-baf9-11ce-8c82-00aa004ba90b}']
    Function RegisterNameSpace(CF : IClassFactory; const clsid : TCLSID; pwzProtocol : LPCWSTR; cPatterns : ULONG; const pwzPatterns : PLPCWSTR; dwReserved : DWORD) : HResult; stdcall;
    Function UnregisterNameSpace(CF : IClassFactory; pszProtocol : LPCWSTR) : HResult; stdcall;
    Function RegisterMimeFilter(CF : IClassFactory; const rclsid : TCLSID; pwzType : LPCWSTR) : HResult; stdcall;
    Function UnregisterMimeFilter(CF : IClassFactory; pwzType : LPCWSTR) : HResult; stdcall;
    Function CreateBinding(BC : IBindCtx; szUrl : LPCWSTR; UnkOuter : IUnknown; Unk : PIUnknown; OINetProt : PIInternetProtocol; dwOption : DWORD) : HResult; stdcall;
    Function SetSessionOption(dwOption : DWORD; pBuffer : Pointer; dwBufferLength : DWORD; dwReserved : DWORD) : HResult; stdcall;
    Function GetSessionOption(dwOption : DWORD; pBuffer : Pointer; dwBufferLength : PDWORD; dwReserved : DWORD) : HResult; stdcall;
  end;
  PIInternetSessionRaw = ^IInternetSessionRaw;

  IInternetSession = interface['{79eac9e7-baf9-11ce-8c82-00aa004ba90b}']
    Function RegisterNameSpace(CF : IClassFactory; const clsid : TCLSID; pwzProtocol : LPCWSTR; cPatterns : ULONG; const pwzPatterns : PLPCWSTR; dwReserved : DWORD) : HResult; stdcall;
    Function UnregisterNameSpace(CF : IClassFactory; pszProtocol : LPCWSTR) : HResult; stdcall;
    Function RegisterMimeFilter(CF : IClassFactory; const rclsid : TCLSID; pwzType : LPCWSTR) : HResult; stdcall;
    Function UnregisterMimeFilter(CF : IClassFactory; pwzType : LPCWSTR) : HResult; stdcall;
    Function CreateBinding(BC : IBindCtx; szUrl : LPCWSTR; UnkOuter : IUnknown; out Unk : IUnknown; out OINetProt : IInternetProtocol; dwOption : DWORD) : HResult; stdcall;
    Function SetSessionOption(dwOption : DWORD; pBuffer : Pointer; dwBufferLength : DWORD; dwReserved : DWORD) : HResult; stdcall;
    Function GetSessionOption(dwOption : DWORD; pBuffer : Pointer; var dwBufferLength : DWORD; dwReserved : DWORD) : HResult; stdcall;
  end;
  PIInternetSession = ^IInternetSession;

  IInternetThreadSwitchRaw = interface ['{79eac9e8-baf9-11ce-8c82-00aa004ba90b}']
    Function Prepare : HResult; stdcall;
    Function Continue : HResult; stdcall;
  end;
  PIInternetThreadSwitchRaw = ^IInternetThreadSwitchRaw;
  IInternetThreadSwitch = IInternetThreadSwitchRaw;
  PIInternetThreadSwitch = PIInternetThreadSwitchRaw;

  IInternetPriorityRaw = interface ['{79eac9eb-baf9-11ce-8c82-00aa004ba90b}']
    Function SetPriority(nPriority : Longint) : HResult; stdcall;
    Function GetPriority(nPriority : PLongint) : HResult; stdcall;
  end;
  PIInternetPriorityRaw = ^IInternetPriorityRaw;
  
  IInternetPriority = interface ['{79eac9eb-baf9-11ce-8c82-00aa004ba90b}']
    Function SetPriority(nPriority : Longint) : HResult; stdcall;
    Function GetPriority(out nPriority : Longint) : HResult; stdcall;
  end;
  PIInternetPriority = ^IInternetPriority;
  
  IInternetProtocolInfoRaw = interface ['{79eac9ec-baf9-11ce-8c82-00aa004ba90b}']
    Function ParseUrl(pwzUrl : LPCWSTR; ParseAction : TParseAction; dwParseFlags : DWORD; pwzResult : LPWSTR; cchResult : DWORD; pcchResult : DWORD; dwReserved : DWORD) : HResult; stdcall;
    Function CombineUrl(pwzBaseUrl, pwzRelativeUrl : LPCWSTR; dwCombineFlags : DWORD; pwzResult : LPWSTR; cchResult : DWORD; pcchResult : PDWORD; dwReserved : DWORD) : HResult; stdcall;
    Function CompareUrl(pwzUrl1, pwzUrl2 : LPCWSTR; dwCompareFlags : DWORD) : HResult; stdcall;
    Function QueryInfo(pwzUrl : LPCWSTR; QueryOption : TQueryOption; dwQueryFlags : DWORD; pBuffer : Pointer; cbBuffer : DWORD; cbBuf : PDWORD; dwReserved : DWORD) : HResult; stdcall;
  end;
  PIInternetProtocolInfoRaw = ^IInternetProtocolInfoRaw;

  IInternetProtocolInfo = interface ['{79eac9ec-baf9-11ce-8c82-00aa004ba90b}']
    Function ParseUrl(pwzUrl : LPCWSTR; ParseAction : TParseAction; dwParseFlags : DWORD; pwzResult : LPWSTR; cchResult : DWORD; pcchResult : DWORD; dwReserved : DWORD) : HResult; stdcall;
    Function CombineUrl(pwzBaseUrl, pwzRelativeUrl : LPCWSTR; dwCombineFlags : DWORD; pwzResult : LPWSTR; cchResult : DWORD; out pcchResult : DWORD; dwReserved : DWORD) : HResult; stdcall;
    Function CompareUrl(pwzUrl1, pwzUrl2 : LPCWSTR; dwCompareFlags : DWORD) : HResult; stdcall;
    Function QueryInfo(pwzUrl : LPCWSTR; QueryOption : TQueryOption; dwQueryFlags : DWORD; pBuffer : Pointer; cbBuffer : DWORD; var cbBuf : DWORD; dwReserved : DWORD) : HResult; stdcall;
  end;
  PIInternetProtocolInfo = ^IInternetProtocolInfo;

  IInternetSecurityMgrSiteRaw = interface ['{79eac9ed-baf9-11ce-8c82-00aa004ba90b}']
    Function GetWindow(hwnd : PHWnd) : HResult; stdcall;
    Function EnableModeless(fEnable : BOOL) : HResult; stdcall;
  end;
  PIInternetSecurityMgrSiteRaw = ^IInternetSecurityMgrSiteRaw;

  IInternetSecurityMgrSite = interface ['{79eac9ed-baf9-11ce-8c82-00aa004ba90b}']
    Function GetWindow(out hwnd : HWnd) : HResult; stdcall;
    Function EnableModeless(fEnable : BOOL) : HResult; stdcall;
  end;
  PIInternetSecurityMgrSite = ^IInternetSecurityMgrSite;

  IInternetSecurityManagerRaw = interface ['{79eac9ee-baf9-11ce-8c82-00aa004ba90b}']
    Function SetSecuritySite(Site : IInternetSecurityMgrSiteRaw) : HResult; stdcall;
    Function GetSecuritySite(Site : PIInternetSecurityMgrSiteRaw) : HResult; stdcall;
    Function MapUrlToZone(pwszUrl : LPCWSTR; dwZone : PDWORD; dwFlags : DWORD) : HResult; stdcall;
    Function GetSecurityId(pwszUrl : LPCWSTR; pbSecurityId : Pointer; cbSecurityId : PDWORD; dwReserved : DWORD) : HResult; stdcall;
    Function ProcessUrlAction(pwszUrl : LPCWSTR; dwAction : DWORD; pPolicy : Pointer; cbPolicy : DWORD; pContext : Pointer; cbContext : DWORD; dwFlags, dwReserved : DWORD) : HResult; stdcall;
    Function QueryCustomPolicy(pwszUrl : LPCWSTR; const guidKey : TGUID; out pPolicy : Pointer; cbPolicy : PDWORD; pContext : Pointer; cbContext : DWORD; dwReserved : DWORD) : HResult; stdcall;
    Function SetZoneMapping(dwZone : DWORD; lpszPattern : LPCWSTR; dwFlags : DWORD) : HResult; stdcall;
    Function GetZoneMappings(dwZone : DWORD; enumString : PIEnumString; dwFlags : DWORD) : HResult; stdcall;
  end;
  PIInternetSecurityManagerRaw = ^IInternetSecurityManagerRaw;
  
  IInternetSecurityManager = interface ['{79eac9ee-baf9-11ce-8c82-00aa004ba90b}']
    Function SetSecuritySite(Site : IInternetSecurityMgrSite) : HResult; stdcall;
    Function GetSecuritySite(out Site : IInternetSecurityMgrSite) : HResult; stdcall;
    Function MapUrlToZone(pwszUrl : LPCWSTR; out dwZone : DWORD; dwFlags : DWORD) : HResult; stdcall;
    Function GetSecurityId(pwszUrl : LPCWSTR; pbSecurityId : Pointer; var cbSecurityId : DWORD; dwReserved : DWORD) : HResult; stdcall;
    Function ProcessUrlAction(pwszUrl : LPCWSTR; dwAction : DWORD; pPolicy : Pointer; cbPolicy : DWORD; pContext : Pointer; cbContext : DWORD; dwFlags, dwReserved : DWORD) : HResult; stdcall;
    Function QueryCustomPolicy(pwszUrl : LPCWSTR; const guidKey : TGUID; out pPolicy : Pointer; out cbPolicy : DWORD; pContext : Pointer; cbContext : DWORD; dwReserved : DWORD) : HResult; stdcall;
    Function SetZoneMapping(dwZone : DWORD; lpszPattern : LPCWSTR; dwFlags : DWORD) : HResult; stdcall;
    Function GetZoneMappings(dwZone : DWORD; out enumString : IEnumString; dwFlags : DWORD) : HResult; stdcall;
  end;
  PIInternetSecurityManager = ^IInternetSecurityManager;
  
  IInternetHostSecurityManagerRaw = interface ['{3af280b6-cb3f-11d0-891e-00c04fb6bfc4}']
    Function GetSecurityId(pbSecurityId : Pointer; cbSecurityId : PDWORD; dwReserved : DWORD) : HResult; stdcall;
    Function ProcessUrlAction(dwAction : DWORD; pPolicy : Pointer; cbPolicy : DWORD; pContext : Pointer; cbContext, dwFlags, dwReserved : DWORD) : HResult; stdcall;
    Function QueryCustomPolicy(const guidKey : TGUID; pPolicy : PPointer; cbPolicy : PDWORD; pContext : Pointer; cbContext, dwReserved : DWORD) : HResult; stdcall;
  end;
  PIInternetHostSecurityManagerRaw = ^IInternetHostSecurityManagerRaw;

  IInternetHostSecurityManager = interface ['{3af280b6-cb3f-11d0-891e-00c04fb6bfc4}']
    Function GetSecurityId(pbSecurityId : Pointer; var cbSecurityId : DWORD; dwReserved : DWORD) : HResult; stdcall;
    Function ProcessUrlAction(dwAction : DWORD; pPolicy : Pointer; cbPolicy : DWORD; pContext : Pointer; cbContext, dwFlags, dwReserved : DWORD) : HResult; stdcall;
    Function QueryCustomPolicy(const guidKey : TGUID; out pPolicy : Pointer; out cbPolicy : DWORD; pContext : Pointer; cbContext, dwReserved : DWORD) : HResult; stdcall;
  end;
  PIInternetHostSecurityManager = ^IInternetHostSecurityManager;

  IInternetZoneManagerRaw = interface  ['{79eac9ef-baf9-11ce-8c82-00aa004ba90b}']
    Function GetZoneAttributes(dwZone : DWORD; ZoneAttributes : PZoneAttributes) : HResult; stdcall;
    Function SetZoneAttributes(dwZone : DWORD; const ZoneAttributes : TZoneAttributes) : HResult; stdcall;
    Function GetZoneCustomPolicy(dwZone : DWORD; const guidKey : TGUID; pPolicy : PPointer; cbPolicy : PDWORD; urlZoneReg : TUrlZoneReg) : HResult; stdcall;
    Function SetZoneCustomPolicy(dwZone : DWORD; const guidKey : TGUID; pPolicy : Pointer; cbPolicy : DWORD; urlZoneReg : TUrlZoneReg) : HResult; stdcall;
    Function GetZoneActionPolicy(dwZone, dwAction : DWORD; pPolicy : Pointer; cbPolicy : DWORD; urlZoneReg : TUrlZoneReg) : HResult; stdcall;
    Function SetZoneActionPolicy(dwZone, dwAction : DWORD; pPolicy : Pointer; cbPolicy : DWORD; urlZoneReg : TUrlZoneReg) : HResult; stdcall;
    Function PromptAction(dwAction : DWORD; hwndParent : HWnd; pwszUrl, pwszText : LPCWSTR; dwPromptFlags : DWORD) : HResult; stdcall;
    Function LogAction(dwAction : DWORD; pwszUrl, pwszText : LPCWSTR; dwLogFlags : DWORD) : HResult; stdcall;
    Function CreateZoneEnumerator(dwEnum, dwCount : PDWORD; dwFlags : DWORD) : HResult; stdcall;
    Function GetZoneAt(dwEnum, dwIndex : DWORD; dwZone : PDWORD) : HResult; stdcall;
    Function DestroyZoneEnumerator(dwEnum : DWORD) : HResult; stdcall;
    Function CopyTemplatePoliciesToZone(dwTemplate, dwZone, dwReserved : DWORD) : HResult; stdcall;
  end;
  PIInternetZoneManagerRaw = ^IInternetZoneManagerRaw;
    
  IInternetZoneManager = interface  ['{79eac9ef-baf9-11ce-8c82-00aa004ba90b}']
    Function GetZoneAttributes(dwZone : DWORD; var ZoneAttributes : TZoneAttributes) : HResult; stdcall;
    Function SetZoneAttributes(dwZone : DWORD; const ZoneAttributes : TZoneAttributes) : HResult; stdcall;
    Function GetZoneCustomPolicy(dwZone : DWORD; const guidKey : TGUID; out pPolicy : Pointer; out cbPolicy : DWORD; urlZoneReg : TUrlZoneReg) : HResult; stdcall;
    Function SetZoneCustomPolicy(dwZone : DWORD; const guidKey : TGUID; pPolicy : Pointer; cbPolicy : DWORD; urlZoneReg : TUrlZoneReg) : HResult; stdcall;
    Function GetZoneActionPolicy(dwZone, dwAction : DWORD; pPolicy : Pointer; cbPolicy : DWORD; urlZoneReg : TUrlZoneReg) : HResult; stdcall;
    Function SetZoneActionPolicy(dwZone, dwAction : DWORD; pPolicy : Pointer; cbPolicy : DWORD; urlZoneReg : TUrlZoneReg) : HResult; stdcall;
    Function PromptAction(dwAction : DWORD; hwndParent : HWnd; pwszUrl, pwszText : LPCWSTR; dwPromptFlags : DWORD) : HResult; stdcall;
    Function LogAction(dwAction : DWORD; pwszUrl, pwszText : LPCWSTR; dwLogFlags : DWORD) : HResult; stdcall;
    Function CreateZoneEnumerator(out dwEnum, dwCount : DWORD; dwFlags : DWORD) : HResult; stdcall;
    Function GetZoneAt(dwEnum, dwIndex : DWORD; out dwZone : DWORD) : HResult; stdcall;
    Function DestroyZoneEnumerator(dwEnum : DWORD) : HResult; stdcall;
    Function CopyTemplatePoliciesToZone(dwTemplate, dwZone, dwReserved : DWORD) : HResult; stdcall;
  end;
  PIInternetZoneManager = ^IInternetZoneManager;
    
  ISoftDistExtRaw = interface ['{B15B8DC1-C7E1-11d0-8680-00AA00BDCB71}']
    Function ProcessSoftDist(szCDFURL : LPCWSTR; SoftDistElement : Pointer; lpdsi : PSoftDistInfo) : HResult; stdcall;
    Function GetFirstCodeBase(szCodeBase : PLPWSTR;const dwMaxSize : DWORD) : HResult; stdcall;
    Function GetNextCodeBase(szCodeBase : PLPWSTR; const dwMaxSize : DWORD) : HResult; stdcall;
    Function AsyncInstallDistributionUnit(bc : IBindCtx; pvReserved : Pointer; flags : DWORD; const cbh : TCodeBaseHold) : HResult; stdcall;
  end;
  PISoftDistExtRaw = ^ISoftDistExtRaw;
  
  ISoftDistExt = interface ['{B15B8DC1-C7E1-11d0-8680-00AA00BDCB71}']
    Function ProcessSoftDist(szCDFURL : LPCWSTR; SoftDistElement : Pointer; var lpdsi : TSoftDistInfo) : HResult; stdcall;
    Function GetFirstCodeBase(var szCodeBase : LPWSTR;const dwMaxSize : DWORD) : HResult; stdcall;
    Function GetNextCodeBase(var szCodeBase : LPWSTR; const dwMaxSize : DWORD) : HResult; stdcall;
    Function AsyncInstallDistributionUnit(bc : IBindCtx; pvReserved : Pointer; flags : DWORD; const cbh : TCodeBaseHold) : HResult; stdcall;
  end;
  PISoftDistExt = ^ISoftDistExt;

  IDataFilterRaw = interface ['{69d14c80-c18e-11d0-a9ce-006097942311}']
    Function DoEncode(dwFlags : DWORD; lInBufferSize : Longint; pbInBuffer : Pointer; lOutBufferSize : Longint; pbOutBuffer : Pointer; lInBytesAvailable : Longint; lInBytesRead, lOutBytesWritten : PLongint; dwReserved : DWORD) : HResult; stdcall;
    Function DoDecode(dwFlags : DWORD; lInBufferSize : Longint; pbInBuffer : Pointer; lOutBufferSize : Longint; pbOutBuffer : Pointer; lInBytesAvailable : Longint; lInBytesRead, lOutBytesWritten : PLongint; dwReserved : DWORD) : HResult; stdcall;
    Function SetEncodingLevel(dwEncLevel : DWORD) : HResult; stdcall;
  end;
  PIDataFilterRaw = ^IDataFilterRaw;

  IDataFilter = interface ['{69d14c80-c18e-11d0-a9ce-006097942311}']
    Function DoEncode(dwFlags : DWORD; lInBufferSize : Longint; pbInBuffer : Pointer; lOutBufferSize : Longint; pbOutBuffer : Pointer; lInBytesAvailable : Longint;  out lInBytesRead, lOutBytesWritten : Longint; dwReserved : DWORD) : HResult; stdcall;
    Function DoDecode(dwFlags : DWORD; lInBufferSize : Longint; pbInBuffer : Pointer; lOutBufferSize : Longint; pbOutBuffer : Pointer; lInBytesAvailable : Longint;    out lInBytesRead, lOutBytesWritten : Longint; dwReserved : DWORD) : HResult; stdcall;
    Function SetEncodingLevel(dwEncLevel : DWORD) : HResult; stdcall;
  end;
  PIDataFilter = ^IDataFilter;

  IEncodingFilterFactoryRaw = interface ['{70bdde00-c18e-11d0-a9ce-006097942311}']
    Function FindBestFilter(pwzCodeIn, pwzCodeOut : LPCWSTR; info : TDataInfo; DF : PIDataFilterRaw) : HResult; stdcall;
    Function GetDefaultFilter(pwzCodeIn, pwzCodeOut : LPCWSTR; info : TDataInfo; DF : PIDataFilterRaw) : HResult; stdcall;
  end;
  PIEncodingFilterFactoryRaw = ^IEncodingFilterFactoryRaw;
  
  IEncodingFilterFactory = interface ['{70bdde00-c18e-11d0-a9ce-006097942311}']
    Function FindBestFilter(pwzCodeIn, pwzCodeOut : LPCWSTR; info : TDataInfo; out DF : IDataFilter) : HResult; stdcall;
    Function GetDefaultFilter(pwzCodeIn, pwzCodeOut : LPCWSTR; info : TDataInfo; out DF : IDataFilter) : HResult; stdcall;
  end;
  PIEncodingFilterFactory = ^IEncodingFilterFactory;
  
Function CoGetClassObjectFromURL(const rCLASSID : TCLSID; szCODE : LPCWSTR; dwFileVersionMS, dwFileVersionLS : DWORD; szTYPE : LPCWSTR; pBindCtx : IBindCtx; dwClsContext : DWORD;  pvReserved : Pointer; const riid : TGUID; ppv  : pointer) : HResult; stdcall; external liburlmon;
Function CoGetClassObjectFromURL(const rCLASSID : TCLSID; szCODE : LPCWSTR; dwFileVersionMS, dwFileVersionLS : DWORD; szTYPE : LPCWSTR; pBindCtx : IBindCtx; dwClsContext : DWORD;  pvReserved : Pointer; const riid : TGUID; out ppv) : HResult; stdcall; external liburlmon;
Function CoInternetCombineUrl(pwzBaseUrl, pwzRelativeUrl : LPCWSTR; dwCombineFlags : DWORD; pszResult : LPWSTR; cchResult : DWORD; pcchResult : PDWORD; dwReserved : DWORD) : HResult ; stdcall; external liburlmon;
Function CoInternetCombineUrl(pwzBaseUrl, pwzRelativeUrl : LPCWSTR; dwCombineFlags : DWORD; pszResult : LPWSTR; cchResult : DWORD; var pcchResult : DWORD; dwReserved : DWORD) : HResult ; stdcall; external liburlmon;
Function CoInternetCompareUrl(pwzUrl1, pwzUrl2 : LPCWSTR; dwFlags : DWORD) : HResult; stdcall; external liburlmon; 
Function CoInternetGetProtocolFlags(pwzUrl : LPCWSTR; dwFlags : PDWORD; dwReserved : DWORD) : HResult; stdcall; external liburlmon;
Function CoInternetGetProtocolFlags(pwzUrl : LPCWSTR; var dwFlags : DWORD; dwReserved : DWORD) : HResult; stdcall; external liburlmon;
Function CoInternetCreateSecurityManager(SP : IServiceProvider; SM : PIInternetSecurityManager; dwReserved : DWORD) : HResult; stdcall; external liburlmon;
Function CoInternetCreateSecurityManager(SP : IServiceProvider; var SM : IInternetSecurityManager; dwReserved : DWORD) : HResult; stdcall; external liburlmon;
Function CoInternetCreateZoneManager(SP : IServiceProvider; ZM : PIInternetZoneManager; dwReserved : DWORD) : HResult; stdcall; external liburlmon;
Function CoInternetCreateZoneManager(SP : IServiceProvider; var ZM : IInternetZoneManager; dwReserved : DWORD) : HResult; stdcall; external liburlmon;
Function CoInternetGetSecurityUrl(pwzUrl : LPCWSTR; pwzSecUrl : PLPWSTR; psuAction : TPSUAction; dwReserved : DWORD) : HResult; stdcall; external liburlmon;
Function CoInternetGetSecurityUrl(pwzUrl : LPCWSTR; var pwzSecUrl : LPWSTR; psuAction : TPSUAction; dwReserved : DWORD) : HResult; stdcall; external liburlmon;
Function CoInternetGetSession(dwSessionMode : DWORD; pIInternetSession : PIInternetSession; dwReserved : DWORD) : HResult; stdcall; external liburlmon;
Function CoInternetGetSession(dwSessionMode : DWORD; var pIInternetSession : IInternetSession; dwReserved : DWORD) : HResult; stdcall; external liburlmon;
Function CoInternetParseUrl(pwzUrl : LPCWSTR; ParseAction : TParseAction; dwFlags : DWORD; pszResult : LPWSTR; cchResult : DWORD; pcchResult : PDWORD; dwReserved : DWORD) : HResult; stdcall; external liburlmon;
Function CoInternetParseUrl(pwzUrl : LPCWSTR; ParseAction : TParseAction; dwFlags : DWORD; pszResult : LPWSTR; cchResult : DWORD; var pcchResult : DWORD; dwReserved : DWORD) : HResult; stdcall; external liburlmon;
Function CoInternetQueryInfo(pwzUrl : LPCWSTR; QueryOptions : TQueryOption; dwQueryFlags : DWORD; pvBuffer : Pointer; cbBuffer : DWORD; pcbBuffer : PDWORD; dwReserved : DWORD) : HResult; stdcall; external liburlmon;
Function CoInternetQueryInfo(pwzUrl : LPCWSTR; QueryOptions : TQueryOption; dwQueryFlags : DWORD; pvBuffer : Pointer; cbBuffer : DWORD; var pcbBuffer : DWORD; dwReserved : DWORD) : HResult; stdcall; external liburlmon;
Function CopyBindInfo(const cbiSrc : TBindInfo; biDest : PBindInfo) : HResult; stdcall; external liburlmon;
Function CopyBindInfo(const cbiSrc : TBindInfo; var biDest : TBindInfo) : HResult; stdcall; external liburlmon;
Function CopyStgMedium(const cstgmedSrc : TStgMedium; stgmedDest : PStgMedium) : HResult; stdcall; external liburlmon;
Function CopyStgMedium(const cstgmedSrc : TStgMedium; var stgmedDest : TStgMedium) : HResult; stdcall; external liburlmon;
Function CreateAsyncBindCtxEx(pbc : IBindCtx; dwOptions : DWORD; BSCb : IBindStatusCallback; Enum : IEnumFORMATETC; ppBC : PIBindCtx; reserved : DWORD) : HResult; stdcall; external liburlmon;
Function CreateAsyncBindCtxEx(pbc : IBindCtx; dwOptions : DWORD; BSCb : IBindStatusCallback; Enum : IEnumFORMATETC; out ppBC : IBindCtx; reserved : DWORD) : HResult; stdcall; external liburlmon;
Function CreateAsyncBindCtx(reserved : DWORD; pBSCb : IBindStatusCallback; pEFetc : IEnumFORMATETC; ppBC : PIBindCtx) : HResult; stdcall; external liburlmon;
Function CreateAsyncBindCtx(reserved : DWORD; pBSCb : IBindStatusCallback; pEFetc : IEnumFORMATETC; out ppBC : IBindCtx) : HResult; stdcall; external liburlmon;
Function CreateFormatEnumerator(cfmtetc : UINT; const rgfmtetc : TFormatEtc; ppenumfmtetc : PIEnumFormatEtc) : HResult; stdcall; external liburlmon;
Function CreateFormatEnumerator(cfmtetc : UINT; const rgfmtetc : TFormatEtc; out ppenumfmtetc : IEnumFormatEtc) : HResult; stdcall; external liburlmon;
Function CreateURLBinding(lpszUrl : LPCWSTR; pbc : IBindCtx; ppBdg : PIBinding) : HResult; stdcall; external liburlmon;
Function CreateURLBinding(lpszUrl : LPCWSTR; pbc : IBindCtx; out ppBdg : IBinding) : HResult; stdcall; external liburlmon;
Function CreateURLMoniker(MkCtx : IMoniker; szURL : LPCWSTR; mk : PIMoniker) : HResult; stdcall; external liburlmon;
Function CreateURLMoniker(MkCtx : IMoniker; szURL : LPCWSTR; out mk : IMoniker) : HResult; stdcall; external liburlmon;
Function FindMediaTypeClass(pBC : IBindCtx; szType : LPCSTR; const pclsID : TCLSID; reserved : DWORD) : HResult; stdcall; external liburlmon;
Function FindMediaType(rgszTypes : LPCSTR; rgcfTypes : PClipFormat) : HResult; stdcall; external liburlmon;
Function FindMimeFromData(pBC : IBindCtx; pwzUrl : LPCWSTR; pBuffer : Pointer; cbSize : DWORD; pwzMimeProposed : LPCWSTR; dwMimeFlags : DWORD; ppwzMimeOut : PLPWSTR; dwReserved : DWORD) : HResult; stdcall; external liburlmon;
Function FindMimeFromData(pBC : IBindCtx; pwzUrl : LPCWSTR; pBuffer : Pointer; cbSize : DWORD; pwzMimeProposed : LPCWSTR; dwMimeFlags : DWORD; out ppwzMimeOut : LPWSTR; dwReserved : DWORD) : HResult; stdcall; external liburlmon;
Function GetClassFileOrMime(pBC : IBindCtx; szFilename : LPCWSTR; pBuffer : Pointer; cbSize : DWORD; szMime : LPCWSTR; dwReserved : DWORD; pclsid : PCLSID) : HResult; stdcall; external liburlmon;
Function GetClassFileOrMime(pBC : IBindCtx; szFilename : LPCWSTR; pBuffer : Pointer; cbSize : DWORD; szMime : LPCWSTR; dwReserved : DWORD; out pclsid : TCLSID) : HResult; stdcall; external liburlmon;
Function GetClassURL(szURL : LPCWSTR; const ClsID : TCLSID) : HResult; stdcall; external liburlmon;
Function GetSoftwareUpdateInfo(szDistUnit : LPCWSTR; var dsi : TSoftDistInfo) : HResult; stdcall; external liburlmon;
Function HlinkGoBack(unk : IUnknown) : HResult; stdcall; external liburlmon;
Function HlinkGoForward(unk : IUnknown) : HResult; stdcall; external liburlmon;
Function HlinkNavigateMoniker(Unk : IUnknown; mkTarget : IMoniker) : HResult; stdcall; external liburlmon;
Function HlinkNavigateString(unk : IUnknown; szTarget : LPCWSTR) : HResult; stdcall; external liburlmon;
Function HlinkSimpleNavigateToMoniker( mkTarget : Imoniker; szLocation, szTargetFrameName : LPCWSTR; Unk : IUnknown; bc : IBindCtx; BSC : IBindStatusCallback; grfHLNF, dwReserved : DWORD) : HResult; stdcall; external liburlmon;
Function HlinkSimpleNavigateToString(szTarget, szLocation, szTargetFrameName : LPCWSTR; Unk : IUnknown; pbc : IBindCtx; BSC : IBindStatusCallback; grfHLNF, dwReserved : DWORD) : HResult; stdcall; external liburlmon;
Function IsAsyncMoniker(pmk : IMoniker) : HResult; stdcall; external liburlmon;
Function IsLoggingEnabledA(pszUrl : PAnsiChar) : BOOL; stdcall; external liburlmon;
Function IsLoggingEnabledW(pszUrl : PWideChar) : BOOL; stdcall; external liburlmon;
Function IsValidURL(pBC : IBindCtx; szURL : LPCWSTR; dwReserved : DWORD) : HResult; stdcall; external liburlmon;
Function MkParseDisplayNameEx(pbc : IBindCtx; szDisplayName : LPCWSTR; pchEaten : PULONG; ppmk : PIMoniker) : HResult; stdcall; external liburlmon;
Function MkParseDisplayNameEx(pbc : IBindCtx; szDisplayName : LPCWSTR; out pchEaten : ULONG; out ppmk : IMoniker) : HResult; stdcall; external liburlmon;
Function ObtainUserAgentString(dwOption : DWORD; pszUAOut : LPSTR; cbSize : PDWORD) : HResult; stdcall; external liburlmon;
Function ObtainUserAgentString(dwOption : DWORD; pszUAOut : LPSTR; var cbSize : DWORD) : HResult; stdcall; external liburlmon;
Function RegisterBindStatusCallback(pBC : IBindCtx; pBSCb : IBindStatusCallback; ppBSCBPrev : PIBindStatusCallback; dwReserved : DWORD) : HResult; stdcall; external liburlmon;
Function RegisterBindStatusCallback(pBC : IBindCtx; pBSCb : IBindStatusCallback; out ppBSCBPrev : IBindStatusCallback; dwReserved : DWORD) : HResult; stdcall; external liburlmon;
Function RegisterFormatEnumerator(pBC : IBindCtx; pEFetc : IEnumFormatEtc; reserved : DWORD) : HResult; stdcall; external liburlmon;
Function RegisterMediaTypeClass(pBC : IBindCtx; ctypes : UINT; const rgszTypes : LPCSTR; rgclsID : PCLSID; reserved : DWORD) : HResult; stdcall; external liburlmon;
Function RegisterMediaTypes(ctypes : UINT; const rgszTypes : LPCSTR; const rgcfTypes : TClipFormat) : HResult; stdcall; external liburlmon;
Function RevokeBindStatusCallback(pBC : IBindCtx; pBSCb : IBindStatusCallback) : HResult; stdcall; external liburlmon;
Function RevokeFormatEnumerator(pBC : IBindCtx; pEFetc : IEnumFormatEtc) : HResult; stdcall; external liburlmon;
Function SetSoftwareUpdateAdvertisementState(szDistUnit : LPCWSTR;dwAdState, dwAdvertisedVersionMS, dwAdvertisedVersionLS : DWORD) : HResult; stdcall; external liburlmon;
Function URLDownloadToCacheFileA(p1 : IUnknown; p2 : PAnsiChar; p3 : PAnsiChar; p4 : DWORD; p5 : DWORD; p6 : IBindStatusCallback) : HResult; stdcall; external liburlmon;
Function URLDownloadToCacheFileW(p1 : IUnknown; p2 : PWideChar; p3 : PWideChar; p4 : DWORD; p5 : DWORD; p6 : IBindStatusCallback) : HResult; stdcall; external liburlmon;
Function URLDownloadToFileA(Caller : IUnknown; URL : PAnsiChar; FileName : PAnsiChar; Reserved : DWORD; StatusCB : IBindStatusCallback) : HResult; stdcall; external liburlmon;
Function URLDownloadToFileW(Caller : IUnknown; URL : PWideChar; FileName : PWideChar; Reserved : DWORD; StatusCB : IBindStatusCallback) : HResult; stdcall; external liburlmon;
Function UrlMkGetSessionOption(dwOption : DWORD; pBuffer : Pointer; dwBufferLength : DWORD; pdwBufferLength : PDWORD; dwReserved : DWORD) : HResult; stdcall; external liburlmon;
Function UrlMkGetSessionOption(dwOption : DWORD; pBuffer : Pointer; dwBufferLength : DWORD; out pdwBufferLength : DWORD; dwReserved : DWORD) : HResult; stdcall; external liburlmon;
Function UrlMkSetSessionOption(dwOption : DWORD; pBuffer : Pointer; dwBufferLength, dwReserved : DWORD) : HResult; stdcall; external liburlmon;
Function URLOpenBlockingStreamA(p1 : IUnknown; p2 : PAnsiChar; p3 : PIStream; p4 : DWORD; p5 : IBindStatusCallback) : HResult; stdcall; external liburlmon;
Function URLOpenBlockingStreamA(p1 : IUnknown; p2 : PAnsiChar; out p3 : IStream; p4 : DWORD; p5 : IBindStatusCallback) : HResult; stdcall; external liburlmon;
Function URLOpenBlockingStreamW(p1 : IUnknown; p2 : PWideChar; p3 : PIStream; p4 : DWORD; p5 : IBindStatusCallback) : HResult; stdcall; external liburlmon;
Function URLOpenBlockingStreamW(p1 : IUnknown; p2 : PWideChar; out p3 : IStream; p4 : DWORD; p5 : IBindStatusCallback) : HResult; stdcall; external liburlmon;
Function URLOpenPullStreamA(p1 : IUnknown; p2 : PAnsiChar; p3 : DWORD; BSC : IBindStatusCallback) : HResult; stdcall; external liburlmon;
Function URLOpenPullStreamW(p1 : IUnknown; p2 : PWideChar; p3 : DWORD; BSC : IBindStatusCallback) : HResult; stdcall; external liburlmon;
Function URLOpenStreamA(p1 : IUnknown; p2 : PAnsiChar; p3 : DWORD; p4 : IBindStatusCallback) : HResult; stdcall; external liburlmon;
Function URLOpenStreamW(p1 : IUnknown; p2 : PWideChar; p3 : DWORD; p4 : IBindStatusCallback) : HResult; stdcall; external liburlmon;
Function WriteHitLogging(const Logginginfo : THIT_LOGGING_INFO) : BOOL; stdcall; external liburlmon;
Procedure ReleaseBindInfo(const bindinfo : TBindInfo); stdcall; external liburlmon; external liburlmon;

implementation

end.
