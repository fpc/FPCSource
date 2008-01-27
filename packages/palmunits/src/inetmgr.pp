{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1996-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: INetMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *   This header file contains equates for the Internet Library.
 *
 * History:
 *    6/2/97   Created by Ron Marianetti
 *    12/23/99 Fix <> vs. "" problem. (jmp)
 *
 *****************************************************************************)

unit inetmgr;

interface

uses palmos, libtraps, errorbase, datamgr, systemresources, event_;

// Creator. Used for both the database that contains the INet Library and
//  it's features for the feature manager.
const
  inetCreator = sysFileCINetLib; // The Net Library creator

// INet Library features have this creator
  inetLibFtrCreator = sysFileCINetLib; // creatorID of INet Lib features.

// Name of the InetLib
  inetLibName = 'INet.lib'; // pass in to SysLibFind()

// Feature inetCreator, #0 is index of the the version number feature.
// The Feature creator is inetLibFtrCreator.
// Encoding is: 0xMMmfsbbb, where MM is major version, m is minor version
// f is bug fix, s is stage: 3-release,2-beta,1-alpha,0-development,
// bbb is build number for non-releases
// V1.12b3   would be: 0x01122003
// V2.00a2   would be: 0x02001002
// V1.01     would be: 0x01013000

  inetFtrNumVersion = 0;

  // INetLib owns the Ctp device bits feature.
  // Those bits contains device specific info bits that are sent to the Elaine server.
  // See Ctp.h for the bit descriptions
  inetFtrNumCtpDeviceBits1 = 1;

  inetLibType = sysFileTLibrary; // Our Net Code Resources Database type

// ID for proxy IP address in flash
  inetFlashProxyID = 'IP';
  inetDefaultFlashProxyID = 'DP';

//Also uses mobitexNetworkIdUS and mobitexNetworkIdCanada (0xb433 and 0xc4d7) to store
//current proxies for US and Canada. The responsibility for writing these and keeping
//them in sync lies with the Wireless panel, not with netlib.

//-----------------------------------------------------------------------------
// IP addresses of Elaine servers - used for default wireless proxies
//-----------------------------------------------------------------------------

  netProxyIPManhattanHGA        = $0A0186A5;      // Manhattan HGA = 10.1.134.165 or MAN 100005
  netProxyIPDefaultHGA          = netProxyIPManhattanHGA;
  netProxyIPDefaultHGAStr       = '10.1.134.165'; // Should correspond to above value

(********************************************************************
 * Error codes
 ********************************************************************)

  inetErrNone                  = 0;
  inetErrTooManyClients        = inetErrorClass or 1;        // Too many clients already
  inetErrHandleInvalid         = inetErrorClass or 2;        // Invalid inetH or sockH
  inetErrParamsInvalid         = inetErrorClass or 3;
  inetErrURLVersionInvalid     = inetErrorClass or 4;
  inetErrURLBufTooSmall        = inetErrorClass or 5;
  inetErrURLInvalid            = inetErrorClass or 6;
  inetErrTooManySockets        = inetErrorClass or 7;
  inetErrNoRequestCreated      = inetErrorClass or 8;
  inetErrNotConnected          = inetErrorClass or 9;
  inetErrInvalidRequest        = inetErrorClass or 10;
  inetErrNeedTime              = inetErrorClass or 11;
  inetErrHostnameInvalid       = inetErrorClass or 12;
  inetErrInvalidPort           = inetErrorClass or 13;
  inetErrInvalidHostAddr       = inetErrorClass or 14;
  inetErrNilBuffer             = inetErrorClass or 15;
  inetErrConnectTimeout        = inetErrorClass or 16;
  inetErrResolveTimeout        = inetErrorClass or 17;
  inetErrSendReqTimeout        = inetErrorClass or 18;
  inetErrReadTimeout           = inetErrorClass or 19;
  inetErrBufTooSmall           = inetErrorClass or 20;
  inetErrSchemeNotSupported    = inetErrorClass or 21;
  inetErrInvalidResponse       = inetErrorClass or 22;
  inetErrSettingTooLarge       = inetErrorClass or 25;
  inetErrSettingSizeInvalid    = inetErrorClass or 26;
  inetErrRequestTooLong        = inetErrorClass or 27;
  inetErrSettingNotImplemented = inetErrorClass or 28;

// Configuration errors
  inetErrConfigNotFound         = inetErrorClass or 29;
  inetErrConfigCantDelete       = inetErrorClass or 30;
  inetErrConfigTooMany          = inetErrorClass or 31;
  inetErrConfigBadName          = inetErrorClass or 32;
  inetErrConfigNotAlias         = inetErrorClass or 33;
  inetErrConfigCantPointToAlias = inetErrorClass or 34;
  inetErrConfigEmpty            = inetErrorClass or 35;
  inetErrConfigAliasErr         = inetErrorClass or 37;

  inetErrNoWirelessInterface    = inetErrorClass or 38;

// Encryption related errors
  inetErrEncryptionNotAvail     = inetErrorClass or 39;
    // Need to re-send transaction because server told us to reset our
    //  encryption sequence number
  inetErrNeedRetryEncSeqNum     = inetErrorClass or 40;
    // Need to re-send transaction because server sent us a new
    //  public key to use.
  inetErrNeedRetryEncPublicKey  = inetErrorClass or 41;

  inetErrResponseTooShort       = inetErrorClass or 42;

// errors specific to handling Mobitex ILLEGAL responses
  inetErrMobitexIllegalOKHost   = inetErrorClass or 43;
  inetErrMobitexIllegalBadHost  = inetErrorClass or 44;
// see error 92 also

// HTTP errors
  inetErrHTTPBadRequest         = inetErrorClass or 45;
  inetErrHTTPUnauthorized       = inetErrorClass or 46;
  inetErrHTTPForbidden          = inetErrorClass or 47;
  inetErrHTTPNotFound           = inetErrorClass or 48;
  inetErrHTTPMethodNotAllowed   = inetErrorClass or 49;
  inetErrHTTPNotAcceptable      = inetErrorClass or 50;
  inetErrHTTPProxyAuthRequired  = inetErrorClass or 51;
  inetErrHTTPRequestTimeout     = inetErrorClass or 52;
  inetErrHTTPConflict           = inetErrorClass or 53;
  inetErrHTTPGone               = inetErrorClass or 54;
  inetErrHTTPLengthRequired     = inetErrorClass or 55;
  inetErrHTTPPreconditionFailed = inetErrorClass or 56;
  inetErrHTTPRequestTooLarge    = inetErrorClass or 57;
  inetErrHTTPRequestURITooLong  = inetErrorClass or 58;
  inetErrHTTPUnsupportedType    = inetErrorClass or 59;
  inetErrHTTPServerError        = inetErrorClass or 60;

// CTP errors
  inetErrCTPServerError         = inetErrorClass or 61;

// Cache errors
  inetErrTypeNotCached          = inetErrorClass or 62;
  inetErrCacheInvalid           = inetErrorClass or 63;

// Palm: and PalmCall: scheme errors
  inetErrURLDispatched          = inetErrorClass or 64;
  inetErrDatabaseNotFound       = inetErrorClass or 65;

  inetErrCTPMalformedRequest    = inetErrorClass or 66;
  inetErrCTPUnknownCommand      = inetErrorClass or 67;
  inetErrCTPTruncated           = inetErrorClass or 68;
  inetErrCTPUnknownError        = inetErrorClass or 69;
  inetErrCTPProxyError          = inetErrorClass or 70;
  inetErrCTPSocketErr           = inetErrorClass or 71;

  inetErrCTPInvalidURL              = inetErrorClass or 72;
  inetErrCTPReferringPageOutOfDate  = inetErrorClass or 73;
  inetErrCTPBadRequest              = inetErrorClass or 74;
  inetErrUNUSED                     = inetErrorClass or 75;
  inetErrCTPMailServerDown          = inetErrorClass or 76;
  inetErrCTPHostNotFound            = inetErrorClass or 77;

// Content Conversion Errors
  inetErrCTPContentInvalidTag           = inetErrorClass or 78;
  inetErrCTPContentInternal             = inetErrorClass or 79;
  inetErrCTPContentDataEnd              = inetErrorClass or 80;
  inetErrCTPContentResourceTooBig       = inetErrorClass or 81;
  inetErrCTPContentNoNoFrames           = inetErrorClass or 82;
  inetErrCTPContentUnsupportedContent   = inetErrorClass or 83;
  inetErrCTPContentUnsupportedEncoding  = inetErrorClass or 84;
  inetErrCTPContentBadForm              = inetErrorClass or 85;
  inetErrCTPContentBadFormMissingAction = inetErrorClass or 86;
  inetErrCTPContentBadFormMissingMethod = inetErrorClass or 87;
  inetErrCTPContentNoSourceData         = inetErrorClass or 88;
  inetErrCTPContentBadImage             = inetErrorClass or 89;
  inetErrCTPContentImageTooLarge        = inetErrorClass or 90;

// Mobitex illegal handled error code.  This error is sent after
//INetLib handles inetErrMobitexIllegalOKHost or inetErrMobitexIllegalBadHost
//errors.  The application needs to know that something went wrong and it needs
//to change state.  This error does not need to be displayed to the user.
  inetErrMobitexErrorHandled = inetErrorClass or 91;

// Proxy down, non-default host, show dialog asking to revert to default
  inetErrProxyDownBadHost    = inetErrorClass or 92;

// A second type of readtime.  This should occur only when some data is received
// and the connection is lost.
  inetErrHostConnectionLost  = inetErrorClass or 93;

// Unable to locate link record within a PQA file
  inetErrLinkNotFound        = inetErrorClass or 94;

  inetErrCacheInvalidSort    = inetErrorClass or 95;

// The following are used and bit field parameters to the sort routine.  They
// are additive but ordered.  Precendence is given to the lowest ordered bit.
  inetCacheCompareByMasterURL = $01;
  inetCacheCompareByURL       = $02;
  inetCacheCompareByTime      = $04;

(********************************************************************
 * Input flags
 ********************************************************************)

//-----------------------------------------------------------------------------
// flag word definitions for INetLibURLOpen
//-----------------------------------------------------------------------------

  inetOpenURLFlagLookInCache = $0001;
  inetOpenURLFlagKeepInCache = $0002;
  inetOpenURLFlagForceEncOn  = $0008; // use encryption even if
                                      //  scheme does not desire it
  inetOpenURLFlagForceEncOff = $0010; // no encryption even if
                                      //  scheme desires it

//-----------------------------------------------------------------------------
// flag word definitions for INetURLInfo. These flags bits are set in the
//   flags field of the INetURLINfoType structure by INetLibURLGetInfo()
//-----------------------------------------------------------------------------

  inetURLInfoFlagIsSecure  = $0001;
  inetURLInfoFlagIsRemote  = $0002;
  inetURLInfoFlagIsInCache = $0004;

(********************************************************************
 * Configuration Support
 ********************************************************************)

//-----------------------------------------------------------------------------
// Names of built-in configuration aliases available through the
//  INetLibConfigXXX calls
//-----------------------------------------------------------------------------

  inetCfgNameDefault     = '.Default';     // The default configuration
  inetCfgNameDefWireline = '.DefWireline'; // The default wireline configuration
  inetCfgNameDefWireless = '.DefWireless'; // The default wireless configuration
  inetCfgNameCTPDefault  = '.CTPDefault';  // Points to either .CTPWireline or .CTPWireless
  inetCfgNameCTPWireline = '.CTPWireline'; // Wireline through the Jerry Proxy
  inetCfgNameCTPWireless = '.CTPWireless'; // Wireless through the Jerry Proxy

//--------------------------------------------------------------------
// Structure of a configuration name. Used by INetLibConfigXXX calls
//---------------------------------------------------------------------

const
  inetConfigNameSize = 32;

type
  INetConfigNameType = record
    name: array [0..inetConfigNameSize-1] of Char; // name of configuration
  end;

  INetConfigNamePtr = ^INetConfigNameType;

(********************************************************************
 * Scheme Types
 ********************************************************************)

type
  INetSchemeEnum = WordEnum; //!!!

const
  inetSchemeUnknown = -1;
  inetSchemeDefault = 0;

  inetSchemeHTTP = Succ(inetSchemeDefault);  // http:
  inetSchemeHTTPS = Succ(inetSchemeHTTP);    // https:
  inetSchemeFTP = Succ(inetSchemeHTTPS);     // ftp:
  inetSchemeGopher = Succ(inetSchemeFTP);    // gopher:
  inetSchemeFile = Succ(inetSchemeGopher);   // file:
  inetSchemeNews = Succ(inetSchemeFile);     // news:
  inetSchemeMailTo = Succ(inetSchemeNews);   // mailto:
  inetSchemePalm = Succ(inetSchemeMailTo);   // palm:
  inetSchemePalmCall = Succ(inetSchemePalm); // palmcall:

  inetSchemeMail = Succ(inetSchemePalmCall); // not applicable to URLS, but used
                                             //  for the INetLibSockOpen call when
                                             //  creating a socket for mail IO
  inetSchemeMac = Succ(inetSchemeMail);      // mac: - Mac file system HTML

  inetSchemeFirst = inetSchemeHTTP;          // first one
  inetSchemeLast  = inetSchemeMail;          // last one

(********************************************************************
 * Scheme Ports
 ********************************************************************)

const
  inetPortFTP    = 21;
  inetPortHTTP   = 80;
  inetPortGopher = 70;
  inetPortNews   = 119;
  inetPortHTTPS  = 44;

(********************************************************************
 * Structure of a cracked URL.
 ********************************************************************)

type
  INetURLType = record
    version: UInt16;     // should be 0, for future compatibility

    schemeP: ^UInt8;     // ptr to scheme portion
    schemeLen: UInt16;   // size of scheme portion
    schemeEnum: UInt16;  // INetSchemEnum

    usernameP: ^UInt8;   // ptr to username portion
    usernameLen: UInt16; // size of username

    passwordP: ^UInt8;   // ptr to password portion
    passwordLen: UInt16; // size of password

    hostnameP: ^UInt8;   // ptr to host name portion
    hostnameLen: UInt16; // size of host name

    port: UInt16;        // port number

    pathP: ^UInt8;       // ptr to path portion
    pathLen: UInt16;      // size of path

    paramP: ^UInt8;      // param (;param)
    paramLen: UInt16;    // size of param

    queryP: ^UInt8;      // query (?query)
    queryLen: UInt16;    // size of query

    fragP: ^UInt8;       // fragment (#frag)
    fragLen: UInt16;     // size of fragment
  end;

(********************************************************************
 * Structure for INetURLInfo. This structure is filled in with info
 *  about a URL.
 ********************************************************************)

type
  INetURLInfoType = record
    version: UInt16;   // should be 0, for future compatibility

    flags: UInt16;     // flags word, one or ore of
                       //   inetURLInfoFlagXXX flags
    undefined: UInt32; // reserved for future use
  end;

(********************************************************************
 * Content and Compression Type Enums(from proxy server or PQA Builder)
 ********************************************************************)

type
  INetContentTypeEnum = Enum;

const
  inetContentTypeTextPlain = 0;
  inetContentTypeTextHTML = Succ(inetContentTypeTextPlain);
  inetContentTypeImageGIF = Succ(inetContentTypeTextHTML);
  inetContentTypeImageJPEG = Succ(inetContentTypeImageGIF);
  inetContentTypeApplicationCML = Succ(inetContentTypeImageJPEG);
  inetContentTypeImagePalmOS = Succ(inetContentTypeApplicationCML);
  inetContentTypeOthe = Succ(inetContentTypeImagePalmOS);

type
  INetCompressionTypeEnum = Enum;

const
  inetCompressionTypeNone = 0;
  inetCompressionTypeBitPacked = Succ(inetCompressionTypeNone);
  inetCompressionTypeLZ77 = Succ(inetCompressionTypeBitPacked);
  inetCompressionTypeBest = Succ(inetCompressionTypeLZ77);
  inetCompressionTypeLZ77Primer1 = Succ(inetCompressionTypeBest);

(********************************************************************
 * Proxy Types
 ********************************************************************)

type
  INetProxyEnum = Enum;

const
  inetProxyNone = 0; // no proxy
  inetProxyCTP = 1;  // CTP (Jerry) proxy

(********************************************************************
 * Transport Types
 ********************************************************************)
type
  INetTransportEnum = Enum;

const
  inetTransportPPP     = 0; // PPP
  inetTransportMobitex = 1; // Mobitex

(********************************************************************
 * Settings for the INetLibSettingSet/Get call.
 ********************************************************************)

type
  INetSettingEnum = Enum;

const
  inetSettingProxyType = 0;                                    // (RW) UInt32, INetProxyEnum

  inetSettingProxyName = Succ(inetSettingProxyType);           // (RW) Char[], name of proxy
  inetSettingProxyPort = Succ(inetSettingProxyName);           // (RW) UInt32,  TCP port # of proxy

  inetSettingProxySocketType = Succ(inetSettingProxyPort);     // (RW) UInt32, which type of socket to use
                                                               //  netSocketTypeXXX

  inetSettingCacheSize = Succ(inetSettingProxySocketType);     // (RW) UInt32, max size of cache
  inetSettingCacheRef = Succ(inetSettingCacheSize);            // (R) DmOpenRef, ref of cache DB

  inetSettingNetLibConfig = Succ(inetSettingCacheRef);         // (RW) UInt32, Which NetLib config to use.

  inetSettingRadioID = Succ(inetSettingNetLibConfig);          // (R)  UInt32[2], the 64-bit radio ID
  inetSettingBaseStationID = Succ(inetSettingRadioID);         // (R)  UInt32, the radio base station ID

  inetSettingMaxRspSize = Succ(inetSettingBaseStationID);      // (W) UInt32 (in bytes)
  inetSettingConvAlgorithm = Succ(inetSettingMaxRspSize);      // (W) UInt32 (CTPConvEnum)
  inetSettingContentWidth = Succ(inetSettingConvAlgorithm);    // (W) UInt32 (in pixels)
  inetSettingContentVersion = Succ(inetSettingContentWidth);   // (W) UInt32 Content version (encoder version)

  inetSettingNoPersonalInfo = Succ(inetSettingContentVersion); // (RW) UInt32 send no deviceID/zipcode

  inetSettingUserName = Succ(inetSettingNoPersonalInfo);

  //---------------------------------------------------------------------------------
  // New Settings as of PalmOS 4.0
  //---------------------------------------------------------------------------------

  inetSettingGraphicsSel = Succ(inetSettingUserName);          // (W) UInt8 (User Graphics selection)

  inetSettingTransportType = Succ(inetSettingGraphicsSel);     // (RW) UInt32, INetTransportEnum

  inetSettingServerBits1 = Succ(inetSettingTransportType);     // (RW) UInt32, bits sent by the server over ctp
  inetSettingSendRawLocationInfo = Succ(inetSettingServerBits1); // (W) Boolean, make the handheld send its Raw Location information.
                                                                 // One use of this feature is to convert Web clipping's "%Location:..." codes into content info

  inetSettingEnableCookies = Succ(inetSettingSendRawLocationInfo); // (RW) Boolean
                                                                   // true:    Cookies are enabled
                                                                   // false:   Cookies are disabled (default)

  inetSettingMaxCookieJarSize = Succ(inetSettingEnableCookies);
                                                                   // (RW) UInt32, maximum cookie jar size in
                                                                   // in kilobytes

  // The following setting is a new interface in PalmOS 4.0 that allow Clipper
  // or other INetLib clients to get raw location information as described in
  // PalmLocRawData.h.
  // INetLib will return a pointer to a newly allocated memory buffer containing
  // the raw location information to send to Elaine (Web Clipping proxy server).
  // Elaine will then use a Windows DLL to analyse the raw location information
  // in order to transform it into something useful like zipcode, cityname, etc.
  // See PalmLocRawData.h for more details...
  inetSettingLocRawInfo = Succ(inetSettingMaxCookieJarSize);         // (R) void* Allocated memory buffer - must be free by caller

  // The following affect how the "Web Clipping" panel will edit the
  // configuration.  When "reset to default" is pressed, these default
  // values will be copied back into the appropriate place.
  // If the field is set to not be editable, the panel will not allow
  // the user to change it
  inetSettingProxyNameDefault = Succ(inetSettingLocRawInfo);         // Default Name for this config
  inetSettingProxyPortDefault = Succ(inetSettingProxyNameDefault);   // Default Port for this config
  inetSettingProxyNameEditable = Succ(inetSettingProxyPortDefault);  // Is the proxy name editable?
  inetSettingProxyPortEditable = Succ(inetSettingProxyNameEditable); // Is the proxy port editable?

  inetSettingPalmUserID = Succ(inetSettingProxyPortEditable);        // The palm.net user id

  inetSettingLast = Succ(inetSettingPalmUserID);

(********************************************************************
 * Settings for the INetLibSockSettingSet/Get call.
 ********************************************************************)

type
  INetSockSettingEnum = Enum;

const
  inetSockSettingScheme = 0;                                               // (R) UInt32, INetSchemeEnum
  inetSockSettingSockContext = Succ(inetSockSettingScheme);                // (RW) UInt32,

  inetSockSettingCompressionType = Succ(inetSockSettingSockContext);       // (R) Char[]
  inetSockSettingCompressionTypeID = Succ(inetSockSettingCompressionType); // (R) UInt32 (INetCompressionTypeEnum)
  inetSockSettingContentType = Succ(inetSockSettingCompressionTypeID);     // (R) Char[]
  inetSockSettingContentTypeID = Succ(inetSockSettingContentType);         // (R) UInt32 (INetContentTypeEnum)
  inetSockSettingData = Succ(inetSockSettingContentTypeID);                // (R) UInt32, pointer to data
  inetSockSettingDataHandle = Succ(inetSockSettingData);                   // (R) UInt32, MemHandle to data
  inetSockSettingDataOffset = Succ(inetSockSettingDataHandle);             // (R) UInt32, offset to data from MemHandle

  inetSockSettingTitle = Succ(inetSockSettingDataOffset);                  // (RW) Char[]
  inetSockSettingURL = Succ(inetSockSettingTitle);                         // (R) Char[]
  inetSockSettingIndexURL = Succ(inetSockSettingURL);                      // (RW) Char[]

  inetSockSettingFlags = Succ(inetSockSettingIndexURL);                    // (W) UInt16, one or more of
                                                                           //   inetOpenURLFlagXXX flags

  inetSockSettingReadTimeout = Succ(inetSockSettingFlags);                 // (RW) UInt32. Read timeout in ticks

  inetSockSettingContentVersion = Succ(inetSockSettingReadTimeout);        // (R) UInt32, version number for content

  inetSockSettingLast = Succ(inetSockSettingContentVersion);

(********************************************************************
 * Possible socket status values that can be returned from INetLibSockStatus
 ********************************************************************)

type
  INetStatusEnum = Enum;

const
  inetStatusNew = 0;                                                // just opened
  inetStatusResolvingName = Succ(inetStatusNew);                    // looking up host address
  inetStatusNameResolved = Succ(inetStatusResolvingName);           // found host address
  inetStatusConnecting = Succ(inetStatusNameResolved);              // connecting to host
  inetStatusConnected = Succ(inetStatusConnecting);                 // connected to host
  inetStatusSendingRequest = Succ(inetStatusConnected);             // sending request
  inetStatusWaitingForResponse = Succ(inetStatusSendingRequest);    // waiting for response
  inetStatusReceivingResponse = Succ(inetStatusWaitingForResponse); // receiving response
  inetStatusResponseReceived = Succ(inetStatusReceivingResponse);   // response received
  inetStatusClosingConnection = Succ(inetStatusResponseReceived);   // closing connection
  inetStatusClosed = Succ(inetStatusClosingConnection);             // closed
  inetStatusAcquiringNetwork = Succ(inetStatusClosed);              // network temporarily
                                                                    // unreachable; socket on hold
  inetStatusPrvInvalid = 30;                                        // internal value, not
                                                                    // returned by INetMgr. Should
                                                                    // be last.

(********************************************************************
 * HTTP Attributes which can be set/get using the
 *  INetLibHTTPAttrSet/Get calls.
 *
 * Generally, attributes are only set BEFORE calling
 *      INetLibSockHTTPReqSend
 * and attributes are only gotten AFTER the complete response
 *       has been received.
 *
 * Attributes marked with the following flags:
 *      (R) - read only
 *      (W) - write only
 *      (RW)    - read/write
 *      (-)     - not implemented yet
 ********************************************************************)

type
  INetHTTPAttrEnum = Enum;

const
  // local error trying to communicate with server, if any
  inetHTTPAttrCommErr = 0;                                                  // (R) UInt32, read-only

  // object attributes, defined at creation
  inetHTTPAttrEntityURL = Succ(inetHTTPAttrCommErr);                        // (-) Char[], which resource was requested

  //-----------------------------------------------------------
  // Request only attributes
  //-----------------------------------------------------------
  inetHTTPAttrReqAuthorization = Succ(inetHTTPAttrEntityURL);               // (-) Char[]
  inetHTTPAttrReqFrom = Succ(inetHTTPAttrReqAuthorization);                 // (-) Char[]
  inetHTTPAttrReqIfModifiedSince = Succ(inetHTTPAttrReqFrom);               // (-) UInt32
  inetHTTPAttrReqReferer = Succ(inetHTTPAttrReqIfModifiedSince);            // (-) Char[]

  // The following are ignored unless going through a CTP proxy
  inetHTTPAttrWhichPart = Succ(inetHTTPAttrReqReferer);                     // (W) UInt32 (0 -> N)
  inetHTTPAttrIncHTTP = Succ(inetHTTPAttrWhichPart);                        // (W) UInt32 (Boolean) only applicable
                                                                            //   when inetHTTPAttrConvAlgorithm set to
                                                                            //   ctpConvNone
  inetHTTPAttrCheckMailHi = Succ(inetHTTPAttrIncHTTP);                      // (W) UInt32
  inetHTTPAttrCheckMailLo = Succ(inetHTTPAttrCheckMailHi);                  // (W) UInt32
  inetHTTPAttrReqContentVersion = Succ(inetHTTPAttrCheckMailLo);            // (W) UInt32 Desired content version. Represented
                                                                            //  as 2 low bytes. Lowest byte is minor version,
                                                                            //  next higher byte is major version.

  //--------------------------------------------------------------
  // Response only attributes
  //--------------------------------------------------------------
  // Server response info
  inetHTTPAttrRspAll = Succ(inetHTTPAttrReqContentVersion);                 // (-) Char[] - entire HTTP response including
                                                                            //   data
  inetHTTPAttrRspSize = Succ(inetHTTPAttrRspAll);                           // (R) UInt32 - entire HTTP Response size including
                                                                            //   header and data
  inetHTTPAttrRspVersion = Succ(inetHTTPAttrRspSize);                       // (-) Char[]
  inetHTTPAttrResult = Succ(inetHTTPAttrRspVersion);                        // (R) UInt32 (ctpErrXXX when using CTP Proxy)
  inetHTTPAttrErrDetail = Succ(inetHTTPAttrResult);                         // (R) UInt32 (server/proxy err code when
                                                                            //      using CTP Proxy)
  inetHTTPAttrReason = Succ(inetHTTPAttrErrDetail);                         // (R) Char[]
  inetHTTPAttrDate = Succ(inetHTTPAttrReason);                              // (-) UInt32
  inetHTTPAttrNoCache = Succ(inetHTTPAttrDate);                             // (-) UInt32
  inetHTTPAttrPragma = Succ(inetHTTPAttrNoCache);                           // (-) Char[]
  inetHTTPAttrServer = Succ(inetHTTPAttrPragma);                            // (-) Char[]
  inetHTTPAttrWWWAuthentication = Succ(inetHTTPAttrServer);                 // (-) Char[]

  // Returned entity attributes
  inetHTTPAttrContentAllow = Succ(inetHTTPAttrWWWAuthentication);           // (-) Char[]
  inetHTTPAttrContentLength = Succ(inetHTTPAttrContentAllow);               // (R) UInt32
  inetHTTPAttrContentLengthUncompressed = Succ(inetHTTPAttrContentLength);  // (R) UInt32 (in bytes)
  inetHTTPAttrContentPtr = Succ(inetHTTPAttrContentLengthUncompressed);     // (-) Char *
  inetHTTPAttrContentExpires = Succ(inetHTTPAttrContentPtr);                // (-) UInt32
  inetHTTPAttrContentLastModified = Succ(inetHTTPAttrContentExpires);       // (-) UInt32
  inetHTTPAttrContentLocation = Succ(inetHTTPAttrContentLastModified);      // (-) Char[]
  inetHTTPAttrContentLengthUntruncated = Succ(inetHTTPAttrContentLocation); // (R) UInt32
  inetHTTPAttrContentVersion = Succ(inetHTTPAttrContentLengthUntruncated);  // (R) UInt32, actual content version. Represented
                                                                            //  as 2 low bytes. Lowest byte is minor version,
                                                                            //  next higher byte is major version.
  inetHTTPAttrContentCacheID = Succ(inetHTTPAttrContentVersion);            // (R) UInt32, cacheID for this item
  inetHTTPAttrReqSize = Succ(inetHTTPAttrContentCacheID);                   // (R) UInt32 size of request sent

(********************************************************************
 * Structure of our Internet events. This structure is a superset of
 *  the regular event type. Note that we use the first 2 user events
 *  for the Internet Library so any app that uses this library must be
 *  to use user event IDs greater than inetLastEvent.
 *
 *  library refNum in it....
 ********************************************************************)

const
  inetSockReadyEvent        = firstINetLibEvent;
  inetSockStatusChangeEvent = firstINetLibEvent + 1;
  inetLastEvent             = firstINetLibEvent + 1;

type
  generic = record
    datum: array [0..7] of UInt16;
  end;

  inetSockReady = record
    sockH: MemHandle;     // socket MemHandle
    context: UInt32;      // application defined
    inputReady: Boolean;  // true if ready for reads
    outputReady: Boolean; // true if ready for writes
  end;

  inetSockStatusChange = record
    sockH: MemHandle; // socket MemHandle
    context: UInt32;  // application defined
    status: UInt16;   // new status
    sockErr: Err;     // socket err, if any
  end;

  INetEventType = record
    eType: UInt16;
    penDown: Boolean;
    reserved: UInt8;
    screenX: Int16;
    screenY: Int16;
    case Integer of
      0: (generic: generic);
      1: (inetSockReady: inetSockReady);
      2: (inetSockStatusChange: inetSockStatusChange);
  end;

(********************************************************************
 * Commands for INetLibWirelessIndicatorCmd (and INetLibWiCmd)
 ********************************************************************)

type
  WiCmdEnum = Enum;

const
  wiCmdInit = 0;
  wiCmdClear = Succ(wiCmdInit);
  wiCmdSetEnabled = Succ(wiCmdClear);
  wiCmdDraw = Succ(wiCmdSetEnabled);
  wiCmdEnabled = Succ(wiCmdDraw);
  wiCmdSetLocation = Succ(wiCmdEnabled);
  wiCmdEras = Succ(wiCmdSetLocation);

(********************************************************************
 * INet Library functions.
 ********************************************************************)

type
  INetLibTrapNumberEnum = Enum;

const
  inetLibTrapSettingGet = sysLibTrapCustom;
  inetLibTrapSettingSet = Succ(inetLibTrapSettingGet);

  inetLibTrapGetEvent = Succ(inetLibTrapSettingSet);

  inetLibTrapURLOpen = Succ(inetLibTrapGetEvent);

  inetLibTrapSockRead = Succ(inetLibTrapURLOpen);
  inetLibTrapSockWrite = Succ(inetLibTrapSockRead);

  inetLibTrapSockOpen = Succ(inetLibTrapSockWrite);
  inetLibTrapSockClose = Succ(inetLibTrapSockOpen);
  inetLibTrapSockStatus = Succ(inetLibTrapSockClose);
  inetLibTrapSockSettingGet = Succ(inetLibTrapSockStatus);
  inetLibTrapSockSettingSet = Succ(inetLibTrapSockSettingGet);
  inetLibTrapSockConnect = Succ(inetLibTrapSockSettingSet);

  // Utilities
  inetLibTrapURLCrack = Succ(inetLibTrapSockConnect);
  inetLibTrapURLsAdd = Succ(inetLibTrapURLCrack);
  inetLibTrapURLsCompare = Succ(inetLibTrapURLsAdd);
  inetLibTrapURLGetInfo = Succ(inetLibTrapURLsCompare);

  // HTTP calls
  inetLibTrapSockHTTPReqCreate = Succ(inetLibTrapURLGetInfo);
  inetLibTrapSockHTTPAttrSet = Succ(inetLibTrapSockHTTPReqCreate);
  inetLibTrapSockHTTPReqSend = Succ(inetLibTrapSockHTTPAttrSet);
  inetLibTrapSockHTTPAttrGet = Succ(inetLibTrapSockHTTPReqSend);

  // Mail traps
  inetLibTrapSockMailReqCreate = Succ(inetLibTrapSockHTTPAttrGet);
  inetLibTrapSockMailAttrSet = Succ(inetLibTrapSockMailReqCreate);
  inetLibTrapSockMailReqAdd = Succ(inetLibTrapSockMailAttrSet);
  inetLibTrapSockMailReqSend = Succ(inetLibTrapSockMailReqAdd);
  inetLibTrapSockMailAttrGet = Succ(inetLibTrapSockMailReqSend);
  inetLibTrapSockMailQueryProgress = Succ(inetLibTrapSockMailAttrGet);

  // Cache calls
  inetLibTrapCacheList = Succ(inetLibTrapSockMailQueryProgress);
  inetLibTrapCacheGetObject = Succ(inetLibTrapCacheList);

  // Config calls
  inetLibConfigMakeActive_ = Succ(inetLibTrapCacheGetObject);
  inetLibConfigList_ = Succ(inetLibConfigMakeActive_);
  inetLibConfigIndexFromName_ = Succ(inetLibConfigList_);
  inetLibConfigDelete_ = Succ(inetLibConfigIndexFromName_);
  inetLibConfigSaveAs_ = Succ(inetLibConfigDelete_);
  inetLibConfigRename_ = Succ(inetLibConfigSaveAs_);
  inetLibConfigAliasSet_ = Succ(inetLibConfigRename_);
  inetLibConfigAliasGet_ = Succ(inetLibConfigAliasSet_);

  //old wireless Indicator
  inetLibTrapWiCmd = Succ(inetLibConfigAliasGet_);

  // File Calls
  inetLibTrapSockFileGetByIndex = Succ(inetLibTrapWiCmd);

  inetLibTrapCheckAntennaState = Succ(inetLibTrapSockFileGetByIndex);

  inetLibTrapCTPSend = Succ(inetLibTrapCheckAntennaState);

// Additional Cache calls
  inetLibTrapCachePurge = Succ(inetLibTrapCTPSend);

// new wireless Indicator
  inetLibTrapWirelessIndicatorCmd = Succ(inetLibTrapCachePurge);

// Additional Cache calls
  inetLibTrapCacheGetObjectV2 = Succ(inetLibTrapWirelessIndicatorCmd);
  inetLibTrapIndexedCacheFind = Succ(inetLibTrapCacheGetObjectV2);
  inetLibTrapPrepareCacheForHistory = Succ(inetLibTrapIndexedCacheFind);

//This should be at the END  ***********

  inetLibTrapLast = Succ(inetLibTrapPrepareCacheForHistory);

(********************************************************************
 * Structure of cache entry
 *  Used as a parameter to INetLibCacheList. If urlP or titleP are NULL,
 *  the corresponding length fields will be updated with the desired lengths
 ********************************************************************)

type
  INetCacheEntryType = record
    urlP: ^UInt8;
    urlLen: UInt16;

    titleP: ^UInt8;
    titleLen: UInt16;

    lastViewed: UInt32;  // seconds since 1/1/1904
    firstViewed: UInt32; // seconds since 1/1/1904
  end;

  INetCacheEntryP = ^INetCacheEntryType;

(********************************************************************
 * Structure for INetLibCacheGetObject. This structure is filled in with info
 *  about a cache entry.
 ********************************************************************)

type
  INetCacheInfoType = record
    recordH: MemHandle;
    contentType: INetContentTypeEnum;
    encodingType: INetCompressionTypeEnum;
    uncompressedDataSize: UInt32;
    flags: UInt8;
    reserved: UInt8;
    dataOffset: UInt16;  // offset to content
    dataLength: UInt16;  // size of content
    urlOffset: UInt16;   // offset to URL
    viewTime: UInt32;    // time last viewed
    createTime: UInt32;  // time entry was created
    murlOffset: UInt16;  // offset to master URL
    serverBits1: UInt32; // Bits sent by the server
  end;

  INetCacheInfoPtr = ^INetCacheInfoType;

//--------------------------------------------------
// Library initialization, shutdown, sleep and wake
//--------------------------------------------------
function INetLibOpen(libRefnum: UInt16; config: UInt16; flags: UInt32;
                     cacheRef: DmOpenRef; cacheSize: UInt32; var inetHP: MemHandle): Err; syscall sysLibTrapOpen;

function INetLibClose(libRefnum: UInt16; inetH: MemHandle): Err; syscall sysLibTrapClose;

function INetLibSleep(libRefnum: UInt16): Err; syscall sysLibTrapSleep;

function INetLibWake(libRefnum: UInt16): Err; syscall sysLibTrapWake;

//--------------------------------------------------
// Settings
//--------------------------------------------------
function INetLibSettingGet(libRefnum: UInt16; inetH: MemHandle; setting: UInt16 {INetSettingEnum};
                           bufP: Pointer; var bufLenP: UInt16): Err; syscall inetLibTrapSettingGet;

function INetLibSettingSet(libRefnum: UInt16; inetH: MemHandle; setting: UInt16 {INetSettingEnum};
                           bufP: Pointer; bufLen: UInt16): Err; syscall inetLibTrapSettingSet;

//--------------------------------------------------
// Event Management
//--------------------------------------------------

procedure INetLibGetEvent(libRefnum: UInt16; inetH: MemHandle; var eventP: INetEventType; timeout: Int32); syscall inetLibTrapGetEvent;

//--------------------------------------------------
// High level calls
//--------------------------------------------------

function INetLibURLOpen(libRefnum: UInt16; inetH: MemHandle; var urlP, cacheIndexURLP: UInt8;
                        var sockHP: MemHandle; timeout: Int32; flags: UInt16): Err; syscall inetLibTrapURLOpen;


function INetLibCTPSend(libRefnum: UInt16; inetH: MemHandle; var sockHP: MemHandle;
                        var writeP: UInt8; writelen: UInt32; timeout: Int32; ctpCommand: UInt16): Err; syscall inetLibTrapCTPSend;


function INetLibSockClose(libRefnum: UInt16; socketH: MemHandle): Err; syscall inetLibTrapSockClose;

//--------------------------------------------------
// Read/Write
//--------------------------------------------------

function INetLibSockRead(libRefnum: UInt16; sockH: MemHandle; bufP: Pointer;
                         reqBytes: UInt32; var actBytesP: UInt32; timeout: Int32): Err; syscall inetLibTrapSockRead;

function INetLibSockWrite(libRefnum: UInt16; sockH: MemHandle; bufP: Pointer;
                          reqBytes: UInt32; var actBytesP: UInt32; timeout: Int32): Err; syscall inetLibTrapSockWrite;

//--------------------------------------------------
// Low level Socket calls
//--------------------------------------------------

function INetLibSockOpen(libRefnum: UInt16; inetH: MemHandle;
                         scheme: UInt16 {INetSchemEnum}; var sockHP: MemHandle): Err; syscall inetLibTrapSockOpen;

function INetLibSockStatus(libRefnum: UInt16; socketH: MemHandle; var statusP: UInt16;
                           var sockErrP: Err; var inputReadyP, outputReadyP: Boolean): Err; syscall inetLibTrapSockStatus;


function INetLibSockSettingGet(libRefnum: UInt16; socketH: MemHandle; setting: UInt16 {INetSockSettingEnum};
                               bufP: Pointer; var bufLenP: UInt16): Err; syscall inetLibTrapSockSettingGet;

function INetLibSockSettingSet(libRefnum: UInt16; socketH: MemHandle; setting: UInt16 {INetSockSettingEnum};
                               bufP: Pointer; bufLen: UInt16): Err; syscall inetLibTrapSockSettingSet;


function INetLibSockConnect(libRefnum: UInt16; sockH: MemHandle; var hostnameP: UInt8;
                            port: UInt16; timeou: Int32): Err; syscall inetLibTrapSockConnect;

//--------------------------------------------------
// HTTP specific calls
//--------------------------------------------------

function INetLibSockHTTPReqCreate(libRefnum: UInt16; sockH: MemHandle; var verbP, resNameP, refererP: UInt8): Err; syscall inetLibTrapSockHTTPReqCreate;

function INetLibSockHTTPAttrSet(libRefnum: UInt16; sockH: MemHandle; attr: UInt16 {inetHTTPAttrEnum};
                                attrIndex: UInt16; var bufP: UInt8; bufLen, flags: UInt16): Err; syscall inetLibTrapSockHTTPAttrSet;

function INetLibSockHTTPReqSend(libRefnum: UInt16; sockH: MemHandle; writeP: Pointer;
                                writeLen: UInt32; timeout: Int32): Err; syscall inetLibTrapSockHTTPReqSend;

function INetLibSockHTTPAttrGet(libRefnum: UInt16; sockH: MemHandle; attr: UInt16 {inetHTTPAttrEnum};
                                attrIndex: UInt16; bufP: Pointer; var bufLenP: UInt32): Err; syscall inetLibTrapSockHTTPAttrGet;

//--------------------------------------------------
// Utilities
//--------------------------------------------------

function INetLibURLCrack(libRefnum: UInt16; var urlTextP: UInt8; var urlP: INetURLType): Err; syscall inetLibTrapURLCrack;

function INetLibURLsAdd(libRefnum: UInt16; baseURLStr, embeddedURLStr, resultURLStr: PChar; var resultLenP: UInt16): Err; syscall inetLibTrapURLsAdd;

function INetLibURLsCompare(libRefnum: UInt16; URLStr1, URLStr2: PChar): Int16; syscall inetLibTrapURLsCompare;

function INetLibURLGetInfo(libRefnum: UInt16; inetH: MemHandle; var urlTextP: UInt8; var urlInfoP: INetURLInfoType): Err; syscall inetLibTrapURLGetInfo;

type
  int = SmallInt;

function INetLibWiCmd(refNum: UInt16; cmd: UInt16 {WiCmdEnum}; enableOrX, y: int): Boolean; syscall inetLibTrapWiCmd;

function INetLibWirelessIndicatorCmd(refNum: UInt16; inetH: MemHandle; {WiCmdEnum} cmd: UInt16; enableOrX, y: int): Boolean; syscall inetLibTrapWirelessIndicatorCmd;

function INetLibCheckAntennaState(refNum: UInt16): Err; syscall inetLibTrapCheckAntennaState;

//--------------------------------------------------
// Cache interface
//--------------------------------------------------

function INetLibCacheList(libRefnum: UInt16; inetH: MemHandle; var cacheIndexURLP: UInt8;
                          var indexP: UInt16; var uidP: UInt32; cacheP: INetCacheEntryP): Err; syscall inetLibTrapCacheList;

function INetLibCacheGetObject(libRefnum: UInt16; clientParamH: MemHandle; var urlTextP: UInt8;
                               uniqueID: UInt32; cacheInfoP: INetCacheInfoPtr): Err; syscall inetLibTrapCacheGetObject;

function INetLibCachePurge(libRefnum: UInt16; clientParamH: MemHandle; var urlTextP: UInt8; uniqueID: UInt32): Err; syscall inetLibTrapCachePurge;

function INetLibCacheGetObjectV2(libRefnum: UInt16; clientParamH: MemHandle; var urlTextP: UInt8; uniqueID: UInt32;
                             rcIndex: UInt16; cacheInfoP: INetCacheInfoPtr; cacheEntryP: INetCacheEntryP): Err; syscall inetLibTrapCacheGetObjectV2;

function INetLibIndexedCacheFind(libRefnum: UInt16; cacheDBRef: DmOpenRef; var dataP: UInt8; lookFor: Int16; var indexP: UInt16;
                       order: Int16; var cacheIdP: UInt32): Err; syscall inetLibTrapIndexedCacheFind;

function INetLibPrepareCacheForHistory(libRefnum: UInt16; clientParamH: MemHandle): Err; syscall inetLibTrapPrepareCacheForHistory;

//--------------------------------------------------
// Configuration Calls
//--------------------------------------------------

function INetLibConfigMakeActive(refNum: UInt16; inetH: MemHandle; configIndex: UInt16): Err; syscall inetLibConfigMakeActive_;

function INetLibConfigList(refNum: UInt16; var nameArray{[]}: INetConfigNameType; var arrayEntriesP: UInt16): Err; syscall inetLibConfigList_;

function INetLibConfigIndexFromName(refNum: UInt16; nameP: INetConfigNamePtr; var indexP: UInt16): Err; syscall inetLibConfigIndexFromName_;

function INetLibConfigDelete(refNum: UInt16; index: UInt16): Err; syscall inetLibConfigDelete_;

function INetLibConfigSaveAs(refNum: UInt16; inetH: MemHandle; nameP: INetConfigNamePtr): Err; syscall inetLibConfigSaveAs_;

function INetLibConfigRename(refNum: UInt16; index: UInt16; newNameP: INetConfigNamePtr): Err; syscall inetLibConfigRename_;

function INetLibConfigAliasSet(refNum: UInt16; configIndex, aliasToIndex: UInt16): Err; syscall inetLibConfigAliasSet_;

function INetLibConfigAliasGet(refNum: UInt16; aliasIndex: UInt16; var indexP: UInt16; var isAnotherAliasP: Boolean): Err; syscall inetLibConfigAliasGet_;

//--------------------------------------------------
// File specific calls
//--------------------------------------------------

function INetLibSockFileGetByIndex(libRefnum: UInt16; sockH: MemHandle; index: UInt32;
                                   var  handleP: MemHandle; var offsetP, lengthP: UInt32): Err; syscall inetLibTrapSockFileGetByIndex;

implementation

end.
