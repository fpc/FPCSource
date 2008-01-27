{$MACRO ON}

{$define Rsc := }
(******************************************************************************
 *
 * Copyright (c) 1996-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: NetMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *   This module contains the interface definition for the TCP/IP
 *  library on Pilot.
 *
 * History:
 *    2/14/96  Created by Ron Marianetti
 *       Name  Date     Description
 *       ----  ----     -----------
 *       jrb   3/13/98  Removed NetIFSettings that are Mobitex specific.
 *                      Added RadioStateEnum for the setting.
 *                      Added NetIFSettingSpecificMobitex
 *                      Added what are considered "generic" wirless settings.
 *       jaq   10/1/98  added netMaxIPAddrStrLen constant
 *       scl   3/ 5/99  integrated Eleven's changes into Main
 *       jhl   7/14/00  added net notice
 *
 *****************************************************************************)

unit netmgr;

interface

uses palmos, libtraps, errorbase, sysevent, event_;

(********************************************************************
 * Type and creator of Net Library database
 ********************************************************************)

// Creator. Used for both the database that contains the Net Library and
//  it's preferences database.
const
  netCreator       = Rsc('netl'); // Our Net Library creator

// Feature Creators and numbers, for use with the FtrGet() call. This
//  feature can be obtained to get the current version of the Net Library
  netFtrCreator    = netCreator;
  netFtrNumVersion = 0; // get version of Net Library
                        // 0xMMmfsbbb, where MM is major version, m is minor version
                        // f is bug fix, s is stage: 3-release,2-beta,1-alpha,0-development,
                        // bbb is build number for non-releases
                        // V1.12b3   would be: 0x01122003
                        // V2.00a2   would be: 0x02001002
                        // V1.01     would be: 0x01013000
// Begin Change (BGT)
// Feature for defining the number of command blocks to allocate
  netFtrCommandBlocks = 1; // get the number of command blocks

// Types. Used to identify the Net Library from it's prefs.
  netLibType       = Rsc('libr'); // Our Net Code Resources Database type
  netPrefsType     = Rsc('rsrc'); // Our Net Preferences Database type

// All Network interface's have the following type:
  netIFFileType    = Rsc('neti'); // The filetype of all Network Interfaces

// Each Network interface has a unique creator:
  netIFCreatorLoop = Rsc('loop'); // Loopback network interface creator.
  netIFCreatorSLIP = Rsc('slip'); // SLIP network interface creator.
  netIFCreatorPPP  = Rsc('ppp_'); // PPP network interface creator.
//<chg 1-28-98 RM>
  netIFCreatorRAM  = Rsc('ram_'); // Mobitex network interface creator

// Special value for configIndex parameter to NetLibOpenConfig that tells it
// to use the current settings - even if they are not the defined default settings
// This is provided for testing purposes
  netConfigIndexCurSettings = $FFFF;

// <SCL 3/5/99> Commented out netMaxNetIFs since Tim says it should NOT be here!!
// Still need to fix (Eleven) code that currently depends on it...
// Max # of interfaces that can be installed
// netMaxNetIFs = 4;

//-----------------------------------------------------------------------------
// Misc. constants
//-----------------------------------------------------------------------------

const
  netDrvrTypeNameLen = 8;  // Maximum driver type length
  netDrvrHWNameLen   = 16; // Maximum driver hardware name length
  netIFNameLen       = 10; // Maximum interface name (driver type + instance num)
  netIFMaxHWAddrLen  = 14; // Maximum size of a hardware address
  netMaxIPAddrStrLen = 16; // Max length of an IP address string with null terminator (255.255.255.255)

//-----------------------------------------------------------------------------
// Names of built-in configuration aliases available through the
//  NetLibConfigXXX calls
//-----------------------------------------------------------------------------

  netCfgNameDefault     = '.Default';     // The default configuration
  netCfgNameDefWireline = '.DefWireline'; // The default wireline configuration
  netCfgNameDefWireless = '.DefWireless'; // The default wireless configuration
  netCfgNameCTPWireline = '.CTPWireline'; // Wireline through the Jerry Proxy
  netCfgNameCTPWireless = '.CTPWireless'; // Wireless through the Jerry Proxy

//-----------------------------------------------------------------------------
//Flags for the NetUWirelessAppHandleEvent() utility routine
//-----------------------------------------------------------------------------

const
  netWLAppEventFlagCTPOnly     = $00000001; // using wireless radio for CTP protocol only
  netWLAppEventFlagDisplayErrs = $00000002; // Show error alerts for any errors

//-----------------------------------------------------------------------------
// Option constants that can be passed to NetSocketOptionSet and NetSocketOptionGet
// When an option is set or retrieved, both the level of the option and the
// option number must be specified. The level refers to which layer the option
// refers to, like the uppermost socket layer, for example.
//-----------------------------------------------------------------------------

// Socket level options
type
  NetSocketOptEnum = WordEnum;

const
  // IP Level options
  netSocketOptIPOptions           = 1; // options in IP header (IP_OPTIONS)

  // TCP Level options
  netSocketOptTCPNoDelay          = 1; // don't delay send to coalesce packets
  netSocketOptTCPMaxSeg           = 2; // TCP maximum segment size (TCP_MAXSEG)

  // Socket level options
  netSocketOptSockDebug           = $0001; // turn on debugging info recording
  netSocketOptSockAcceptConn      = $0002; // socket has had listen
  netSocketOptSockReuseAddr       = $0004; // allow local address reuse
  netSocketOptSockKeepAlive       = $0008; // keep connections alive
  netSocketOptSockDontRoute       = $0010; // just use interface addresses
  netSocketOptSockBroadcast       = $0020; // permit sending of broadcast msgs
  netSocketOptSockUseLoopback     = $0040; // bypass hardware when possible
  netSocketOptSockLinger          = $0080; // linger on close if data present
  netSocketOptSockOOBInLine       = $0100; // leave received OutOfBand data in line

  netSocketOptSockSndBufSize      = $1001; // send buffer size
  netSocketOptSockRcvBufSize      = $1002; // receive buffer size
  netSocketOptSockSndLowWater     = $1003; // send low-water mark
  netSocketOptSockRcvLowWater     = $1004; // receive low-water mark
  netSocketOptSockSndTimeout      = $1005; // send timeout
  netSocketOptSockRcvTimeout      = $1006; // receive timeout
  netSocketOptSockErrorStatus     = $1007; // get error status and clear
  netSocketOptSockSocketType      = $1008; // get socket type

  // The following are Pilot specific options
  netSocketOptSockNonBlocking     = $2000; // set non-blocking mode on or off
  netSocketOptSockRequireErrClear = $2001; // return error from all further calls to socket
                                           //  unless  netSocketOptSockErrorStatus is cleared.
  netSocketOptSockMultiPktAddr    = $2002; // for SOCK_RDM (RMP) sockets. This is the
                                           // fixed IP addr (i.e. Mobitex MAN #) to use
                                           //  for multiple packet requests.
  // for socket notification
  // 05/20/00 jhl
  netSocketOptSockNotice          = $2003; // prime socket for notification

// Option levels for SocketOptionSet and SocketOptionGet
type
  NetSocketOptLevelEnum = WordEnum;

const
  netSocketOptLevelIP     = 0;     // IP level options (IPPROTO_IP)
  netSocketOptLevelTCP    = 6;     // TCP level options (IPPROTO_TCP)
  netSocketOptLevelSocket = $FFFF; // Socket level options (SOL_SOCKET)

// Structure used for manipulating the linger option
type
  NetSocketLingerType = record
    onOff: Int16; // option on/off
    time: Int16;  // linger time in seconds
  end;

//-----------------------------------------------------------------------------
// Enumeration of Socket domains and types passed to NetSocketOpen
//-----------------------------------------------------------------------------

type
  NetSocketAddrEnum = Enum;

const
  netSocketAddrRaw  = 0; // (AF_UNSPEC, AF_RAW)
  netSocketAddrINET = 2; // (AF_INET)

type
  NetSocketTypeEnum = Enum;

const
  netSocketTypeStream      = 1; // (SOCK_STREAM)
  netSocketTypeDatagram    = 2; // (SOCK_DGRAM)
  netSocketTypeRaw         = 3; // (SOCK_RAW)
  netSocketTypeReliableMsg = 4; // (SOCK_RDM)
  netSocketTypeLicensee    = 8; // Socket entry reserved for licensees.

// Protocols, passed in the protocol parameter to NetLibSocketOpen
const
  netSocketProtoIPICMP = 1;   // IPPROTO_ICMP
  netSocketProtoIPTCP  = 6;   // IPPROTO_TCP
  netSocketProtoIPUDP  = 17;  // IPPROTO_UDP
  netSocketProtoIPRAW  = 255; // IPPROTO_RAW

//-----------------------------------------------------------------------------
// Enumeration of Socket direction, passed to NetSocketShutdown
//-----------------------------------------------------------------------------

type
  NetSocketDirEnum = Enum;

const
  netSocketDirInput  = 0;
  netSocketDirOutput = 1;
  netSocketDirBoth   = 2;

//-----------------------------------------------------------------------------
// Basic Types
//-----------------------------------------------------------------------------
// Socket refnum

type
  NetSocketRef = Int16;

// Type used to hold internet addresses
  NetIPAddr = UInt32; // a 32-bit IP address.

// IFMediaEvent notifications types
type
  NetLibIFMediaEventNotificationTypeEnum = Enum;

const
  netIFMediaUp = 1; // Usually sent by Network interfaces
                    // after they have displayed the UI for displaying
                    // connection establishment progress.

  netIFMediaDown = Succ(netIFMediaUp);
                    // Sent by Network interface's when their inactivity timer
                    // is ellapsed.

// Notification structure sent in SysNotifyNetLibIFMedia.
type
  SysNotifyNetLibIFMediaTag = record
    eType: NetLibIFMediaEventNotificationTypeEnum;
    ifCreator: UInt32;  // interface creator
    ifInstance: UInt16; // interface instance
  end;
  SysNotifyNetLibIFMediaType = SysNotifyNetLibIFMediaTag;

//-----------------------------------------------------------------------------
// For socket notification
// 05/20/00 jhl
//-----------------------------------------------------------------------------

// Notice types
type
  NoticeTypeEnum = Enum;

const
  netSocketNoticeNotify = 1;
// ummmm...
// shouldn't do this - must fix EventMgr before background/ISR events can be posted
  netSocketNoticeEvent = Succ(netSocketNoticeNotify);
  netSocketNoticeMailbox = Succ(netSocketNoticeEvent);
  netSocketNoticeCallback = Succ(netSocketNoticeMailbox);
  netSocketNoticeWake = Succ(netSocketNoticeCallback);

// Notification structure sent for netSocketNoticeNotify.
type
  SysNotifyNetSocketType = record
    socketRef: NetSocketRef; // Socket sending the notification
    condition: UInt32;       // Bit field reporting trigger conditions
  end;

// Event structure sent for netSocketNoticeEvent.
// This should be defined via Event.h, so it stays in sync.
type
  netSocketNotice = record
    socketRef: NetSocketRef; // Socket sending the notification
    condition: UInt32;       // Bit field reporting trigger conditions
  end;

type
  NetSocketNoticeEventType = record
    eType: eventsEnum; // User specified event type
    penDown: Boolean;
    tapCount: UInt8;
    screenX: Int16;
    screenY: Int16;
    case Integer of
      1: (generic: _GenericEventType); // Establish size of union
      2: (netSocketNotice: netSocketNotice);
  end;

// Mailbox structure sent for netSocketNoticeMailbox.
type
  NetSocketNoticeMailboxType = record
    message_: UInt32;      // User specified message
    reserved: UInt16;
    socketRef: NetSocketRef; // Socket sending the notification
    condition: UInt32;       // Bit field reporting trigger conditions
  end;

// Callback definition for netSocketNoticeCallback.
type
  NetSocketNoticeCallbackPtr = function(userDataP: Pointer; socketRef: UInt16; condition: UInt32): Err;

type
  notify = record
    notifyType: UInt32; // Notification type
                        // sends SysNotifyNetSocketType in notification
  end;

type
  event = record
    eType: eventsEnum; // Event type
                       // adds NetSocketNoticeEventType event to UI event queue
  end;

type
  mailbox = record
    mailboxID: UInt32; // ID of mailbox for send
    message_: UInt32;  // first element of mailbox message
    wAck: UInt32;      // third argument to SysMailboxSend()
                       // sends NetSocketNoticeMailboxType message to specified mailboxID
  end;

type
  callback = record
    callbackP: NetSocketNoticeCallbackPtr; // Callback proc pointer
    userDataP: Pointer;                    // User specified ptr passed as callback parameter
    // (*callbackP)(userDataP,socketRef,condition)
  end;

type
  wake = record
    taskID: UInt32;            // ID of task to wake
    socketRefP: ^NetSocketRef; // address to receive socketRef
    conditionP: ^UInt32;       // address to receive trigger condition
  end;

// Structure used to register for a notice
type
  NetSocketNoticeType = record
    condition: UInt32;     // Bit field specifying trigger conditions
    type_: NoticeTypeEnum; // Notice type
    case Integer of
      1: (notify: notify);
// ummmm...
// shouldn't do this - must fix EventMgr before background/ISR events can be posted
      2: (event: event);
      3: (mailbox: mailbox);
      4: (callback: callback);
      5: (wake: wake); // SysTaskWake(taskID)
  end;

// Bit values for specifying and reporting trigger conditions
const
  netSocketNoticeErr             = $00000001;
  netSocketNoticeUDPReceive      = $00000002;
  netSocketNoticeTCPReceive      = $00000004;
  netSocketNoticeTCPTransmit     = $00000008;
  netSocketNoticeTCPRemoteClosed = $00000010;
  netSocketNoticeTCPClosed       = $00000020;
  netSocketNoticeConnectInbound  = $00000040;
  netSocketNoticeConnectOutbound = $00000080;

//-----------------------------------------------------------------------------
// Structure used to hold an internet socket address. This includes the internet
//  address and the port number. This structure directly maps to the BSD unix
//  struct sockaddr_in.
//-----------------------------------------------------------------------------

type
  NetSocketAddrINType = record
    family: Int16;   // Address family in HBO (Host UInt8 Order)
    port: UInt16;    // the UDP port in NBO (Network UInt8 Order)
    addr: NetIPAddr; // IP address in NBO (Network UInt8 Order)
  end;

// Constant that means "use the local machine's IP address"
const
  netIPAddrLocal = 0; // Can be used in NetSockAddrINType.addr

// Structure used to hold a generic socket address. This is a generic struct
// designed to hold any type of address including internet addresses. This
// structure directly maps to the BSD unix struct sockaddr.

type
  NetSocketAddrType = record
    family: Int16;                   // Address family
    data: array [0..14-1] of UInt8;  // 14 bytes of address
  end;
  NetSocketAddrPtr = ^NetSocketAddrType;

// Structure used to hold a raw socket address. When using the netSocketAddrRaw
//  protocol family, the caller must bind() the socket to an interface and
//  specifies the interface using this structure. IMPORTANT: NUMEROUS
//  ROUTINES IN NETLIB RELY ON THE FACT THAT THIS STRUCTURE IS THE SAME
//  SIZE AS A NetSocketAddrINType STRUCTURE.

type
  NetSocketAddrRawType = record
    family: Int16;      // Address family in HBO (Host UInt8 Order)
    ifInstance: UInt16; // the interface instance number
    ifCreator: UInt32;  // the interface creator
  end;

//-----------------------------------------------------------------------------
// Structure used to hold information about data to be sent. This structure
//  is passed to NetLibSendMsg and contains the optional address to send to,
//  a scatter-write array of data to be sent, and optional access rights
//-----------------------------------------------------------------------------

// Scatter/Gather array type. A pointer to an array of these structs is
//  passed to the NetLibSendPB and NetLibRecvPB calls. It specifies where
//  data should go to or come from as a list of buffer addresses and sizes.

type
  NetIOVecType = record
    bufP: ^UInt8;   // buffer address
    bufLen: UInt16; // buffer length
  end;

  NetIOVecPtr = ^NetIOVecType;

const
  netIOVecMaxLen = 16; // max# of NetIOVecTypes in an array

// Read/Write ParamBlock type. Passed directly to the SendPB and RecvPB calls.
type
  NetIOParamType = record
    addrP: ^UInt8;           // address - or 0 for default
    addrLen: UInt16;         // length of address
    iov: NetIOVecPtr;        // scatter/gather array
    iovLen: UInt16;          // length of above array
    accessRights: ^UInt8;    // access rights
    accessRightsLen: UInt16; // length of accessrights
  end;

  NetIOParamPtr = ^NetIOParamType;

// Flags values for the NetLibSend, NetLibReceive calls
const
  netIOFlagOutOfBand = $01; // process out-of-band data
  netIOFlagPeek      = $02; // peek at incoming message
  netIOFlagDontRoute = $04; // send without using routing

//-----------------------------------------------------------------------------
// Structures used for looking up a host by name or address (NetLibGetHostByName)
//-----------------------------------------------------------------------------

// Equates for DNS names, from RFC-1035
  netDNSMaxDomainName  = 255;
  netDNSMaxDomainLabel = 63;

  netDNSMaxAliases     = 1; // max # of aliases for a host
  netDNSMaxAddresses   = 4; // max # of addresses for a host

// The actual results of NetLibGetHostByName() are returned in this structure.
// This structure is designed to match the "struct hostent" structure in Unix.

type
  NetHostInfoType = record
    nameP: PChar;         // official name of host
    nameAliasesP: ^PChar; // array of alias's for the name
    addrType: UInt16;     // address type of return addresses
    addrLen: UInt16;      // the length, in bytes, of the addresses
                          // Note this denotes length of a address, not # of addresses.
    addrListP: ^UInt8Ptr; // array of ptrs to addresses in HBO
  end;

  NetHostInfoPtr = ^NetHostInfoType;

// "Buffer" passed to call as a place to store the results
  NetHostInfoBufType = record
    hostInfo: NetHostInfoType; // high level results of call are here

    // The following fields contain the variable length data that
    //  hostInfo points to
    name: array [0..netDNSMaxDomainName] of Char; // hostInfo->name

    aliasList: array [0..netDNSMaxAliases] of PChar; // +1 for 0 termination.
    aliases: array [0..netDNSMaxAliases-1, 0..netDNSMaxDomainName] of Char;

    addressList: array [0..netDNSMaxAddresses-1] of ^NetIPAddr;
    address: array [0..netDNSMaxAddresses-1] of NetIPAddr;
  end;

  NetHostInfoBufPtr = ^NetHostInfoBufType;

//-----------------------------------------------------------------------------
// Structures used for looking up a service (NetLibGetServByName)
//-----------------------------------------------------------------------------

// Equates for service names
const
  netServMaxName    = 15; // max # characters in service name
  netProtoMaxName   = 15; // max # characters in protocol name
  netServMaxAliases = 1;  // max # of aliases for a service

// The actual results of NetLibGetServByName() are returned in this structure.
// This structure is designed to match the "struct servent" structure in Unix.

type
  NetServInfoType = record
    nameP: PChar;         // official name of service
    nameAliasesP: ^PChar; // array of alias's for the name
    port: UInt16;         // port number for this service
    protoP: PChar;        // name of protocol to use
  end;

  NetServInfoPtr = ^NetServInfoType;

// "Buffer" passed to call as a place to store the results
  NetServInfoBufType = record
    servInfo: NetServInfoType; // high level results of call are here

    // The following fields contain the variable length data that
    //  servInfo points to
    name: array [0..netServMaxName] of Char; // hostInfo->name

    aliasList: array [0..netServMaxAliases] of PChar; // +1 for 0 termination.
    aliases: array [0..netServMaxAliases-1, 0..netServMaxName-1] of Char;
    protoName: array [0..netProtoMaxName] of Char;

    reserved: UInt8;
  end;

  NetServInfoBufPtr = ^NetServInfoBufType;

//--------------------------------------------------------------------
// Structure of a configuration name. Used by NetLibConfigXXX calls
// <chg 1-28-98 RM> added for the new Config calls.
//---------------------------------------------------------------------
const
  netConfigNameSize = 32;

type
  NetConfigNameType = record
    name: array [0..netConfigNameSize-1] of Char; // name of configuration
  end;

  NetConfigNamePtr = ^NetConfigNameType;

(********************************************************************
 * Tracing Flags. These flags are ORed together and passed as a UInt32
 *  in the netSettingTraceFlags setting and netIFSettingTraceFlags to
 *  enable/disable various trace options.
 ********************************************************************)

const
  netTracingErrors    = $00000001; // record errors
  netTracingMsgs      = $00000002; // record messages
  netTracingPktIP     = $00000004; // record packets sent/received
                                   //  to/from interfaces at the IP layer
                                   // NOTE:  netTracingPktData40 & netTracingPktData
                                   //  will control how much data of each packet is
                                   //  recorded.
  netTracingFuncs     = $00000008; // record function flow
  netTracingAppMsgs   = $00000010; // record application messages
                                   // (NetLibTracePrintF, NetLibTracePutS)
  netTracingPktData40 = $00000020; // record first 40 bytes of packets
                                   //  when netTracingPktsXX is also on.
                                   // NOTE: Mutually exclusive with
                                   //  netTracingPktData and only applicable if
                                   //  one of the netTracingPktsXX bits is also set
  netTracingPktData   = $00000040; // record all bytes of IP packets
                                   //  sent/received to/from interfaces
                                   // NOTE: Mutually exclusive with
                                   //  netTracingPkts & netTracingPktData64
  netTracingPktIFHi   = $00000080; // record packets sent/received at highest layer
                                   //  of interface (just below IP layer).
                                   // NOTE:  netTracingPktData40 & netTracingPktData
                                   //  will control how much data of each packet is
                                   //  recorded.
  netTracingPktIFMid  = $00000100; // record packets sent/received at mid layer
                                   //  of interface (just below IFHi layer).
                                   // NOTE:  netTracingPktData40 & netTracingPktData
                                   //  will control how much data of each packet is
                                   //  recorded.
  netTracingPktIFLow  = $00000200; // record packets sent/received at low layer
                                   //  of interface (just below IFMid layer).
                                   // NOTE:  netTracingPktData40 & netTracingPktData
                                   //  will control how much data of each packet is
                                   //  recorded.

// OBSOLETE tracing bit, still used by Network Panel
  netTracingPkts      = netTracingPktIP;

(********************************************************************
 * Command numbers and parameter blocks for the NetLibMaster() call.
 * This call is used to put the Net library into certain debugging modes
 *      or for obtaining statistics from the Net Library.
 *
 ********************************************************************)

type
  NetMasterEnum = Enum;

const
  // These calls return info
  netMasterInterfaceInfo = 0;
  netMasterInterfaceStats = Succ(netMasterInterfaceInfo);
  netMasterIPStats = Succ(netMasterInterfaceStats);
  netMasterICMPStats = Succ(netMasterIPStats);
  netMasterUDPStats = Succ(netMasterICMPStats);
  netMasterTCPStats = Succ(netMasterUDPStats);

  // This call used to read the trace buffer.
  netMasterTraceEventGet = Succ(netMasterTCPStats);  // get trace event by index

type

  //.............................................................
  // InterfaceInfo command
  //.............................................................

  interfaceInfo = record
    index: UInt16;    // -> index of interface
    creator: UInt32;  // <- creator
    instance: UInt16; // <- instance
    netIFP: Pointer;  // <- net_if pointer

    // driver level info
    drvrName: array [0..netDrvrTypeNameLen-1] of Char; // <- type of driver (SLIP,PPP, etc)
    hwName: array [0..netDrvrHWNameLen-1] of Char;     // <- hardware name (Serial Library, etc)
    localNetHdrLen: UInt8;     // <- local net header length
    localNetTrailerLen: UInt8; // <- local net trailer length
    localNetMaxFrame: UInt16;  // <- local net maximum frame size

    // media layer info
    ifName: array [0..netIFNameLen-1] of Char; // <- interface name w/instance
    driverUp: Boolean; // <- true if interface driver up
    ifUp: Boolean;     // <- true if interface is up
    hwAddrLen: UInt16; // <- length of hardware address
    hwAddr: array [0..netIFMaxHWAddrLen-1] of UInt8;      // <- hardware address
    mtu: UInt16;  // <- maximum transfer unit of interface
    speed: UInt32; // <- speed in bits/sec.
    lastStateChange: UInt32; // <- time in milliseconds of last state change

    // Address info
    ipAddr: NetIPAddr;     // Address of this interface
    subnetMask: NetIPAddr; // subnet mask of local network
    broadcast: NetIPAddr;  // broadcast address of local network
  end;

  //.............................................................
  // InterfaceStats command
  //.............................................................

  interfaceStats = record
    index: UInt16;           // -> index of interface
    inOctets: UInt32;        // <- ....
    inUcastPkts: UInt32;
    inNUcastPkts: UInt32;
    inDiscards: UInt32;
    inErrors: UInt32;
    inUnknownProtos: UInt32;
    outOctets: UInt32;
    outUcastPkts: UInt32;
    outNUcastPkts: UInt32;
    outDiscards: UInt32;
    outErrors: UInt32;
  end;

  //.............................................................
  // IPStats command
  //.............................................................

  ipStats = record
    ipInReceives: UInt32;
    ipInHdrErrors: UInt32;
    ipInAddrErrors: UInt32;
    ipForwDatagrams: UInt32;
    ipInUnknownProtos: UInt32;
    ipInDiscards: UInt32;
    ipInDelivers: UInt32;
    ipOutRequests: UInt32;
    ipOutDiscards: UInt32;
    ipOutNoRoutes: UInt32;
    ipReasmReqds: UInt32;
    ipReasmOKs: UInt32;
    ipReasmFails: UInt32;
    ipFragOKs: UInt32;
    ipFragFails: UInt32;
    ipFragCreates: UInt32;
    ipRoutingDiscards: UInt32;
    ipDefaultTTL: UInt32;
    ipReasmTimeout: UInt32;
  end;

  //.............................................................
  // ICMPStats command
  //.............................................................

  icmpStats = record
    icmpInMsgs: UInt32;
    icmpInErrors: UInt32;
    icmpInDestUnreachs: UInt32;
    icmpInTimeExcds: UInt32;
    icmpInParmProbs: UInt32;
    icmpInSrcQuenchs: UInt32;
    icmpInRedirects: UInt32;
    icmpInEchos: UInt32;
    icmpInEchoReps: UInt32;
    icmpInTimestamps: UInt32;
    icmpInTimestampReps: UInt32;
    icmpInAddrMasks: UInt32;
    icmpInAddrMaskReps: UInt32;
    icmpOutMsgs: UInt32;
    icmpOutErrors: UInt32;
    icmpOutDestUnreachs: UInt32;
    icmpOutTimeExcds: UInt32;
    icmpOutParmProbs: UInt32;
    icmpOutSrcQuenchs: UInt32;
    icmpOutRedirects: UInt32;
    icmpOutEchos: UInt32;
    icmpOutEchoReps: UInt32;
    icmpOutTimestamps: UInt32;
    icmpOutTimestampReps: UInt32;
    icmpOutAddrMasks: UInt32;
    icmpOutAddrMaskReps: UInt32;
  end;

  //.............................................................
  // UDPStats command
  //.............................................................

  udpStats = record
    udpInDatagrams: UInt32;
    udpNoPorts: UInt32;
    udpInErrors: UInt32;
    udpOutDatagrams: UInt32;
  end;

  //.............................................................
  // TCPStats command
  //.............................................................

  tcpStats = record
    tcpRtoAlgorithm: UInt32;
    tcpRtoMin: UInt32;
    tcpRtoMax: UInt32;
    tcpMaxConn: UInt32;
    tcpActiveOpens: UInt32;
    tcpPassiveOpens: UInt32;
    tcpAttemptFails: UInt32;
    tcpEstabResets: UInt32;
    tcpCurrEstab: UInt32;
    tcpInSegs: UInt32;
    tcpOutSegs: UInt32;
    tcpRetransSegs: UInt32;
    tcpInErrs: UInt32;
    tcpOutRsts: UInt32;
  end;

  //.............................................................
  // TraceEventGet command
  //.............................................................

  traceEventGet = record
    index: UInt16; // which event
    textP: PChar;  // ptr to text string to return it in
  end;

type
  NetMasterPBType = record
    // These fields are specific to each command
    case Integer of
      0: (interfaceInfo: interfaceInfo);
      1: (interfaceStats: interfaceStats);
      2: (ipStats: ipStats);
      3: (icmpStats: icmpStats);
      4: (udpStats: udpStats);
      5: (tcpStats: tcpStats);
      6: (traceEventGet: traceEventGet);
  end;

  NetMasterPBPtr = ^NetMasterPBType;

//-----------------------------------------------------------------------------
// Enumeration of Net settings as passed to NetLibSettingGet/Set.
//-----------------------------------------------------------------------------

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Global environment settings common to all attached network interfaces,
//   passed to NetLibSettingGet/Set
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

type
  NetSettingEnum = WordEnum;

const
  netSettingResetAll = 0;                                         // void, NetLibSettingSet only, resets all settings
                                                                  //  to their defaults.

  netSettingPrimaryDNS = Succ(netSettingResetAll);                // UInt32, IP address of Primary DN Server
  netSettingSecondaryDNS = Succ(netSettingPrimaryDNS);            // UInt32, IP address of Secondary DN Server
  netSettingDefaultRouter = Succ(netSettingSecondaryDNS);         // UInt32, IP address of Default router
  netSettingDefaultIFCreator = Succ(netSettingDefaultRouter);     // UInt32, Creator type of default interface
  netSettingDefaultIFInstance = Succ(netSettingDefaultIFCreator); // UInt16, Instance# of default interface
  netSettingHostName = Succ(netSettingDefaultIFInstance);         // Char[64], name of host (not including domain)
  netSettingDomainName = Succ(netSettingHostName);                // Char[256], domain name of hosts's domain
  netSettingHostTbl = Succ(netSettingDomainName);                 // Char[], host table
  netSettingCloseWaitTime = Succ(netSettingHostTbl);              // UInt32, time in milliseconds to stay in close-wait state
  netSettingInitialTCPResendTime = Succ(netSettingCloseWaitTime); // UInt32, time in milliseconds before TCP resends a packet.
                                                                  //  This is just the initial value, the timeout is adjusted
                                                                  //  from this initial value depending on history of ACK times.
                                                                  //  This is sometimes referred to as the RTO (Roundtrip Time Out)
                                                                  //  See RFC-1122 for additional information.

  // The following settings are not used for configuration, but rather put the
  //  stack into various modes for debugging, etc.
  netSettingTraceBits = $1000;                                    // UInt32, enable/disable various trace flags (netTraceBitXXXX)
  netSettingTraceSize = Succ(netSettingTraceBits);                // UInt32, max trace buffer size in bytes. Default 0x800.
                                                                  //  Setting this will also clear the trace buffer.
  netSettingTraceStart = Succ(netSettingTraceSize);               // UInt32, for internal use ONLY!!
  netSettingTraceRoll = Succ(netSettingTraceStart);               // UInt8, if true, trace buffer will rollover after it fills.
                                                                  //  Default is true.

  netSettingRTPrimaryDNS = Succ(netSettingTraceRoll);             // used internally by Network interfaces
                                                                  //  that dynamically obtain the DNS address
  netSettingRTSecondaryDNS = Succ(netSettingRTPrimaryDNS);        // used internally by Network interfaces
                                                                  //  that dynamically obtain the DNS address

  netSettingConfigTable = Succ(netSettingRTSecondaryDNS);         // used internally by NetLib - NOT FOR USE BY
                                                                  //  APPLICATIONS!!

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Settings for each Network Interface, passed to NetLibIFSettingGet/Set
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

type
  NetIFSettingEnum = WordEnum;

const

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Reset all settings to defaults
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    netIFSettingResetAll = 0;                                               // void, NetLibIFSettingSet only, resets all settings
                                                                            //  to their defaults.

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Status - read only
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    netIFSettingUp = Succ(netIFSettingResetAll);                            // UInt8, true if interface is UP.
    netIFSettingName = Succ(netIFSettingUp);                                // Char[32], name of interface

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Addressing
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    netIFSettingReqIPAddr = Succ(netIFSettingName);                         // UInt32, requested IP address of this interface
    netIFSettingSubnetMask = Succ(netIFSettingReqIPAddr);                   // UInt32, subnet mask of this interface
    netIFSettingBroadcast = Succ(netIFSettingSubnetMask);                   // UInt32, broadcast address for this interface

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // User Info
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    netIFSettingUsername = Succ(netIFSettingBroadcast);                     // Char[], login script user name
                                                                            //               If 0 length, then user will be prompted for it
    netIFSettingPassword = Succ(netIFSettingUsername);                      // Char[], login script user password
                                                                            //               If 0 length, then user will be prompted for it
    netIFSettingDialbackUsername = Succ(netIFSettingPassword);              // Char[], login script dialback user name.
                                                                            //               If 0 length, then netIFSettingUsername is used
    netIFSettingDialbackPassword = Succ(netIFSettingDialbackUsername);      // Char[], login script dialback user password.
                                                                            //               If 0 length, then user will be prompted for it
    netIFSettingAuthUsername = Succ(netIFSettingDialbackPassword);          // Char[], PAP/CHAP name.
                                                                            //               If 0 length, then netIFSettingUsername is used
    netIFSettingAuthPassword = Succ(netIFSettingAuthUsername);              // Char[], PAP/CHAP password.
                                                                            //               If "$", then user will be prompted for it
                                                                            //               else If 0 length, then netIFSettingPassword or result
                                                                            //                  of it's prompt (if it was empty) will be used
                                                                            //               else it is used as-is.

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Connect Settings
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    netIFSettingServiceName = Succ(netIFSettingAuthPassword);               // Char[], name of service
    netIFSettingLoginScript = Succ(netIFSettingServiceName);                // Char[], login script
    netIFSettingConnectLog = Succ(netIFSettingLoginScript);                 // Char[], connect log
    netIFSettingInactivityTimeout = Succ(netIFSettingConnectLog);           // UInt16, # of seconds of inactivity allowed before
                                                                            //  interface is brought down. If 0 then
                                                                            //  no inactivity timeout enforced.
    netIFSettingEstablishmentTimeout = Succ(netIFSettingInactivityTimeout); // UInt16, max delay in seconds between connection
                                                                            //  establishment stages

    // Serial based protocol options
    netIFSettingDynamicIP = Succ(netIFSettingEstablishmentTimeout);         // UInt8, if true, get IP address from server
                                                                            //  N/A for SLIP
    netIFSettingVJCompEnable = Succ(netIFSettingDynamicIP);                 // UInt8, if true enable VJ Header compression
                                                                            //  Default is on for PPP, off for SLIP
    netIFSettingVJCompSlots = Succ(netIFSettingVJCompEnable);               // UInt8, # of slots to use for VJ compression.
                                                                            //  Default is 4 for PPP, 16 for SLIP
                                                                            //  (each slot uses 256 bytes of RAM).
    netIFSettingMTU = Succ(netIFSettingVJCompSlots);                        // UInt16, maximum transmission unit in bytes
                                                                            //  ignored in current PPP and SLIP interfaces
    netIFSettingAsyncCtlMap = Succ(netIFSettingMTU);                        // UInt32, bitmask of characters to escape
                                                                            //  ignored in current PPP interfaces

    // Serial settings, used by serial based network interfaces
    netIFSettingPortNum = Succ(netIFSettingAsyncCtlMap);                    // UInt16, port number to use
    netIFSettingBaudRate = Succ(netIFSettingPortNum);                       // UInt32, baud rate in bits/sec.
    netIFSettingFlowControl = Succ(netIFSettingBaudRate);                   // UInt8, flow control setting bits. Set to 0x01 for
                                                                            //   hardware flow control, else set to 0x00.
    netIFSettingStopBits = Succ(netIFSettingFlowControl);                   // UInt8, # of stop bits
    netIFSettingParityOn = Succ(netIFSettingStopBits);                      // UInt8, true if parity on
    netIFSettingParityEven = Succ(netIFSettingParityOn);                    // UInt8, true if parity even

    // Modem settings, optionally used by serial based network interfaces
    netIFSettingUseModem = Succ(netIFSettingParityEven);                    // UInt8, if true dial-up through modem
    netIFSettingPulseDial = Succ(netIFSettingUseModem);                     // UInt8, if true use pulse dial, else tone
    netIFSettingModemInit = Succ(netIFSettingPulseDial);                    // Char[], modem initialization string
    netIFSettingModemPhone = Succ(netIFSettingModemInit);                   // Char[], modem phone number string
    netIFSettingRedialCount = Succ(netIFSettingModemPhone);                 // UInt16, # of times to redial

    //---------------------------------------------------------------------------------
    // New Settings as of PalmOS 3.0
    // Power control, usually only implemented by wireless interfaces
    //---------------------------------------------------------------------------------
    netIFSettingPowerUp = Succ(netIFSettingRedialCount);                    // UInt8, true if this interface is powered up
                                                                            //       false if this interface is in power-down mode
                                                                            //  interfaces that don't support power modes should
                                                                            //  quietly ignore this setting.

    // Wireless or Wireline, read-only, returns true for wireless interfaces. this
    //  setting is used by application level functions to determine which interface(s)
    //  to attach/detach given user preference and/or state of the antenna.
    netIFSettingWireless = Succ(netIFSettingPowerUp);                       // UInt8, true if this interface is wireless

    // Option to query server for address of DNS servers
    netIFSettingDNSQuery = Succ(netIFSettingWireless);                      // UInt8, if true PPP queries for DNS address. Default true

    //---------------------------------------------------------------------------------
    // New Settings as of PalmOS 3.2
    // Power control, usually only implemented by wireless interfaces
    //---------------------------------------------------------------------------------

    netIFSettingQuitOnTxFail = Succ(netIFSettingDNSQuery);                  // BYTE  W-only. Power down RF on tx fail
    netIFSettingQueueSize = Succ(netIFSettingQuitOnTxFail);                 // UInt8  R-only. The size of the Tx queue in the RF interface
    netIFSettingTxInQueue = Succ(netIFSettingQueueSize);                    // BYTE  R-only. Packets remaining to be sent
    netIFSettingTxSent = Succ(netIFSettingTxInQueue);                       // BYTE  R-only. Packets sent since SocketOpen
    netIFSettingTxDiscard = Succ(netIFSettingTxSent);                       // BYTE  R-only. Packets discarded on SocketClose
    netIFSettingRssi = Succ(netIFSettingTxDiscard);                         // char  R-only. signed value in dBm.
    netIFSettingRssiAsPercent = Succ(netIFSettingRssi);                     // char  R-only. signed value in percent, with 0 being no coverage and 100 being excellent.
    netIFSettingRadioState = Succ(netIFSettingRssiAsPercent);               // enum  R-only. current state of the radio
    netIFSettingBase = Succ(netIFSettingRadioState);                        // UInt32 R-only. Interface specific
    netIFSettingRadioID = Succ(netIFSettingBase);                           // UInt32[2] R-only, two 32-bit. interface specific
    netIFSettingBattery = Succ(netIFSettingRadioID);                        // UInt8, R-only. percentage of battery left
    netIFSettingNetworkLoad = Succ(netIFSettingBattery);                    // UInt8, R-only. percent estimate of network loading

    //---------------------------------------------------------------------------------
    // New Settings as of PalmOS 3.3
    //---------------------------------------------------------------------------------

    netIFSettingConnectionName = Succ(netIFSettingNetworkLoad);             // Char [] Connection Profile Name

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // The following settings are not used for configuration, but rather put the
    //  stack into various modes for debugging, etc.
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    netIFSettingTraceBits = $1000;     // UInt32, enable/disable various trace flags (netTraceBitXXXX)
    netIFSettingGlobalsPtr = Succ(netIFSettingTraceBits);                   // UInt32, (Read-Only) sinterface's globals ptr
    netIFSettingActualIPAddr = Succ(netIFSettingGlobalsPtr);                // UInt32, (Read-Only) the actual IP address that the interface
                                                                            //   ends up using. The login script executor stores
                                                                            //   the result of the "g" script command here as does
                                                                            //   the PPP negotiations.
    netIFSettingServerIPAddr = Succ(netIFSettingActualIPAddr);              // UInt32, (Read-Only) the IP address of the PPP server
                                                                            //  we're connected to

    // The following setting should be true if this network interface should be
    // brought down when the Pilot is turned off.
    netIFSettingBringDownOnPowerDown = Succ(netIFSettingServerIPAddr);      // UInt8, if true interface will be brought down when
                                                                            //  Pilot is turned off.

    // The following setting is used by the TCP/IP stack ONLY!! It tells the interface
    //  to pass all received packets as-is to the NetIFCallbacksPtr->raw_rcv() routine.
    //  This setting gets setup when an application creates a raw socket in the raw domain
    netIFSettingRawMode = Succ(netIFSettingBringDownOnPowerDown);           // UInt32, parameter to pass to raw_rcv() along with
                                                                            //  packet pointer.

    //---------------------------------------------------------------------------------
    // New Settings as of PalmOS 4.0
    //---------------------------------------------------------------------------------

    // The following setting is a new interface in PalmOS 4.0 that allow INetlib
    // or other NetLib clients to get raw location information as described in
    // PalmLocRawData.h.
    // NetLib will return a pointer to a newly allocated memory buffer containing
    // the raw location information to send to Elaine (Web Clipping proxy server).
    // Elaine will then use a Windows DLL to analyse the raw location information
    // in order to transform it into something useful like zipcode, cityname, etc.
    // See PalmLocRawData.h for more details...
    netIFSettingLocRawInfo = Succ(netIFSettingRawMode);                     // void* R-only: Allocated memory buffer - must be free by caller

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // 3rd party settings start here...
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    netIFSettingCustom = $8000;

//=========================================================================================
// Enums for the netIFSettingRadioState setting
//
// JB added for the radio state setting.
// <chg 3-17-98 RM> fixed naming conventions.
//=========================================================================================

type
  NetRadioStateEnum = Enum;

const
  netRadioStateOffNotConnected = 0;
  netRadioStateOnNotConnected = Succ(netRadioStateOffNotConnected); // scanning
  netRadioStateOnConnected = Succ(netRadioStateOnNotConnected);     // have channel
  netRadioStateOffConnected = Succ(netRadioStateOnConnected);

(************************************************************
 * Net Library Macros
 *************************************************************)

// Return current time in milliseconds.
function NetNow: UInt32;

// File Descriptor macros used for the NetLibSelect() call
type
  NetFDSetType = UInt32;
  NetFDSetPtr = ^NetFDSetType;

const
  netFDSetSize = 32;

procedure netFDSet(n: UInt8; var p: NetFDSetType);
procedure nnetFDClr(n: UInt8; var p: NetFDSetType);
function nnetFDIsSet(n: UInt8; var p: NetFDSetType): Boolean;
procedure nnetFDZero(var p: NetFDSetType);

//-----------------------------------------------------------------------------
// Net error codes
//-----------------------------------------------------------------------------

const
  netErrAlreadyOpen                = netErrorClass or 1;
  netErrNotOpen                    = netErrorClass or 2;
  netErrStillOpen                  = netErrorClass or 3;
  netErrParamErr                   = netErrorClass or 4;
  netErrNoMoreSockets              = netErrorClass or 5;
  netErrOutOfResources             = netErrorClass or 6;
  netErrOutOfMemory                = netErrorClass or 7;  // Might be because free heap space is <32K probably because handheld's RAM <2MB
  netErrSocketNotOpen              = netErrorClass or 8;
  netErrSocketBusy                 = netErrorClass or 9;  //EINPROGRESS
  netErrMessageTooBig              = netErrorClass or 10;
  netErrSocketNotConnected         = netErrorClass or 11;
  netErrNoInterfaces               = netErrorClass or 12; //ENETUNREACH
  netErrBufTooSmall                = netErrorClass or 13;
  netErrUnimplemented              = netErrorClass or 14;
  netErrPortInUse                  = netErrorClass or 15; //EADDRINUSE
  netErrQuietTimeNotElapsed        = netErrorClass or 16; //EADDRINUSE
  netErrInternal                   = netErrorClass or 17;
  netErrTimeout                    = netErrorClass or 18; //ETIMEDOUT
  netErrSocketAlreadyConnected     = netErrorClass or 19; //EISCONN
  netErrSocketClosedByRemote       = netErrorClass or 20;
  netErrOutOfCmdBlocks             = netErrorClass or 21;
  netErrWrongSocketType            = netErrorClass or 22;
  netErrSocketNotListening         = netErrorClass or 23;
  netErrUnknownSetting             = netErrorClass or 24;
  netErrInvalidSettingSize         = netErrorClass or 25;
  netErrPrefNotFound               = netErrorClass or 26;
  netErrInvalidInterface           = netErrorClass or 27;
  netErrInterfaceNotFound          = netErrorClass or 28;
  netErrTooManyInterfaces          = netErrorClass or 29;
  netErrBufWrongSize               = netErrorClass or 30;
  netErrUserCancel                 = netErrorClass or 31;
  netErrBadScript                  = netErrorClass or 32;
  netErrNoSocket                   = netErrorClass or 33;
  netErrSocketRcvBufFull           = netErrorClass or 34;
  netErrNoPendingConnect           = netErrorClass or 35;
  netErrUnexpectedCmd              = netErrorClass or 36;
  netErrNoTCB                      = netErrorClass or 37;
  netErrNilRemoteWindowSize        = netErrorClass or 38;
  netErrNoTimerProc                = netErrorClass or 39;
  netErrSocketInputShutdown        = netErrorClass or 40; // EOF to sockets API
  netErrCmdBlockNotCheckedOut      = netErrorClass or 41;
  netErrCmdNotDone                 = netErrorClass or 42;
  netErrUnknownProtocol            = netErrorClass or 43;
  netErrUnknownService             = netErrorClass or 44;
  netErrUnreachableDest            = netErrorClass or 45;
  netErrReadOnlySetting            = netErrorClass or 46;
  netErrWouldBlock                 = netErrorClass or 47; //EWOULDBLOCK
  netErrAlreadyInProgress          = netErrorClass or 48; //EALREADY
  netErrPPPTimeout                 = netErrorClass or 49;
  netErrPPPBroughtDown             = netErrorClass or 50;
  netErrAuthFailure                = netErrorClass or 51;
  netErrPPPAddressRefused          = netErrorClass or 52;
// The following map into the Epilogue DNS errors declared in DNS.ep.h:
//  and MUST be kept in this order!!
  netErrDNSNameTooLong             = netErrorClass or 53;
  netErrDNSBadName                 = netErrorClass or 54;
  netErrDNSBadArgs                 = netErrorClass or 55;
  netErrDNSLabelTooLong            = netErrorClass or 56;
  netErrDNSAllocationFailure       = netErrorClass or 57;
  netErrDNSTimeout                 = netErrorClass or 58;
  netErrDNSUnreachable             = netErrorClass or 59;
  netErrDNSFormat                  = netErrorClass or 60;
  netErrDNSServerFailure           = netErrorClass or 61;
  netErrDNSNonexistantName         = netErrorClass or 62;
  netErrDNSNIY                     = netErrorClass or 63;
  netErrDNSRefused                 = netErrorClass or 64;
  netErrDNSImpossible              = netErrorClass or 65;
  netErrDNSNoRRS                   = netErrorClass or 66;
  netErrDNSAborted                 = netErrorClass or 67;
  netErrDNSBadProtocol             = netErrorClass or 68;
  netErrDNSTruncated               = netErrorClass or 69;
  netErrDNSNoRecursion             = netErrorClass or 70;
  netErrDNSIrrelevant              = netErrorClass or 71;
  netErrDNSNotInLocalCache         = netErrorClass or 72;
  netErrDNSNoPort                  = netErrorClass or 73;
// The following map into the Epilogue IP errors declared in IP.ep.h:
//  and MUST be kept in this order!!
  netErrIPCantFragment             = netErrorClass or 74;
  netErrIPNoRoute                  = netErrorClass or 75;
  netErrIPNoSrc                    = netErrorClass or 76;
  netErrIPNoDst                    = netErrorClass or 77;
  netErrIPktOverflow               = netErrorClass or 78;
// End of Epilogue IP errors
  netErrTooManyTCPConnections      = netErrorClass or 79;
  netErrNoDNSServers               = netErrorClass or 80;
  netErrInterfaceDown              = netErrorClass or 81;

// Mobitex network radio interface error code returns
  netErrNoChannel                  = netErrorClass or 82; // The datalink layer cannot acquire a channel
  netErrDieState                   = netErrorClass or 83; // Mobitex network has issued a DIE command.
  netErrReturnedInMail             = netErrorClass or 84; // The addressed of the transmitted packet was not available, and the message was placed in the network's mailbox.
  netErrReturnedNoTransfer         = netErrorClass or 85; // This message cannot be transferred or put in the network mailbox.
  netErrReturnedIllegal            = netErrorClass or 86; // The message could not be switched to the network
  netErrReturnedCongest            = netErrorClass or 87; // Line, radio channels, or network nodes are congested.
  netErrReturnedError              = netErrorClass or 88; // Technical error in the network.
  netErrReturnedBusy               = netErrorClass or 89; // The B-party is busy.
  netErrGMANState                  = netErrorClass or 90; // The modem has not registered with the network.
  netErrQuitOnTxFail               = netErrorClass or 91; // Couldn't get packet through, shutdown.
  netErrFlexListFull               = netErrorClass or 92; // raw IF error message: see Mobitex spec.
  netErrSenderMAN                  = netErrorClass or 93; // ditto
  netErrIllegalType                = netErrorClass or 94; // ditto
  netErrIllegalState               = netErrorClass or 95; // ditto
  netErrIllegalFlags               = netErrorClass or 96; // ditto
  netErrIllegalSendlist            = netErrorClass or 97; // ditto
  netErrIllegalMPAKLength          = netErrorClass or 98; // ditto
  netErrIllegalAddressee           = netErrorClass or 99; // ditto
  netErrIllegalPacketClass         = netErrorClass or 100; // ditto
  netErrBufferLength               = netErrorClass or 101; // any
  netErrNiCdLowBattery             = netErrorClass or 102; // any
  netErrRFinterfaceFatal           = netErrorClass or 103; // any
  netErrIllegalLogout              = netErrorClass or 104; // raw IF error message
  netErrAAARadioLoad               = netErrorClass or 105; // 7/20/98 JB.  If there is insufficient AAA
  netErrAntennaDown                = netErrorClass or 106;
  netErrNiCdCharging               = netErrorClass or 107; // just for charging
  netErrAntennaWentDown            = netErrorClass or 108;
  netErrNotActivated               = netErrorClass or 109; // The unit has not been FULLY activated.  George and Morty completed.
  netErrRadioTemp                  = netErrorClass or 110; // Radio's temp is too high for FCC compliant TX
  netErrNiCdChargeError            = netErrorClass or 111; // Charging stopped due to NiCd charging characteristic
  netErrNiCdSag                    = netErrorClass or 112; // the computed sag or actual sag indicates a NiCd with diminished capacity.
  netErrNiCdChargeSuspend          = netErrorClass or 113; // Charging has been suspended due to low AAA batteries.
// Left room for more Mobitex errors

// Configuration errors
  netErrConfigNotFound             = netErrorClass or 115;
  netErrConfigCantDelete           = netErrorClass or 116;
  netErrConfigTooMany              = netErrorClass or 117;
  netErrConfigBadName              = netErrorClass or 118;
  netErrConfigNotAlias             = netErrorClass or 119;
  netErrConfigCantPointToAlias     = netErrorClass or 120;
  netErrConfigEmpty                = netErrorClass or 121;
  netErrAlreadyOpenWithOtherConfig = netErrorClass or 122;
  netErrConfigAliasErr             = netErrorClass or 123;
  netErrNoMultiPktAddr             = netErrorClass or 124;
  netErrOutOfPackets               = netErrorClass or 125;
  netErrMultiPktAddrReset          = netErrorClass or 126;
  netErrStaleMultiPktAddr          = netErrorClass or 127;

// Login scripting plugin errors
  netErrScptPluginMissing          = netErrorClass or 128;
  netErrScptPluginLaunchFail       = netErrorClass or 129;
  netErrScptPluginCmdFail          = netErrorClass or 130;
  netErrScptPluginInvalidCmd       = netErrorClass or 131;

  // Telephony errors
  netErrTelMissingComponent        = netErrorClass or 132;
  netErrTelErrorNotHandled         = netErrorClass or 133;

  netErrMobitexStart               = netErrNoChannel;
  netErrMobitexEnd                 = netErrNiCdChargeSuspend;

//-----------------------------------------------------------------------------
// Net library call ID's. Each library call gets the trap number:
//   netTrapXXXX which serves as an index into the library's dispatch table.
//   The constant sysLibTrapCustom is the first available trap number after
//   the system predefined library traps Open,Close,Sleep & Wake.
//
// WARNING!!! This order of these traps MUST match the order of the dispatch
//  table in NetDispatch.c!!!
//-----------------------------------------------------------------------------

type
  NetLibTrapNumberEnum = Enum;

const
  netLibTrapAddrINToA = sysLibTrapCustom;
  netLibTrapAddrAToIN = Succ(netLibTrapAddrINToA);

  netLibTrapSocketOpen = Succ(netLibTrapAddrAToIN);
  netLibTrapSocketClose = Succ(netLibTrapSocketOpen);
  netLibTrapSocketOptionSet = Succ(netLibTrapSocketClose);
  netLibTrapSocketOptionGet = Succ(netLibTrapSocketOptionSet);
  netLibTrapSocketBind = Succ(netLibTrapSocketOptionGet);
  netLibTrapSocketConnect = Succ(netLibTrapSocketBind);
  netLibTrapSocketListen = Succ(netLibTrapSocketConnect);
  netLibTrapSocketAccept = Succ(netLibTrapSocketListen);
  netLibTrapSocketShutdown = Succ(netLibTrapSocketAccept);

  netLibTrapSendPB = Succ(netLibTrapSocketShutdown);
  netLibTrapSend = Succ(netLibTrapSendPB);
  netLibTrapReceivePB = Succ(netLibTrapSend);
  netLibTrapReceive = Succ(netLibTrapReceivePB);
  netLibTrapDmReceive = Succ(netLibTrapReceive);
  netLibTrapSelect = Succ(netLibTrapDmReceive);

  netLibTrapPrefsGet = Succ(netLibTrapSelect);
  netLibTrapPrefsSet = Succ(netLibTrapPrefsGet);

  // The following traps are for internal and Network interface
  //  use only.
  netLibTrapDrvrWake = Succ(netLibTrapPrefsSet);
  netLibTrapInterfacePtr = Succ(netLibTrapDrvrWake);
  netLibTrapMaster = Succ(netLibTrapInterfacePtr);

  // New Traps
  netLibTrapGetHostByName = Succ(netLibTrapMaster);
  netLibTrapSettingGet = Succ(netLibTrapGetHostByName);
  netLibTrapSettingSet = Succ(netLibTrapSettingGet);
  netLibTrapIFAttach = Succ(netLibTrapSettingSet);
  netLibTrapIFDetach = Succ(netLibTrapIFAttach);
  netLibTrapIFGet = Succ(netLibTrapIFDetach);
  netLibTrapIFSettingGet = Succ(netLibTrapIFGet);
  netLibTrapIFSettingSet = Succ(netLibTrapIFSettingGet);
  netLibTrapIFUp = Succ(netLibTrapIFSettingSet);
  netLibTrapIFDown = Succ(netLibTrapIFUp);
  netLibTrapIFMediaUp = Succ(netLibTrapIFDown);
  netLibTrapScriptExecuteV32 = Succ(netLibTrapIFMediaUp);
  netLibTrapGetHostByAddr = Succ(netLibTrapScriptExecuteV32);
  netLibTrapGetServByName = Succ(netLibTrapGetHostByAddr);
  netLibTrapSocketAddr = Succ(netLibTrapGetServByName);
  netLibTrapFinishCloseWait = Succ(netLibTrapSocketAddr);
  netLibTrapGetMailExchangeByName = Succ(netLibTrapFinishCloseWait);
  netLibTrapPrefsAppend = Succ(netLibTrapGetMailExchangeByName);
  netLibTrapIFMediaDown = Succ(netLibTrapPrefsAppend);
  netLibTrapOpenCount = Succ(netLibTrapIFMediaDown);

  netLibTrapTracePrintF = Succ(netLibTrapOpenCount);
  netLibTrapTracePutS = Succ(netLibTrapTracePrintF);

  netLibTrapOpenIfCloseWait = Succ(netLibTrapTracePutS);
  netLibTrapHandlePowerOff = Succ(netLibTrapOpenIfCloseWait);

  netLibTrapConnectionRefresh = Succ(netLibTrapHandlePowerOff);

  // Traps added after 1.0 release of NetLib
  netLibTrapBitMove = Succ(netLibTrapConnectionRefresh);
  netLibTrapBitPutFixed = Succ(netLibTrapBitMove);
  netLibTrapBitGetFixed = Succ(netLibTrapBitPutFixed);
  netLibTrapBitPutUIntV = Succ(netLibTrapBitGetFixed);
  netLibTrapBitGetUIntV = Succ(netLibTrapBitPutUIntV);
  netLibTrapBitPutIntV = Succ(netLibTrapBitGetUIntV);
  netLibTrapBitGetIntV = Succ(netLibTrapBitPutIntV);

  // Traps added after 2.0 release of NetLib
  netLibOpenConfig_ = Succ(netLibTrapBitGetIntV);
  netLibConfigMakeActive_ = Succ(netLibOpenConfig_);
  netLibConfigList_ = Succ(netLibConfigMakeActive_);
  netLibConfigIndexFromName_ = Succ(netLibConfigList_);
  netLibConfigDelete_ = Succ(netLibConfigIndexFromName_);
  netLibConfigSaveAs_ = Succ(netLibConfigDelete_);
  netLibConfigRename_ = Succ(netLibConfigSaveAs_);
  netLibConfigAliasSet_ = Succ(netLibConfigRename_);
  netLibConfigAliasGet_ = Succ(netLibConfigAliasSet_);

  // Traps added after 3.2 release of NetLib
  netLibTrapScriptExecute = Succ(netLibConfigAliasGet_);

  netLibTrapLast = Succ(netLibTrapScriptExecute);

(************************************************************
 * Net Library procedures.
 *************************************************************)

//--------------------------------------------------
// Library initialization, shutdown, sleep and wake
//--------------------------------------------------

function NetLibOpen(libRefnum: UInt16; var netIFErrsP: UInt16): Err; syscall sysLibTrapOpen;

function NetLibClose(libRefnum: UInt16; immediate: UInt16): Err; syscall sysLibTrapClose;

function NetLibSleep(libRefnum: UInt16): Err; syscall sysLibTrapSleep;

function NetLibWake(libRefnum: UInt16): Err; syscall sysLibTrapWake;

// This call forces the library to complete a close if it's
//  currently in the close-wait state. Returns 0 if library is closed,
//  Returns netErrFullyOpen if library is still open by some other task.
function NetLibFinishCloseWait(libRefnum: UInt16): Err; syscall netLibTrapFinishCloseWait;

// This call is for use by the Network preference panel only. It
// causes the NetLib to fully open if it's currently in the close-wait
//  state. If it's not in the close wait state, it returns an error code
function NetLibOpenIfCloseWait(libRefnum: UInt16): Err; syscall netLibTrapOpenIfCloseWait;

// Get the open Count of the NetLib
function NetLibOpenCount(refNum: UInt16; var countP: UInt16): Err; syscall netLibTrapOpenCount;

// Give NetLib a chance to close the connection down in response
// to a power off event. Returns non-zero if power should not be
//  turned off. EventP points to the event that initiated the power off
//  which is either a keyDownEvent of the hardPowerChr or the autoOffChr.
// Don't include unless building for Viewer

function NetLibHandlePowerOff(refNum: UInt16; var eventP: SysEventType): Err; syscall netLibTrapHandlePowerOff;

// Check status or try and reconnect any interfaces which have come down.
// This call can be made by applications when they suspect that an interface
// has come down (like PPP or SLIP). NOTE: This call can display UI
// (if 'refresh' is true) so it MUST be called from the UI task.
function NetLibConnectionRefresh(refNum: UInt16; refresh: Boolean;
                                 var allInterfacesUpP: UInt8; var netIFErrP: UInt16): Err; syscall netLibTrapConnectionRefresh;

//--------------------------------------------------
// Net address translation and conversion routines.
//--------------------------------------------------

// (The NetHToNS, NetHToNL, NetNToHS, and NetNToHL macros which used to be
// defined here are now defined in NetBitUtils.h.  They can still be used
// by #including <NetMgr.h> (this file), because <NetBitUtils.h> is
// unconditionally included below.)

// convert host Int16 to network Int16
function NetHToNS(x: Int16): Int16;

// convert host long to network long
function NetHToNL(x: Int32): Int32;

// convert network Int16 to host Int16
function NetNToHS(x: Int16): Int16;

// convert network long to host long
function NetNToHL(x: Int32): Int32;

// Convert 32-bit IP address to ascii dotted decimal form. The Sockets glue
//  macro inet_ntoa will pass the address of an application global string in
//  spaceP.
function NetLibAddrINToA(libRefnum: UInt16; inet: NetIPAddr; spaceP: PChar): PChar; syscall netLibTrapAddrINToA;

// Convert a dotted decimal ascii string format of an IP address into
//  a 32-bit value.
function NetLibAddrAToIN(libRefnum: UInt16; const a: PChar): NetIPAddr; syscall netLibTrapAddrAToIN;

//--------------------------------------------------
// Socket creation and option setting
//--------------------------------------------------

// Create a socket and return a refnum to it. Protocol is normally 0.
// Returns 0 on success, -1 on error. If error, *errP gets filled in with error code.
function NetLibSocketOpen(libRefnum: UInt16; domain: NetSocketAddrEnum;
                          type_: NetSocketTypeEnum; protocol: Int16; timeout: Int32;
                          var errP: Err): NetSocketRef; syscall netLibTrapSocketOpen;

// Close a socket.
// Returns 0 on success, -1 on error. If error, *errP gets filled in with error code.
function NetLibSocketClose(libRefnum: UInt16; socket: NetSocketRef; timeout: Int32;
                           var errP: Err): Int16; syscall netLibTrapSocketClose;

// Set a socket option. Level is usually netSocketOptLevelSocket. Option is one of
//  netSocketOptXXXXX. OptValueP is a pointer to the new value and optValueLen is
//  the length of the option value.
// Returns 0 on success, -1 on error. If error, *errP gets filled in with error code.
function NetLibSocketOptionSet(libRefnum: UInt16; socket: NetSocketRef;
                            level: UInt16 {NetSocketOptLevelEnum}; option: UInt16 {NetSocketOptEnum};
                            optValueP: Pointer; optValueLen: UInt16;
                            timeout: Int32; var errP: Err): Int16; syscall netLibTrapSocketOptionSet;

// Get a socket option.
// Returns 0 on success, -1 on error. If error, *errP gets filled in with error code.
function NetLibSocketOptionGet(libRefnum: UInt16; socket: NetSocketRef;
                            level: UInt16 {NetSocketOptLevelEnum}; option: UInt16 {NetSocketOptEnum};
                            optValueP: Pointer; var optValueLenP: UInt16;
                            timeout: Int32; var errP: Err): Int16; syscall netLibTrapSocketOptionGet;

//--------------------------------------------------
// Socket Control
//--------------------------------------------------

// Bind a source address and port number to a socket. This makes the
//  socket accept incoming packets destined for the given socket address.
// Returns 0 on success, -1 on error. If error, *errP gets filled in with error code.
function NetLibSocketBind(libRefnum: UInt16; socket: NetSocketRef;
                            sockAddrP: NetSocketAddrPtr; addrLen: Int16; timeout: Int32;
                            var errP: Err): Int16; syscall netLibTrapSocketBind;

// Connect to a remote socket. For a stream based socket (i.e. TCP), this initiates
//  a 3-way handshake with the remote machine to establish a connection. For
//  non-stream based socket, this merely specifies a destination address and port
//  number for future outgoing packets from this socket.
// Returns 0 on success, -1 on error. If error, *errP gets filled in with error code.
function NetLibSocketConnect(libRefnum: UInt16; socket: NetSocketRef;
                            sockAddrP: NetSocketAddrPtr; addrLen: Int16; timeout: Int32;
                            var errP: Err): Int16; syscall netLibTrapSocketConnect;

// Makes a socket ready to accept incoming connection requests. The queueLen
//  specifies the max number of pending connection requests that will be enqueued
//  while the server is busy handling other requests.
//  Only applies to stream based (i.e. TCP) sockets.
// Returns 0 on success, -1 on error. If error, *errP gets filled in with error code.
function NetLibSocketListen(libRefnum: UInt16; socket: NetSocketRef;
                            queueLen: UInt16; timeout: Int32; var errP: Err): Int16; syscall netLibTrapSocketListen;

// Blocks the current process waiting for an incoming connection request. The socket
//  must have previously be put into listen mode through the NetLibSocketListen call.
//  On return, *sockAddrP will have the remote machines address and port number.
//  Only applies to stream based (i.e. TCP) sockets.
// Returns 0 on success, -1 on error. If error, *errP gets filled in with error code.
function NetLibSocketAccept(libRefnum: UInt16; socket: NetSocketRef;
                            sockAddrP: NetSocketAddrPtr; var addrLenP: Int16; timeout: Int32;
                            var errP: Err): Int16; syscall netLibTrapSocketAccept;

// Shutdown a connection in one or both directions.
//  Only applies to stream based (i.e. TCP) sockets.
// Returns 0 on success, -1 on error. If error, *errP gets filled in with error code.
function NetLibSocketShutdown(libRefnum: UInt16; socket: NetSocketRef;
                            direction: Int16 {NetSocketDirEnum}; timeout: Int32; var errP: Err): Int16; syscall netLibTrapSocketShutdown;

// Gets the local and remote addresses of a socket. Useful for TCP sockets that
//  get dynamically bound at connect time.
// Returns 0 on success, -1 on error. If error, *errP gets filled in with error code.
function NetLibSocketAddr(libRefnum: UInt16; socketRef: NetSocketRef;
                            locAddrP: NetSocketAddrPtr; var locAddrLenP: Int16;
                            remAddrP: NetSocketAddrPtr; var remAddrLenP: Int16;
                            timeout: Int32; var errP: Err): Int16; syscall netLibTrapSocketAddr;

//--------------------------------------------------
// Sending and Receiving
//--------------------------------------------------
// Send data through a socket. The data is specified through the NetIOParamType
//  structure.
// Flags is one or more of netMsgFlagXXX.
// Returns # of bytes sent on success, or -1 on error. If error, *errP gets filled
//  in with error code.
function NetLibSendPB(libRefNum: UInt16; socket: NetSocketRef;
                      pbP: NetIOParamPtr; flags: UInt16; timeout: Int32; var errP: Err): Int16; syscall netLibTrapSendPB;

// Send data through a socket. The data to send is passed in a single buffer,
//  unlike NetLibSendPB. If toAddrP is not nil, the data will be sent to
//  address *toAddrP.
// Flags is one or more of netMsgFlagXXX.
// Returns # of bytes sent on success, or -1 on error. If error, *errP gets filled
//  in with error code.
function NetLibSend(libRefNum: UInt16; socket: NetSocketRef;
                    bufP: Pointer; bufLen, flags: UInt16;
                    toAddrP: Pointer; toLen: UInt16; timeout: Int32; var errP: Err): Int16; syscall netLibTrapSend;

// Receive data from a socket. The data is gatthered into buffers specified in the
//  NetIOParamType structure.
// Flags is one or more of netMsgFlagXXX.
// Timeout is max # of ticks to wait, or -1 for infinite, or 0 for none.
// Returns # of bytes received, or -1 on error. If error, *errP gets filled in
//  with error code.
function NetLibReceivePB(libRefNum: UInt16; socket: NetSocketRef;
                         pbP: NetIOParamPtr; flags: UInt16; timeout: Int32; var errP: Err): Int16; syscall netLibTrapReceivePB;

// Receive data from a socket. The data is read into a single buffer, unlike
//  NetLibReceivePB. If fromAddrP is not nil, *fromLenP must be initialized to
//  the size of the buffer that fromAddrP points to and on exit *fromAddrP will
//  have the address of the sender in it.
// Flags is one or more of netMsgFlagXXX.
// Timeout is max # of ticks to wait, or -1 for infinite, or 0 for none.
// Returns # of bytes received, or -1 on error. If error, *errP gets filled in
//  with error code.
function NetLibReceive(libRefNum: UInt16; socket: NetSocketRef;
                       bufP: Pointer; bufLen, flags: UInt16;
                       fromAddrP: Pointer; var fromLenP: UInt16; timeout: Int32; var errP: Err): Int16; syscall netLibTrapReceive;

// Receive data from a socket directly into a (write-protected) Data Manager
//  record.
// If fromAddrP is not nil, *fromLenP must be initialized to
//  the size of the buffer that fromAddrP points to and on exit *fromAddrP will
//  have the address of the sender in it.
// Flags is one or more of netMsgFlagXXX.
// Timeout is max # of ticks to wait, or -1 for infinite, or 0 for none.
// Returns # of bytes received, or -1 on error. If error, *errP gets filled in
//  with error code.
function NetLibDmReceive(libRefNum: UInt16; socket: NetSocketRef;
                         recordP: Pointer; recordOffset: UInt32; rcvLen, flags: UInt16;
                         fromAddrP: Pointer; var fromLenP: UInt16; timeout: Int32; var errP: Err): Int16; syscall netLibTrapDmReceive;

//--------------------------------------------------
// Name Lookups
//--------------------------------------------------

function NetLibGetHostByName(libRefNum: UInt16; const nameP: PChar; bufP: NetHostInfoBufPtr; timeout: Int32; var errP: Err): NetHostInfoPtr; syscall netLibTrapGetHostByName;


function NetLibGetHostByAddr(libRefNum: UInt16; var addrP: UInt8; len, type_: UInt16;
                             bufP: NetHostInfoBufPtr; timeout: Int32; var errP: Err): NetHostInfoPtr; syscall netLibTrapGetHostByAddr;

function NetLibGetServByName(libRefNum: UInt16; const servNameP: PChar;
                             const protoNameP: PChar;  bufP: NetServInfoBufPtr;
                             timeout: Int32; var errP: Err): NetServInfoPtr; syscall netLibTrapGetServByName;

// Looks up a mail exchange name and returns a list of hostnames for it. Caller
//  must pass space for list of return names (hostNames), space for
//  list of priorities for those hosts (priorities) and max # of names to
//  return (maxEntries).
// Returns # of entries found, or -1 on error. If error, *errP gets filled in
//  with error code.
function NetLibGetMailExchangeByName(libRefNum: UInt16; mailNameP: PChar;
                                     maxEntries: UInt16; hostNames: Pointer{Char hostNames[][netDNSMaxDomainName+1]};
                                     priorities: Pointer{UInt16 priorities[]};
                                     timeout: Int32; var errP: Err): Int16; syscall netLibTrapGetMailExchangeByName;

//--------------------------------------------------
// Interface setup
//--------------------------------------------------

function NetLibIFGet(libRefNum: UInt16; index: UInt16; var ifCreatorP: UInt32; var ifInstanceP: UInt16): Err; syscall netLibTrapIFGet;

function NetLibIFAttach(libRefNum: UInt16; ifCreator: UInt32; ifInstance: UInt16; timeout: Int32): Err; syscall netLibTrapIFAttach;

function NetLibIFDetach(libRefNum: UInt16; ifCreator: UInt32; ifInstance: UInt16; timeout: Int32): Err; syscall netLibTrapIFDetach;

function NetLibIFUp(libRefNum: UInt16; ifCreator: UInt32; ifInstance: UInt16): Err; syscall netLibTrapIFUp;

function NetLibIFDown(libRefNum: UInt16; ifCreator: UInt32; ifInstance: UInt16; timeout: Int32): Err; syscall netLibTrapIFDown;

//--------------------------------------------------
// Settings
//--------------------------------------------------
// General settings
function NetLibSettingGet(libRefNum: UInt16; setting: UInt16 {NetSettingEnum}; valueP: Pointer; var valueLenP: UInt16): Err; syscall netLibTrapSettingGet;

function NetLibSettingSet(libRefNum: UInt16; setting: UInt16 {NetSettingEnum}; valueP: Pointer; valueLen: UInt16): Err; syscall netLibTrapSettingSet;

// Network interface specific settings.
function NetLibIFSettingGet(libRefNum: UInt16; ifCreator: UInt32; ifInstance: UInt16;
                            setting: UInt16 {NetIFSettingEnum}; valueP: Pointer; var valueLenP: UInt16): Err; syscall netLibTrapIFSettingGet;

function NetLibIFSettingSet(libRefNum: UInt16; ifCreator: UInt32; ifInstance: UInt16;
                            setting: UInt16 {NetIFSettingEnum}; valueP: Pointer; valueLen: UInt16): Err; syscall netLibTrapIFSettingSet;

//--------------------------------------------------
// System level
//--------------------------------------------------

function NetLibSelect(libRefNum: UInt16; width: UInt16; readFDs, writeFDs, exceptFDs: NetFDSetPtr;
                      timeout: Int32; var errP: Err): Int16; syscall netLibTrapSelect;

//--------------------------------------------------
// Debugging support
//--------------------------------------------------

function NetLibMaster(libRefNum: UInt16; cmd: UInt16; pbP: NetMasterPBPtr;
                      timeout: Int32): Err; syscall netLibTrapMaster;

{!!!
function NetLibTracePrintF(libRefNum: UInt16; const formatStr: PChar; ...): Err; syscall netLibTrapTracePrintF;
!!!}

function NetLibTracePutS(libRefNum: UInt16; strP: PChar): Err; syscall netLibTrapTracePutS;

//--------------------------------------------------
// Configuration Calls
//--------------------------------------------------

function NetLibOpenConfig(refNum: UInt16; configIndex: UInt16; openFlags: UInt32;
                          var netIFErrP: UInt16): Err; syscall netLibOpenConfig_;

function NetLibConfigMakeActive(refNum: UInt16; configIndex: UInt16): Err; syscall netLibConfigMakeActive_;

function NetLibConfigList(refNum: UInt16; nameArray: Pointer {NetConfigNameType nameArray[]};
                          var arrayEntriesP: UInt16): Err; syscall netLibConfigList_;

function NetLibConfigIndexFromName(refNum: UInt16; nameP: NetConfigNamePtr;
                                   var indexP: UInt16): Err; syscall netLibConfigIndexFromName_;

function NetLibConfigDelete(refNum: UInt16; index: UInt16): Err; syscall netLibConfigDelete_;

function NetLibConfigSaveAs(refNum: UInt16; nameP: NetConfigNamePtr): Err; syscall netLibConfigSaveAs_;

function NetLibConfigRename(refNum: UInt16; index: UInt16; newNameP: NetConfigNamePtr): Err; syscall netLibConfigRename_;

function NetLibConfigAliasSet(refNum: UInt16; configIndex, aliasToIndex: UInt16): Err; syscall netLibConfigAliasSet_;

function NetLibConfigAliasGet(refNum: UInt16; aliasIndex: UInt16; var indexP: UInt16; var isAnotherAliasP: Boolean): Err; syscall netLibConfigAliasGet_;

implementation

uses SystemMgr, TimeMgr;

function NetNow: UInt32;
begin
  NetNow := TimGetTicks * 1000 div sysTicksPerSecond;
end;

procedure netFDSet(n: UInt8; var p: NetFDSetType);
begin
  p := p or (1 shl n);
end;

procedure nnetFDClr(n: UInt8; var p: NetFDSetType);
begin
  p := p and not (1 shl n);
end;

function nnetFDIsSet(n: UInt8; var p: NetFDSetType): Boolean;
begin
  nnetFDIsSet := (p and (1 shl n)) <> 0;
end;

procedure nnetFDZero(var p: NetFDSetType);
begin
  p := 0;
end;

// convert host Int16 to network Int16
function NetHToNS(x: Int16): Int16;
begin
  NetHToNS := x;
end;

// convert host long to network long
function NetHToNL(x: Int32): Int32;
begin
  NetHToNL := x;
end;

// convert network Int16 to host Int16
function NetNToHS(x: Int16): Int16;
begin
  NetNToHS := x;
end;

// convert network long to host long
function NetNToHL(x: Int32): Int32;
begin
  NetNToHL := x;
end;

end.
