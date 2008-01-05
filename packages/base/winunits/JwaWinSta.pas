{******************************************************************}
{ This Unit provides Delphi translations of some functions from    }
{ WinSta.dll and Utildll.                                          }
{ Most functions are undocumented and somehow related to           }
{ Terminal Server                                                  }
{                                                                  }
{ Author: Remko Weijnen (r dot weijnen at gmail dot com)           }
{ Documentation can be found at www.remkoweijnen.nl                }
{                                                                  }
{ The contents of this file are subject to                         }
{ the Mozilla Public License Version 1.1 (the "License"); you may  }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at                                  }
{ http://www.mozilla.org/MPL/MPL-1.1.html                          }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{******************************************************************}

{$IFNDEF JWA_OMIT_SECTIONS}

unit JwaWinSta;


interface

{$I jediapilib.inc}

uses
  DateUtils, SysUtils, JwaWinType, // JwaWinType must be declared before JwaWinBase because of duplicate declaration of FILETIME
  JwaWinBase, JwaWinError, JwaNTStatus, JwaWinNT, JwaWinsock2,
  JwaWinSvc, JwaWtsApi32, JwaNative;
{$ENDIF JWA_OMIT_SECTIONS}


{$IFNDEF JWA_IMPLEMENTATIONSECTION}

//==============================================================================
// Defines
//==============================================================================
const
  SERVERNAME_CURRENT = 0;
  
  // constants used for WinStationGetTermSrvCounters
  TOTAL_SESSIONS_CREATED_COUNTER = 1;
  TOTAL_SESSIONS_DISCONNECTED_COUNTER = 2;
  TOTAL_SESSIONS_RECONNECTED_COUNTER = 3;
  TOTAL_SESSIONS_TOTAL_CONNECTED_NOW_COUNTER = 4;
  TOTAL_SESSIONS_TOTAL_DISCONNECTED_NOW_COUNTER = 5;
  TOTAL_SESSIONS_TOTAL_CONNECTED_NOW_COUNTER_2 = 6; //TermSrvSuccLocalLogons;
  TOTAL_SESSIONS_TOTAL_DISCONNECTED_NOW_COUNTER_2 = 7;

  // Max lenght for ElapsedTimeString (server 2008 version of utildll
  // fixes size at 15, so that's assumed to be safe
  ELAPSED_TIME_STRING_LENGTH = 15;

  // WdFlag = WinStation Driver Flag, it is returned in class 3 (WdConfig)
  // of WinStationQueryInformation and has a different value which
  // depends on the protocol. WdFlag is also returned by QueryCurrentWinStation
  WD_FLAG_CONSOLE_XP = $24; // XP
  WD_FLAG_CONSOLE = $34; // 2003/2008
  WD_FLAG_RDP = $36; // XP/2003/2008
  WD_FLAG_ICA = $6E; // Citrix Presentation Server
//  (value from Citrix PS4, other versions could be different!)

  // Class constants for WinStationQueryInformationW
  // These names were found in winsta.dll because they have
  // corresponding unicode 2 ansi conversion functions.
  // Unknown: AsyncConfig, NasiConfig, OemTdConfig, PdConfig, PdConfig2
  // PdParams UserConfig, WinStationCreate, WinStationPrinter
  //
  // The structures below are currently defined, constant names were
  // mapped on best guess:

  WdConfig = 3;
  WinStationClient = 1;
  WinStationConfig = 6;
  WinStationInformation = 8;
  WinStationProductId = 27;
  WinStationRemoteAddress = 29;

  // Constants for WinStationSetInformation
  WinStationBeep = 10;  // Calls MessageBeep

  // This class is used to query the user's primary access token
  // Only System account is allowed to retrieve this!
  WinStationToken = 14;

  // WinStationLocks plays the lock or unlock sound
  // functionality not yet confirmed
  WinStationLock = 28; // Locks or Unlocks the WinStation

  SECONDS_PER_DAY = 86400;
  SECONDS_PER_HOUR = 3600;
  SECONDS_PER_MINUTE = 60;
type
  // This type is used for ElapsedTimeString
  TDiffTime = record
    wDays: Word;
    wHours: Word;
    wMinutes: Word;
    wSeconds: Word;
    wMilliseconds: Word;
  end;
  PDiffTime = ^TDiffTime;

  // This type is used for WinStationQueryLogonCredentialsW
  // dwType can be one of the types defined in JwaWinWlx
  // WLX_CREDENTIAL_TYPE_V1_0 or  WLX_CREDENTIAL_TYPE_V2_0 = 2
  _LOGON_CREDENTIALSW = record
    dwType: DWORD;
    pUsername: PWideChar;
    pDomain: PWideChar;
    pPassword: PWideChar;
    Unknown2 : DWORD;
    Unknown3 : DWORD;
    Unknown4: DWORD;
  end;
  PLOGON_CREDENTIALSW = ^_LOGON_CREDENTIALSW;
  TLogonCredentialsW = _LOGON_CREDENTIALSW;
  PLogonCredentialsW = PLOGON_CREDENTIALSW;

  _WINSTA_USER_TOKEN = record
    ProcessId : DWORD;
    ThreadId : DWORD;
    TokenHandle : THandle;
  end;
  PWINSTA_USER_TOKEN = ^_WINSTA_USER_TOKEN;
  TWinstaUserToken = _WINSTA_USER_TOKEN;
  PWinstaUserToken = ^TWinstaUserToken;

 // this type is used for WinStationGetTemSrvCounters
  _TERM_SRV_COUNTER = record
    dwIndex: DWORD;
    bSuccess: BOOL;
    dwValue: DWORD;
    Reserved2: DWORD;
    Reserved3: DWORD;
    Reserved4: DWORD;
  end;
  PTERM_SRV_COUNTER = ^_TERM_SRV_COUNTER;
  TTermSrvCounter = _TERM_SRV_COUNTER;
  PTermSrvCounter = ^TTermSrvCounter;

  TERM_SRV_COUNTER_ARRAY = array [1..7] of _TERM_SRV_COUNTER;
  PTERM_SRV_COUNTER_ARRAY = ^TERM_SRV_COUNTER_ARRAY;
  TTermSrvCounterArray = TERM_SRV_COUNTER_ARRAY;
  PTermSrvCounterArray = PTERM_SRV_COUNTER_ARRAY;

  // The following types are used for WinStationGetAllProcesses
  // _WINSTA_PROCESS_INFO
  _WINSTA_PROCESS_INFO = record
    ExtendedInfo: PSYSTEM_PROCESSES;
    dwSidLength: DWORD;
    pUserSid: PSID;
  end;
  PWINSTA_PROCESS_INFO = ^_WINSTA_PROCESS_INFO;
  TWinstaProcessInfo = _WINSTA_PROCESS_INFO;
  PWinstaProcessInfo = PWINSTA_PROCESS_INFO;

  // Array of _WINSTA_PROCESS_INFO
  _WINSTA_PROCESS_INFO_ARRAY = array [0..ANYSIZE_ARRAY-1] of _WINSTA_PROCESS_INFO;
  PWINSTA_PROCESS_INFO_ARRAY= ^_WINSTA_PROCESS_INFO_ARRAY;
  TWinstaProcessInfoArray = _WINSTA_PROCESS_INFO_ARRAY;
  PWinstaProcessInfoArray = PWINSTA_PROCESS_INFO_ARRAY;

  // The following types are used for WinStationQueryInformationW

  // WinStationClient, returns information as provided by the
  // Terminal Server client (mstsc).
  _WINSTATION_CLIENTW = record
    Comment: array[0..59] of WCHAR;
    Reserved1: array[0..2] of DWORD;
    ClientUsername: array[0..20] of WCHAR;
    ClientDomain: array[0..17] of WCHAR;
    ClientPassword: array[0..255] of WCHAR; // this was fixec win2000 SP4
    Reserved2: array[0..1635] of BYTE;
    Reserved3: array[0..6] of DWORD;
    Reserved4: array[0..275] of BYTE;
  end;
  PWINSTATION_CLIENTW = ^_WINSTATION_CLIENTW;
  TWinStationClientW = _WINSTATION_CLIENTW;
  PWinStationClientW = PWINSTATION_CLIENTW;

  // WdConfig class, returns information about the WinStationDriver
  _WD_CONFIGW = record
    WdName: array[0..32] of WCHAR;
    WdDLL: array[0..32] of WCHAR;
    WsxDLL: array[0..33] of WCHAR;
    WdFlag: DWORD;
    InputBufferLength: DWORD;
    CfgDLL: array[0..32] of WCHAR;
    WdPrefix: array[0..12] of WCHAR;
  end;
  PWD_CONFIGW = ^_WD_CONFIGW;
  TWdConfigW = _WD_CONFIGW;
  PWdConfigW = PWD_CONFIGW;

  // WinStationConfig class, returns information about the client's
  // configuration such as network, time(zone) settings and such
  _WINSTATION_CONFIGW = record
    Reserved1: DWORD;
    ClientName: array[0..20] of WCHAR;
    Domain: array[0..17] of WCHAR;
    Username: array[0..35] of WCHAR;
    CurrentDirectory: array[0..256] of WCHAR;
    ApplicationName:array[0..259] of WCHAR;
    Reserved2: DWORD;
    AddressFamily: DWORD;  // AF_INET, AF_IPX, AF_NETBIOS, AF_UNSPEC
    ClientAddress: array[0..27] of WCHAR;
    Reserved3: array[0..7] of BYTE;
    Reserved4: array[0..4] of DWORD;
    Reserved5: array[0..69] of BYTE;
    ClientDLLName: array[0..330] of WCHAR;
    Reserved6: array[0..1] of FILETIME;
    AudioDriver: array[0..9] of WCHAR;
    TZBias: DWORD;
    TZStandardName: array[0..31] of WCHAR;
    Reserved7: DWORD; // Standard Bias??
    TZDaylightName: array[0..31] of WCHAR;
    TZDayLightStart: array[0..15] of BYTE;
    TZDayLightBias: DWORD;
    Reserved8: DWORD; // Daylight offset?
    TSInstanceID: array[0..33] of WCHAR; // sometimes windows license key(s)
    Reserved9: DWORD;      // related to license key or instanceid?
  end;
  PWINSTATION_CONFIGW = ^_WINSTATION_CONFIGW;
  TWinStationConfigW = _WINSTATION_CONFIGW;
  PWinStationConfigW = PWINSTATION_CONFIGW;

  // class WinStationInformationClass
  // provides information about the current state of the client such as
  // idletime, sessionstatus and transferred/received bytes
  _WINSTATION_INFORMATIONW = record
    State: DWORD;
    WinStationName: array[0..10] of WideChar;
    Unknown1: array[0..10] of byte;
    Unknown3: array[0..10] of WideChar;
    Unknown2: array[0..8] of byte;
    SessionId: DWORD;
    Reserved2: array[0..3] of byte;
    ConnectTime: FILETIME;
    DisconnectTime: FILETIME;
    LastInputTime: FILETIME;
    LogonTime: FILETIME;
    Unknown4: array[0..11] of byte;
    OutgoingFrames: DWORD;
    OutgoingBytes: DWORD;
    OutgoingCompressBytes: DWORD;
    Unknown5: array[0..435] of byte;
    IncomingCompressedBytes: DWORD;
    Unknown6: array[0..7] of byte;
    IncomingFrames: DWORD;
    IncomingBytes: DWORD;
    Unknown7: array[0..3] of byte;
    Reserved3: array[0..528] of byte;
    Domain: array[0..17] of WideChar;
    Username: array[0..22] of WideChar;
    CurrentTime: FILETIME;
  end;
  PWINSTATION_INFORMATIONW = ^_WINSTATION_INFORMATIONW;
  TWinStationInformationExW = _WINSTATION_INFORMATIONW;
  PWinStationInformationExW = PWINSTATION_INFORMATIONW;

  // WinStationRemoteAddress (class 29)
  // Definition is preliminary
  // AddressFamily can be AF_INET, AF_IPX, AF_NETBIOS, AF_UNSPEC
  // Port is the remote port number (local port number is 3389 by default)
  // Address (for type AF_INET it start's at a 2 byte offset)
  // You can format IP Address to string like this:
  // Format('%d.%d.%d.%d', [WinStationAddress.Address[2],
  //  WinStationRemoteAddress.[3], WinStationRemoteAddress.Address[4],
  //  WinStationRemoteAddress..Address[5]]);
  //
  // Be sure to fill the structure with zeroes before query!
  _WINSTATION_REMOTE_ADDRESS = record
    AddressFamily: DWORD;
    Port: WORD;
    Address: array [0..19] of BYTE;
    Reserved: array[0..5] of BYTE;
  end;
  PWINSTATION_REMOTE_ADDRESS = ^_WINSTATION_REMOTE_ADDRESS;
  TWinStationRemoteAddress = _WINSTATION_REMOTE_ADDRESS;
  PWinStationRemoteAddress = PWINSTATION_REMOTE_ADDRESS;


function AreWeRunningTerminalServices: Boolean;

procedure CachedGetUserFromSid(pSid: PSID; pUserName: LPWSTR;
  var cbUserName: DWORD); stdcall;

function CalculateDiffTime(TimeLow: INT64; TimeHigh: INT64): INT64;
  stdcall;

// Calculate Elapsed time from a Filetime (UTC time) to DiffTime structure
function CalculateElapsedTime(lpFileTime: PFILETIME; var DiffTime: TDiffTime):
  Boolean; stdcall;

function CpuTime2Str(ACPUTime: LARGE_INTEGER): string;

function CurrentDateTimeString(out lpBuffer: PWideChar): Boolean; stdcall;

// This is the version for NT Terminal Server, 2000, XP/2003 and Server 2008
function DateTimeString(DateTime: PFILETIME; lpBuffer: PWideChar): PWideChar;
  stdcall;

// This is a wrapped for all OS versions
function DateTimeStringSafe(DateTime: PFILETIME; lpBuffer: PWideChar;
  cchDest: SIZE_T): PWideChar; stdcall;

// This is the Vista version which takes an additional parameter with
// maximum buffer size (you have to set it) 
function DateTimeStringVista(DateTime: PFILETIME; lpBuffer: PWideChar;
  cchDest: SIZE_T): PWideChar; stdcall;

function DiffTimeString(FTLow: FILETIME; FTHigh: FILETIME;
  out pwElapsedTime: PWideChar): Integer;

// This is the version for NT Terminal Server, 2000, XP/2003 and Server 2008
function ElapsedTimeString(DiffTime: PDiffTime; bShowSeconds: Boolean;
  lpElapsedTime: PWideChar): Integer; stdcall;

// This is a wrapped for all OS versions
function ElapsedTimeStringSafe(DiffTime: PDiffTime; bShowSeconds: Boolean;
  lpElapsedTime: PWideChar; cchDest: SIZE_T): Integer;

// This is the Vista version of ElapsedTimeString which takes an additional
// parameter with the count of characters for lpElapsedTime (you have to set it)
function ElapsedTimeStringEx(DiffTime: PDiffTime; bShowSeconds: Boolean;
  lpElapsedTime: PWideChar; cchDest: SIZE_T): HRESULT; stdcall;

function FileTime2DateTime(FileTime: TFileTime): TDateTime;

function GetUnknownString: PWideChar; stdcall;

function GetWTSLogonIdleTime(hServer: Handle; SessionId: DWORD;
  var sLogonTime: string; var sIdleTime: string): Boolean;

// Helper function that inits the structure for you!
procedure InitTermSrvCounterArray(
  var ATermSrvCounterArray: TTermSrvCounterArray);

function IsTerminalServiceRunning: boolean;

// Tested and working on Windows XP but doesn't seem to work on
// Windows Vista/2008. Better use W version to be sure!
function LogonIdFromWinStationNameA(hServer: HANDLE; pWinStationName: LPSTR;
  var SessionId: DWORD): BOOL; stdcall;

// Tested and working on XP, 2003 and 2008
function LogonIdFromWinStationNameW(hServer: HANDLE; pWinStationName: LPWSTR;
  var SessionId: DWORD): BOOL; stdcall;

// This is the version for NT Terminal Server, 2000, XP/2003 and Server 2008
// Reserve 66 bytes for pWinStationName and 21 for pUserName
function QueryCurrentWinStation(pWinStationName: LPWSTR;
  pUserName: LPWSTR; var SessionId: DWORD; var WdFlag: DWORD): Boolean;
  stdcall;

// This is the Vista version of QueryCurrentWinStation which takes an
// additional parameter with the count of characters for pUserName
// note that pWinStationname is Fixed Size!
function QueryCurrentWinStationEx(pWinStationName: LPWSTR;
  pUserName: PWideChar; cchDest: DWORD; var SessionId: DWORD;
  var WdFlag: DWORD): Boolean; stdcall;

function QueryCurrentWinStationSafe(pWinStationName: LPWSTR;
  pUserName: PWideChar; cchDest: DWORD; var SessionId: DWORD;
  var WdFlag: DWORD): Boolean; stdcall;

function StrConnectState(ConnectState: WTS_CONNECTSTATE_CLASS;
  bShortString: BOOL): PWideChar; stdcall;

function WinStationBroadcastSystemMessage(hServer: HANDLE;
  SendToAllWinstations: BOOL; SessionId: DWORD; TimeOut: DWORD;
  dwFlags: DWORD; lpdwRecipients: DWORD; uiMessage: ULONG; wParam: WPARAM;
  lParam: LPARAM; pResponse: LONGINT): LONGINT; stdcall;

function WinStationCallBack(hServer:HANDLE; SessionId: DWORD;
	pPhoneNumber: LPWSTR): BOOL; stdcall;

function WinStationConnectW(hServer: Handle; SessionId: DWORD;
  TargetSessionId: DWORD; pPassword: LPWSTR;
  bWait: BOOL): BOOL; stdcall;

function WinStationDisconnect(hServer: THandle; SessionId: DWORD;
  bWait: BOOL): BOOL; stdcall;

function WinStationEnumerateA(hServer: HANDLE;
  var ppSessionInfo: PWTS_SESSION_INFOA; var pCount: DWORD): BOOL; stdcall;

function WinStationEnumerateW(hServer: HANDLE;
  var ppSessionInfo: PWTS_SESSION_INFOW; var pCount: DWORD): BOOL; stdcall;

// Used to release memory allocated by WinStationGetAllProcesses
function WinStationFreeGAPMemory(ClassIndex: DWORD;
  pProcessInfo: PWINSTA_PROCESS_INFO_ARRAY; Count: Integer): BOOL; stdcall;

// Important! pProcessInfo must be nil before calling this function
// by using Out parameter Delphi takes care of this for us
function WinStationGetAllProcesses(hServer: HANDLE; ClassIndex: DWORD;
  var Count: Integer; out pProcessInfo: PWINSTA_PROCESS_INFO_ARRAY):
  BOOL; stdcall;

function WinStationGetLanAdapterNameW(hServer: HANDLE; LanaId: DWORD;
  ProtocolTypeLength: DWORD; ProtocolType: PWideChar;
  var ResultLength: DWORD; var LanAdapterName: PWideChar): DWORD; stdcall;

function WinStationGetProcessSid(hServer: Handle; dwPID: DWORD;
  ProcessStartTime: FILETIME; pProcessUserSid: PSID; var dwSidSize: DWORD):
  BOOL; stdcall;

function WinStationGetRemoteIPAddress(hServer: HANDLE; SessionId: DWORD;
  var RemoteIPAddress: string; var Port: WORD): Boolean;

function WinStationGetTermSrvCountersValue(hServer: Handle;
  dwArraySize: DWORD; PCountersArray: PTERM_SRV_COUNTER_ARRAY): BOOL;
  stdcall;

function WinStationNameFromLogonIdA(hServer: HANDLE; SessionId: ULONG;
  pWinStationName: LPSTR): BOOL; stdcall;

function WinStationNameFromLogonIdW(hServer: HANDLE; SessionId: ULONG;
  pWinStationName: LPWSTR): BOOL; stdcall;

function WinStationQueryInformationW(hServer: HANDLE; SessionId: DWORD;
  WinStationInformationClass: Cardinal; pWinStationInformation: PVOID;
  WinStationInformationLength: DWORD; var pReturnLength: DWORD):
  Boolean; stdcall;

function WinStationQueryLogonCredentialsW(
  var LogonCredentials: _LOGON_CREDENTIALSW): HRESULT; stdcall;

function WinstationQueryUserToken(hServer: HANDLE; SessionId: DWORD;
  var hToken: HANDLE): BOOL;

// WinStationRename needs Admin rights and always returns true
// need to check GetLastError
// Duplicate names are not allowed
// Renaming a WinStation gives errors on Remote Connections:
// the windowstation is busy processing connect, disconnect, reset
// or login request

// A version untested
function WinStationRenameA(hServer: HANDLE; pOldWinStationName: LPSTR;
  pNewWinStationName: LPSTR): BOOL; stdcall;

// W version was tested
function WinStationRenameW(hServer: HANDLE; pOldWinStationName: LPWSTR;
  pNewWinStationName: LPWSTR): BOOL; stdcall;

function WinStationSendMessageA(hServer: HANDLE; SessionId: DWORD;
  pTitle: LPSTR; TitleLength: DWORD; pMessage: LPSTR; MessageLength: DWORD;
  Style: DWORD; Timeout: DWORD; var pResponse: DWORD;
  bWait: BOOL): BOOL; stdcall;

function WinStationSendMessageW(hServer: HANDLE; SessionId: DWORD;
  pTitle: LPWSTR; TitleLength: DWORD; pMessage: LPWSTR; MessageLength: DWORD;
  Style: DWORD; Timeout: DWORD; var pResponse: DWORD;
  bWait: BOOL): BOOL; stdcall;

function WinStationSetInformationA(hServer: HANDLE; SessionID: DWORD;
  InformationClass: DWORD; InformationClassDATA: PVOID;
  DataSize: DWORD): BOOL; stdcall;

function WinStationSetInformationW(hServer: HANDLE; SessionID: DWORD;
  InformationClass: DWORD; InformationClassDATA: PVOID;
  DataSize: DWORD): BOOL; stdcall;

function WinStationShadow(hServer: Handle; pServerName: LPWSTR;
  SessionId: DWORD; HotKey: DWORD; HKModifier: DWORD): BOOL; stdcall;

// Admin can stop a shadowed session. SessionId is the targetsession
// so the "victim" and not the one who is shadowing
function WinStationShadowStop(hServer: Handle; SessionId: DWORD;
  bWait: BOOL): BOOL; stdcall;

function WinStationShutDownSystem(hSERVER: HANDLE;
  ShutdownFlags: DWORD): BOOL; stdcall;

function WinStationTerminateProcess(hServer: Handle; dwPID: DWORD;
  dwExitCode: DWORD): BOOL; stdcall;

{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation

uses
  JwaWinDLLNames;
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_INCLUDEMODE}
const
  winstadll = 'winsta.dll';
  utildll = 'utildll.dll';
{$ENDIF JWA_INCLUDEMODE}

{$IFNDEF JWA_INTERFACESECTION}


{$IFNDEF DYNAMIC_LINK}
procedure CachedGetUserFromSid; external utildll name 'CachedGetUserFromSid';
function CalculateDiffTime; external utildll name 'CalculateDiffTime';
function CalculateElapsedTime; external utildll name 'CalculateElapsedTime';
function CurrentDateTimeString; external utildll name 'CurrentDateTimeString';
function DateTimeString; external utildll name 'DateTimeString';
function DateTimeStringVista; external utildll name 'DateTimeString';
function ElapsedTimeString; external utildll name 'ElapsedTimeString';
// Vista version of ElapsedTimeString, exported name is ElapsedTimeString
function ElapsedTimeStringEx; external utildll name 'ElapsedTimeString';
function GetUnknownString; external utildll name 'GetUnknownString';
function LogonIdFromWinStationNameA; external winstadll name 'LogonIdFromWinStationNameA';
function LogonIdFromWinStationNameW; external winstadll name 'LogonIdFromWinStationNameW';
function QueryCurrentWinStation; external utildll name 'QueryCurrentWinStation';
function QueryCurrentWinStationEx; external utildll name 'QueryCurrentWinStation';
function StrConnectState; external utildll name 'StrConnectState';
function WinStationBroadcastSystemMessage; external winstadll name 'WinStationBroadcastSystemMessage';
function WinStationCallBack; external winstadll name 'WinStationCallBack';
function WinStationConnectW; external winstadll name 'WinStationConnectW';
function WinStationDisconnect; external winstadll name 'WinStationDisconnect';
function WinStationEnumerateA; external winstadll name 'WinStationEnumerateA';
function WinStationEnumerateW; external winstadll name 'WinStationEnumerateW';
function WinStationFreeGAPMemory; external winstadll name 'WinStationFreeGAPMemory';
function WinStationGetAllProcesses; external winstadll name 'WinStationGetAllProcesses';
function WinStationGetLanAdapterNameW; external winstadll name 'WinStationGetLanAdapterNameW';
function WinStationGetProcessSid; external winstadll name 'WinStationGetProcessSid';
function WinStationGetTermSrvCountersValue; external winstadll name 'WinStationGetTermSrvCountersValue';
function WinStationNameFromLogonIdA; external winstadll name 'WinStationNameFromLogonIdA';
function WinStationNameFromLogonIdW; external winstadll name 'WinStationNameFromLogonIdW';
function WinStationQueryLogonCredentialsW; external winstadll name 'WinStationQueryLogonCredentialsW';
function WinStationRenameA; external winstadll name 'WinStationRenameA';
function WinStationRenameW; external winstadll name 'WinStationRenameW';
function WinStationSendMessageA; external winstadll name 'WinStationSendMessageA';
function WinStationSendMessageW; external winstadll name 'WinStationSendMessageW';
function WinStationSetInformationA; external winstadll name 'WinStationSetInformationA';
function WinStationSetInformationW; external winstadll name 'WinStationSetInformationW';
function WinStationShadow; external winstadll name 'WinStationShadow';
function WinStationShadowStop; external winstadll name 'WinStationShadowStop';
function WinStationShutDownSystem; external winstadll name 'WinStationShutDownSystem';
function WinStationQueryInformationW; external winstadll name 'WinStationQueryInformationW';
function WinStationTerminateProcess; external winstadll name 'WinStationTerminateProcess';
{$ELSE}

var
  __CachedGetUserFromSid: Pointer;

procedure CachedGetUserFromSid;
begin
  GetProcedureAddress(__CachedGetUserFromSid, utildll, 'CachedGetUserFromSid');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__CachedGetUserFromSid]
  end;
end;

var
  __CalculateDiffTime: Pointer;

function CalculateDiffTime;
begin
  GetProcedureAddress(__CalculateDiffTime, utildll, 'CalculateDiffTime');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__CalculateDiffTime]
  end;
end;

var
  __CalculateElapsedTime: Pointer;

function CalculateElapsedTime;
begin
  GetProcedureAddress(__CalculateElapsedTime, utildll, 'CalculateElapsedTime');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__CalculateElapsedTime]
  end;
end;

var
  __CurrentDateTimeString: Pointer;

function CurrentDateTimeString;
begin
  GetProcedureAddress(__CurrentDateTimeString, utildll, 'CurrentDateTimeString');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__CurrentDateTimeString]
  end;
end;

var
  __DateTimeString: Pointer;

function DateTimeString;
begin
  GetProcedureAddress(__DateTimeString, utildll, 'DateTimeString');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__DateTimeString]
  end;
end;

var
  __DateTimeStringVista: Pointer;

function DateTimeStringVista;
begin
  GetProcedureAddress(__DateTimeStringVista, utildll, 'DateTimeString');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__DateTimeStringVista]
  end;
end;

var
  __ElapsedTimeString: Pointer;

function ElapsedTimeString;
begin
  GetProcedureAddress(__ElapsedTimeString, utildll, 'ElapsedTimeString');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__ElapsedTimeString]
  end;
end;

var
  __ElapsedTimeStringEx: Pointer;

function ElapsedTimeStringEx;
begin
  GetProcedureAddress(__ElapsedTimeStringEx, utildll, 'ElapsedTimeString');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__ElapsedTimeStringEx]
  end;
end;


var
  __GetUnknownString: Pointer;

function GetUnknownString;
begin
  GetProcedureAddress(__GetUnknownString, utildll, 'GetUnknownString');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__GetUnknownString]
  end;
end;

var
  __LogonIdFromWinStationNameA: Pointer;

function LogonIdFromWinStationNameA;
begin
  GetProcedureAddress(__LogonIdFromWinStationNameA, winstadll, 'LogonIdFromWinStationNameA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__LogonIdFromWinStationNameA]
  end;
end;

var
  __LogonIdFromWinStationNameW: Pointer;

function LogonIdFromWinStationNameW;
begin
  GetProcedureAddress(__LogonIdFromWinStationNameW, winstadll, 'LogonIdFromWinStationNameW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__LogonIdFromWinStationNameW]
  end;
end;

var
  __QueryCurrentWinStation: Pointer;

function QueryCurrentWinStation;
begin
  GetProcedureAddress(__QueryCurrentWinStation, utildll, 'QueryCurrentWinStation');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__QueryCurrentWinStation]
  end;
end;

var
  __QueryCurrentWinStationEx: Pointer;

function QueryCurrentWinStationEx;
begin
  GetProcedureAddress(__QueryCurrentWinStationEx, utildll, 'QueryCurrentWinStation');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__QueryCurrentWinStationEx]
  end;
end;

var
  __StrConnectState: Pointer;

function StrConnectState;
begin
  GetProcedureAddress(__StrConnectState, utildll, 'StrConnectState');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__StrConnectState]
  end;
end;

var
  __WinStationBroadcastSystemMessage: Pointer;

function WinStationBroadcastSystemMessage;
begin
  GetProcedureAddress(__WinStationBroadcastSystemMessage, winstadll, 'WinStationBroadcastSystemMessage');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationBroadcastSystemMessage]
  end;
end;

var
  __WinStationCallBack: Pointer;

function WinStationCallBack;
begin
  GetProcedureAddress(__WinStationCallBack, winstadll, 'WinStationCallBack');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationCallBack]
  end;
end;


var
  __WinStationConnectW: Pointer;

function WinStationConnectW;
begin
  GetProcedureAddress(__WinStationConnectW, winstadll, 'WinStationConnectW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationConnectW]
  end;
end;


var
  __WinStationDisconnect: Pointer;

function WinStationDisconnect;
begin
  GetProcedureAddress(__WinStationDisconnect, winstadll, 'WinStationDisconnect');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationDisconnect]
  end;
end;

var
  __WinStationEnumerateA: Pointer;

function WinStationEnumerateA;
begin
  GetProcedureAddress(__WinStationEnumerateA, winstadll, 'WinStationEnumerateA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationEnumerateA]
  end;
end;

var
  __WinStationEnumerateW: Pointer;

function WinStationEnumerateW;
begin
  GetProcedureAddress(__WinStationEnumerateW, winstadll, 'WinStationEnumerateW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationEnumerateW]
  end;
end;

var
  __WinStationFreeGAPMemory: Pointer;

function WinStationFreeGAPMemory;
begin
  GetProcedureAddress(__WinStationFreeGAPMemory, winstadll, 'WinStationFreeGAPMemory');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationFreeGAPMemory]
  end;
end;

var
  __WinStationGetAllProcesses: Pointer;

function WinStationGetAllProcesses;
begin
  GetProcedureAddress(__WinStationGetAllProcesses, winstadll, 'WinStationGetAllProcesses');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationGetAllProcesses]
  end;
end;

var
  __WinStationGetLanAdapterNameW: Pointer;

function WinStationGetLanAdapterNameW;
begin
  GetProcedureAddress(__WinStationGetLanAdapterNameW, winstadll, 'WinStationGetLanAdapterNameW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationGetLanAdapterNameW]
  end;
end;

var
  __WinStationGetProcessSid: Pointer;

function WinStationGetProcessSid;
begin
  GetProcedureAddress(__WinStationGetProcessSid, winstadll, 'WinStationGetProcessSid');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationGetProcessSid]
  end;
end;

var
  __WinStationGetTermSrvCountersValue: Pointer;

function WinStationGetTermSrvCountersValue;
begin
  GetProcedureAddress(__WinStationGetTermSrvCountersValue, winstadll, 'WinStationGetTermSrvCountersValue');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationGetTermSrvCountersValue]
  end;
end;

var
  __WinStationNameFromLogonIdA: Pointer;

function WinStationNameFromLogonIdA;
begin
  GetProcedureAddress(__WinStationNameFromLogonIdA, winstadll, 'WinStationNameFromLogonIdA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationNameFromLogonIdA]
  end;
end;


var
  __WinStationNameFromLogonIdW: Pointer;

function WinStationNameFromLogonIdW;
begin
  GetProcedureAddress(__WinStationNameFromLogonIdW, winstadll, 'WinStationNameFromLogonIdW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationNameFromLogonIdW]
  end;
end;

var
  __WinStationQueryLogonCredentialsW: Pointer;

function WinStationQueryLogonCredentialsW;
begin
  GetProcedureAddress(__WinStationQueryLogonCredentialsW, winstadll, 'WinStationQueryLogonCredentialsW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationQueryLogonCredentialsW]
  end;
end;

var
  __WinStationRenameA: Pointer;

function WinStationRenameA;
begin
  GetProcedureAddress(__WinStationRenameA, winstadll, 'WinStationRenameA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationRenameA]
  end;
end;

var
  __WinStationRenameW: Pointer;

function WinStationRenameW;
begin
  GetProcedureAddress(__WinStationRenameW, winstadll, 'WinStationRenameW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationRenameW]
  end;
end;

var
  __WinStationQueryInformationW: Pointer;

function WinStationQueryInformationW;
begin
  GetProcedureAddress(__WinStationQueryInformationW, winstadll, 'WinStationQueryInformationW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationQueryInformationW]
  end;
end;

var
  __WinStationSendMessageA: Pointer;

function WinStationSendMessageA;
begin
  GetProcedureAddress(__WinStationSendMessageA, winstadll, 'WinStationSendMessageA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationSendMessageA]
  end;
end;

var
  __WinStationSendMessageW: Pointer;

function WinStationSendMessageW;
begin
  GetProcedureAddress(__WinStationSendMessageW, winstadll, 'WinStationSendMessageW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationSendMessageW]
  end;
end;

var
  __WinStationSetInformationA: Pointer;

function WinStationSetInformationA;
begin
  GetProcedureAddress(__WinStationSetInformationA, winstadll, 'WinStationSetInformationA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationSetInformationA]
  end;
end;

var
  __WinStationSetInformationW: Pointer;

function WinStationSetInformationW;
begin
  GetProcedureAddress(__WinStationSetInformationW, winstadll, 'WinStationSetInformationW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationSetInformationW]
  end;
end;

var
  __WinStationShadow: Pointer;

function WinStationShadow;
begin
  GetProcedureAddress(__WinStationShadow, winstadll, 'WinStationShadow');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationShadow]
  end;
end;


var
  __WinStationShadowStop : Pointer;

function WinStationShadowStop;
begin
  GetProcedureAddress(__WinStationShadowStop, winstadll, 'WinStationShadowStop');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationShadowStop]
  end;
end;


var
  __WinStationShutDownSystem : Pointer;

function WinStationShutDownSystem;
begin
  GetProcedureAddress(__WinStationShutDownSystem, winstadll, 'WinStationShutDownSystem');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationShutDownSystem]
  end;
end;

var
  __WinStationTerminateProcess: Pointer;

function WinStationTerminateProcess;
begin
  GetProcedureAddress(__WinStationTerminateProcess, winstadll, 'WinStationTerminateProcess');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationTerminateProcess]
  end;
end;
{$ENDIF DYNAMIC_LINK}

// This function is not exported
function IsVista: boolean;
var VersionInfo: TOSVersionInfoEx;
begin
  // Zero Memory and set structure size
  ZeroMemory(@VersionInfo, SizeOf(VersionInfo));
  VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
  GetVersionEx(@VersionInfo);

  // Are we running Vista?
  Result := (VersionInfo.dwMajorVersion = 6) and
    (VersionInfo.dwMinorVersion = 0) and
    (VersionInfo.wProductType = VER_NT_WORKSTATION);
end;

// This the way QWinsta checks if Terminal Services is active:
function AreWeRunningTerminalServices: Boolean;
var VersionInfo: TOSVersionInfoEx;
  dwlConditionMask: Int64;
begin
  // Zero Memory and set structure size
  ZeroMemory(@VersionInfo, SizeOf(VersionInfo));
  VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);

  // We are either Terminal Server or Personal Terminal Server
  VersionInfo.wSuiteMask := VER_SUITE_TERMINAL or VER_SUITE_SINGLEUSERTS;
  dwlConditionMask := VerSetConditionMask(0, VER_SUITENAME, VER_OR);

  // Test it
  Result := VerifyVersionInfo(VersionInfo, VER_SUITENAME, dwlConditionMask);
end;

// This functions converts CPU times as returned by
// TSystemProcesses structure to a string
function CpuTime2Str(ACPUTime: LARGE_INTEGER): String;
var SystemTime: TSystemTime;
{$IFDEF COMPILER7_UP}
  FS: TFormatSettings;
{$ENDIF COMPILER7_UP}
begin
  FileTimeToSystemTime(FILETIME(ACPUTime), SystemTime);
{$IFDEF COMPILER7_UP}
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, FS);
  Result := TimeToStr(SystemTimeToDateTime(SystemTime), FS);
{$ELSE}
  Result := TimeToStr(SystemTimeToDateTime(SystemTime));
{$ENDIF COMPILER7_UP}
end;

function DateTimeStringSafe(DateTime: PFILETIME; lpBuffer: PWideChar;
  cchDest: SIZE_T): PWideChar; stdcall;
begin
  // Zero Memory
  ZeroMemory(lpBuffer, cchDest * SizeOf(WCHAR));

 // Are we running Vista?
  if IsVista then
  begin
    // Vista version
    Result := DateTimeStringVista(DateTime, lpBuffer, cchDest);
  end
  else begin
    // Other OS's (including server 2008!)
    Result := DateTimeString(DateTime, lpBuffer);
  end;
end;

// DiffTimeString is a helper function that returns a formatted
// Elapsed time string (the way Idle Time is displayed in TSAdmin)
// Return value is the string length
function DiffTimeString(FTLow: FILETIME; FTHigh: FILETIME;
  out pwElapsedTime: PWideChar): Integer;
var
  DiffSecs: INT64;
  DiffTime: TDiffTime;
  NumChars: DWORD;
begin
  // Get the Difftime where Time1 is the "oldest" time
  // Return value is the difference in seconds
  DiffSecs := CalculateDiffTime(Int64(FTLow), Int64(FTHigh));
  // Recalc DiffTime to TDiffTime
  ZeroMemory(@DiffTime, SizeOf(DiffTime));
  // Calculate no of whole days
  DiffTime.wDays := DiffSecs DIV SECONDS_PER_DAY;
  // Calculate no of whole hours
  DiffTime.wHours :=  DiffSecs MOD SECONDS_PER_DAY DIV SECONDS_PER_HOUR;
  // Calculate no of whole minutes
  DiffTime.wMinutes := DiffSecs MOD SECONDS_PER_DAY MOD SECONDS_PER_HOUR
    DIV SECONDS_PER_MINUTE; // Result = No of whole minutes
  // Calculate no of whole seconds
  DiffTime.wSeconds := DiffSecs MOD SECONDS_PER_DAY MOD SECONDS_PER_HOUR
    MOD SECONDS_PER_MINUTE; // Result = No of seconds
  // Note that Milliseconds are not used and therefore not calculated

  // Reserve Memory
  GetMem(pwElapsedTime, ELAPSED_TIME_STRING_LENGTH * SizeOf(WCHAR));
  // Format Elapsed TimeString in minutes (bShowSeconds = False)
  NumChars := ElapsedTimeStringSafe(@DiffTime, False, pwElapsedTime,
    ELAPSED_TIME_STRING_LENGTH);
  Result := NumChars;
  // Caller has to free memory when done
end;

function ElapsedTimeStringSafe(DiffTime: PDiffTime; bShowSeconds: Boolean;
  lpElapsedTime: PWideChar; cchDest: SIZE_T): Integer;
var
  hr: HRESULT;
begin
  // Zero Memory
  ZeroMemory(lpElapsedTime, cchDest * SizeOf(WCHAR));

 // Are we running Vista?
  if IsVista then
  begin
    hr := ElapsedTimeStringEx(DiffTime, bShowSeconds, lpElapsedTime,
      cchDest);
    if Succeeded(hr) then
    begin
      Result := cchDest;
    end
    else begin
      Result := 0;
    end;
   
  end
  else begin
    Result := ElapsedTimeString(DiffTime, bShowSeconds, lpElapsedTime);
  end;
  // Caller has to free memory when done
end;

function FileTime2DateTime(FileTime: TFileTime): TDateTime;
var LocalFileTime: TFileTime;
  SystemTime: TSystemTime;
begin
  FileTimeToLocalFileTime(FileTime, LocalFileTime);
  FileTimeToSystemTime(LocalFileTime, SystemTime);
  Result := SystemTimeToDateTime(SystemTime);
end;

function GetWTSLogonIdleTime(hServer: HANDLE; SessionId: DWORD;
  var sLogonTime: string; var sIdleTime: string): Boolean;
var
  uReturnLength: DWORD;
  Info: _WINSTATION_INFORMATIONW;
  CurrentTime: TDateTime;
  LastInputTime: TDateTime;
  IdleTime: TDateTime;
  LogonTime: TDateTime;
  Days, Hours, Minutes: Word;
  {$IFDEF COMPILER7_UP}
  FS: TFormatSettings;
  {$ENDIF COMPILER7_UP}
begin
  {$IFDEF COMPILER7_UP}
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, FS);
  {$ENDIF COMPILER7_UP}
  uReturnLength := 0;
  try
    Result := WinStationQueryInformationW(hServer, SessionId, 8, @Info, SizeOf(Info), uReturnLength);
    if Result then
    begin
      LogonTime := FileTime2DateTime(Info.LogonTime);
      if YearOf(LogonTime) = 1601 then
        sLogonTime := ''
      else
        {$IFDEF COMPILER7_UP}
        sLogonTime := DateTimeToStr(LogonTime, FS);
        {$ELSE}
        sLogonTime := DateTimeToStr(LogonTime);
        {$ENDIF COMPILER7_UP}
      { from Usenet post by Chuck Chopp
        http://groups.google.com/group/microsoft.public.win32.programmer.kernel/browse_thread/thread/c6dd86e7df6d26e4/3cf53e12a3246e25?lnk=st&q=WinStationQueryInformationa+group:microsoft.public.*&rnum=1&hl=en#3cf53e12a3246e25
        2)  The system console session cannot go into an idle/disconnected state.
            As such, the LastInputTime value will always math CurrentTime for the
            console session.
        3)  The LastInputTime value will be zero if the session has gone
            disconnected.  In that case, use the DisconnectTime value in place of
            LastInputTime when calculating the current idle time for a disconnected session.
        4)  All of these time values are GMT time values.
        5)  The disconnect time value will be zero if the sesson has never been
            disconnected.}
      CurrentTime := FileTime2DateTime(Info.CurrentTime);
      LastInputTime := FileTime2DateTime(Info.LastInputTime);

      // Disconnected session = idle since DisconnectTime
      if YearOf(LastInputTime) = 1601 then
        LastInputTime := FileTime2DateTime(Info.DisconnectTime);

      IdleTime := LastInputTime - CurrentTime;
      Days := Trunc(IdleTime);
      Hours := HourOf(IdleTime);
      Minutes := MinuteOf(IdleTime);
      if Days > 0 then
        sIdleTime := Format('%dd %d:%1.2d', [Days, Hours, Minutes])
      else
      if Hours > 0 then
        sIdleTime := Format('%d:%1.2d', [Hours, Minutes])
      else
      if Minutes > 0 then
        sIdleTime := IntToStr(Minutes)
      else
        sIdleTime := '-';
    end;
  except
    Result := False;
  end;
end;

procedure InitTermSrvCounterArray(var ATermSrvCounterArray: TTermSrvCounterArray);
begin
  ATermSrvCounterArray[1].dwIndex := TOTAL_SESSIONS_CREATED_COUNTER;
  ATermSrvCounterArray[2].dwIndex := TOTAL_SESSIONS_DISCONNECTED_COUNTER;
  ATermSrvCounterArray[3].dwIndex := TOTAL_SESSIONS_RECONNECTED_COUNTER;
  ATermSrvCounterArray[4].dwIndex := TOTAL_SESSIONS_TOTAL_CONNECTED_NOW_COUNTER;
  ATermSrvCounterArray[5].dwIndex := TOTAL_SESSIONS_TOTAL_DISCONNECTED_NOW_COUNTER;
  ATermSrvCounterArray[6].dwIndex := TOTAL_SESSIONS_TOTAL_CONNECTED_NOW_COUNTER_2;
  ATermSrvCounterArray[7].dwIndex := TOTAL_SESSIONS_TOTAL_DISCONNECTED_NOW_COUNTER_2;
end;

// This is the way WTSApi32.dll checks if Terminal Service is running
function IsTerminalServiceRunning: boolean;
var hSCM: HANDLE;
  hService: HANDLE;
  ServiceStatus: SERVICE_STATUS;
begin
  Result := False;
  // Open handle to Service Control Manager
  hSCM := OpenSCManager(nil, SERVICES_ACTIVE_DATABASE, GENERIC_READ);
  if hSCM > 0 then
  begin
    // Open handle to Terminal Server Service
    hService := OpenService(hSCM, 'TermService', GENERIC_READ);
    if hService > 0 then
    begin
      // Check if the service is running
      QueryServiceStatus(hService, ServiceStatus);
      Result := ServiceStatus.dwCurrentState = SERVICE_RUNNING;
      // Close the handle
      CloseServiceHandle(hService);
    end;
    // Close the handle
    CloseServiceHandle(hSCM);
  end;
end;

function QueryCurrentWinStationSafe(pWinStationName: LPWSTR;
  pUserName: PWideChar; cchDest: DWORD; var SessionId: DWORD;
  var WdFlag: DWORD): Boolean;
begin
  // Zero Memory
  ZeroMemory(pWinStationName, 66);
  ZeroMemory(pUserName, cchDest * SizeOf(WCHAR));

  // Are we running Vista?
  if IsVista then
  begin
    Result := QueryCurrentWinStationEx(pWinStationName, pUserName, cchDest,
      SessionId, WdFlag);
  end
  else begin
    Result := QueryCurrentWinStation(pWinStationName, pUserName, SessionId,
      WdFlag);
  end;

end;

function WinStationGetRemoteIPAddress(hServer: HANDLE; SessionId: DWORD;
  var RemoteIPAddress: string; var Port: WORD): Boolean;
var WinStationRemoteIPAddress: TWinStationRemoteAddress;
  pReturnLength: DWORD;
begin
  // Zero Memory
  ZeroMemory(@WinStationRemoteIPAddress, SizeOf(WinStationRemoteIPAddress));
  // Query Remote Address
  Result := WinStationQueryInformationW(hServer, SessionId,
    WinStationRemoteAddress, @WinStationRemoteIPAddress,
    SizeOf(WinStationRemoteIPAddress), pReturnLength);
  if Result then
  begin
    // If the AddressFamily is IPv4
    if WinStationRemoteIPAddress.AddressFamily = AF_INET then
    begin
      // The ntohs function converts a u_short from TCP/IP network byte order
      // to host byte order (which is little-endian on Intel processors).
      Port := ntohs(WinStationRemoteIPAddress.Port);
      with WinStationRemoteIPAddress do
      begin
        // format the IP Address as string
        RemoteIPAddress := Format('%d.%d.%d.%d', [Address[2], Address[3],
          Address[4], Address[5]]);
        // If you want to convert the to a sockaddr structure you could
        // user WSAStringToAddress
      end;
    end
    else begin
      Result := False;
      Port := 0;
      RemoteIPAddress := '';
      // SetLastError to give the user a clue as to why we failed..
      //  An address incompatible with the requested protocol was used.
      // (An address incompatible with the requested protocol was used.)
      SetLastError(WSAEAFNOSUPPORT);
    end;
  end;
end;

function WinStationQueryUserToken(hServer: HANDLE; SessionId: DWORD;
  var hToken: HANDLE): BOOL;
var WinstaUserToken: _WINSTA_USER_TOKEN;
  dwReturnLength: DWORD;
  LUID: _LUID;
  bWasPrivEnabled: Boolean;
  Res: NTSTATUS;
begin
  // Enable SeTcbPrivilege (system account has this enabled by default)
  LookupPrivilegeValue(nil, SE_TCB_NAME, LUID);
  Res := RtlAdjustPrivilege(LUID.LowPart, True, False, @bWasPrivEnabled);

  // Initialize structure
  WinstaUserToken.ProcessId :=  GetCurrentProcessId;  // Current Process Id
  WinstaUserToken.ThreadId := GetCurrentThreadId; // Current Thread Id
  WinstaUserToken.TokenHandle := 0;

  if Res = STATUS_SUCCESS then
  begin
    // Query for the token, we are only allowed to do this if we are the
    // System account (else ACCESS_DENIED is returned)
    Result := WinStationQueryInformationW(hServer, SessionId, WinStationToken,
      @WinstaUserToken, SizeOf(WinstaUserToken), dwReturnLength);
    hToken := WinStaUserToken.TokenHandle;

    // Restore state of SeTcbPrivilege
    RtlAdjustPrivilege(LUID.LowPart, bWasPrivEnabled, False, @bWasPrivEnabled);
  end
  else begin
    Result := False;
    // Convert NTStatus to WinError and SetLastError
    SetLastError(RtlNtStatusToDosError(Res));
  end;

end;

{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}

