{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }
//
// Module Name:
//
//   ras.h
//
// Abstract:
//
//   Remote Access Service structures and defines
//

//
//  Microsoft Windows Mobile 6.0 for PocketPC SDK.
//

unit RAS;

{$CALLING cdecl}

interface

uses Windows, Tapi;

{$PACKRECORDS 4} // #include "pshpack4.h"

const
      RAS_MaxEntryName      = 20;
      RAS_MaxDeviceName     = 128;
      RAS_MaxDeviceType     = 16;
      RAS_MaxParamKey       = 32;
      RAS_MaxParamValue     = 128;
      RAS_MaxPhoneNumber    = 128;
      RAS_MaxCallbackNumber = 48;
      RAS_MaxIpAddress      = 15;
      RAS_MaxIpxAddress     = 21;

// Ras extensions

     	RAS_MaxAreaCode			= 10;
     	RAS_MaxPadType			 = 32;
     	RAS_MaxX25Address	= 200;
     	RAS_MaxFacilities	= 200;
     	RAS_MaxUserData			= 200;

// RAS IP Address

type
     tagRasIpAddr = record
       a:byte;
       b:byte;
       c:byte;
       d:byte;
     end;
     RASIPADDR = tagRasIpAddr;

// RAS Entry Definition
type
      tagRASENTRYW = RECORD
	       dwSize:DWORD;
	       dwfOptions:DWORD;
	       dwCountryID:DWORD;
	       dwCountryCode:DWORD;
			     szAreaCode:array[0..RAS_MaxAreaCode] of WCHAR;
	       szLocalPhoneNumber:array[0..RAS_MaxPhoneNumber] of WCHAR;
        dwAlternateOffset:DWORD;
	       ipaddr:RASIPADDR;
	       ipaddrDns:RASIPADDR;
	       ipaddrDnsAlt:RASIPADDR;
	       ipaddrWins:RASIPADDR;
	       ipaddrWinsAlt:RASIPADDR;
	       dwFrameSize:DWORD;
	       dwfNetProtocols:DWORD;
	       dwFramingProtocol:DWORD;
        szScript:array[0..MAX_PATH-1] of WCHAR;
	       szAutodialDll:array[0..MAX_PATH-1] of WCHAR;
	       szAutodialFunc:array[0..MAX_PATH-1] of WCHAR;
			     szDeviceType:array[0..RAS_MaxDeviceType] of WCHAR;
	       szDeviceName:array[0..RAS_MaxDeviceName] of WCHAR;
	       szX25PadType:array[0..RAS_MaxPadType] of WCHAR;
	       szX25Address:array[0..RAS_MaxX25Address] of WCHAR;
	       szX25Facilities:array[0..RAS_MaxFacilities] of WCHAR;
	       szX25UserData:array[0..RAS_MaxUserData] of WCHAR;
	       dwChannels:DWORD;
	       dwReserved1:DWORD;
	       dwReserved2:DWORD;

	       dwCustomAuthKey:DWORD; // EAP extension type to use
      end;
      RASENTRYW = tagRASENTRYW;

      RASENTRY    = RASENTRYW;
      LPRASENTRYW = ^RASENTRYW;
      LPRASENTRY  = ^RASENTRY;

// Describes Country Information

      RASCTRYINFO = record
	       dwSize:DWORD;
	       dwCountryID:DWORD;
	       dwNextCountryID:DWORD;
	       dwCountryCode:DWORD;
	       dwCountryNameOffset:DWORD;
      end;

      LPRASCTRYINFO = ^RASCTRYINFO;
      RASCTRYINFOW  = RASCTRYINFO;
      LPRASCTRYINFOW = ^RASCTRYINFOW;

// Describes RAS Device Information
      tagRASDEVINFOW = record
        dwSize:DWORD;
	       szDeviceType:array[0..RAS_MaxDeviceType] of WCHAR;
	       szDeviceName:array[0..RAS_MaxDeviceName] of WCHAR;
      end;
      RASDEVINFOW = tagRASDEVINFOW;
      RASDEVINFO		= RASDEVINFOW;
      LPRASDEVINFOW	= ^RASDEVINFOW;
      LPRASDEVINFO	= ^RASDEVINFO;

// RASENTRY 'dwfOptions' bit flags.
const
      RASEO_UseCountryAndAreaCodes  = $00000001;
      RASEO_SpecificIpAddr          = $00000002;
      RASEO_SpecificNameServers     = $00000004;
      RASEO_IpHeaderCompression     = $00000008;
      RASEO_RemoteDefaultGateway    = $00000010;
      RASEO_DisableLcpExtensions    = $00000020;
      RASEO_TerminalBeforeDial      = $00000040;
      RASEO_TerminalAfterDial       = $00000080;
      RASEO_ModemLights             = $00000100;
      RASEO_SwCompression           = $00000200;
      RASEO_RequireEncryptedPw      = $00000400;
      RASEO_RequireMsEncryptedPw    = $00000800;
      RASEO_RequireDataEncryption   = $00001000;
      RASEO_NetworkLogon            = $00002000;
      RASEO_UseLogonCredentials     = $00004000;
      RASEO_PromoteAlternates       = $00008000;
      RASEO_SecureLocalFiles        = $00010000;
      RASEO_DialAsLocalCall         = $00020000;

      RASEO_ProhibitPAP             = $00040000;
      RASEO_ProhibitCHAP            = $00080000;
      RASEO_ProhibitMsCHAP          = $00100000;
      RASEO_ProhibitMsCHAP2         = $00200000;
      RASEO_ProhibitEAP             = $00400000;
      RASEO_PreviewUserPw           = $01000000;
      RASEO_NoUserPwRetryDialog     = $02000000;
      RASEO_CustomScript            = $80000000;


// RASENTRY 'dwfNetProtocols' bit flags. (session negotiated protocols)
      RASNP_NetBEUI       = $00000001;  // Negotiate NetBEUI
      RASNP_Ipx           = $00000002;  // Negotiate IPX
      RASNP_Ip            = $00000004;  // Negotiate TCP/IP


// RASENTRY 'dwFramingProtocols' (framing protocols used by the server)
      RASFP_Ppp           = $00000001;  // Point-to-Point Protocol (PPP)
      RASFP_Slip          = $00000002;  // Serial Line Internet Protocol (SLIP)
      RASFP_Ras           = $00000004;  // Microsoft proprietary protocol


// RASENTRY 'szDeviceType' strings
      RASDT_Direct        = 'direct';    // Direct Connect (WINCE Extension)
      RASDT_Modem         = 'modem';     // Modem
      RASDT_Isdn          = 'isdn';      // ISDN
      RASDT_X25           = 'x25';       // X.25
      RASDT_Vpn           = 'vpn';		  // PPTP
      RASDT_PPPoE         = 'PPPoE';     // PPPoE

type
    HRASCONN = HANDLE;
    LPHRASCONN = ^HRASCONN;

// Identifies an active RAS connection.  (See RasEnumConnections)

    tagRASCONNW = record
      dwSize:DWORD;
      hrasconn:HRASCONN;
      szEntryName:array[0..RAS_MaxEntryName] of WCHAR;
    end;
    RASCONNW = tagRASCONNW;
    RASCONN = RASCONNW;

    LPRASCONNW = ^RASCONNW;
    LPRASCONN  = ^RASCONN;


// Enumerates intermediate states to a connection.  (See RasDial)
const
      RASCS_PAUSED = $1000;
      RASCS_DONE   = $2000;

type
      tagRASCONNSTATE = (RASCS_OpenPort := 0,
                         RASCS_PortOpened,
                         RASCS_ConnectDevice,
                         RASCS_DeviceConnected,
                         RASCS_AllDevicesConnected,
                         RASCS_Authenticate,
                         RASCS_AuthNotify,
                         RASCS_AuthRetry,
                         RASCS_AuthCallback,
                         RASCS_AuthChangePassword,
                         RASCS_AuthProject,
                         RASCS_AuthLinkSpeed,
                         RASCS_AuthAck,
                         RASCS_ReAuthenticate,
                         RASCS_Authenticated,
                         RASCS_PrepareForCallback,
                         RASCS_WaitForModemReset,
                         RASCS_WaitForCallback,
                         RASCS_Projected,

                         RASCS_Interactive 			= RASCS_PAUSED,
                         RASCS_RetryAuthentication,
                         RASCS_CallbackSetByCaller,
                         RASCS_PasswordExpired,

                         RASCS_Connected 			= RASCS_DONE,
                         RASCS_Disconnected
                        );
      RASCONNSTATE = tagRASCONNSTATE;
      LPRASCONNSTATE = ^tagRASCONNSTATE;


// Describes the status of a RAS connection.  (See RasConnectionStatus)

      tagRASCONNSTATUSW = record
        dwSize:DWORD;
        rasconnstate:RASCONNSTATE;
        dwError:DWORD;
        szDeviceType:array[0..RAS_MaxDeviceType] of WCHAR;
        szDeviceName:array[0..RAS_MaxDeviceName] of WCHAR;
      end;
      RASCONNSTATUSW = tagRASCONNSTATUSW;
      RASCONNSTATUS = RASCONNSTATUSW;
      LPRASCONNSTATUSW = ^RASCONNSTATUSW;
      LPRASCONNSTATUS  = ^RASCONNSTATUS;

// Describes connection establishment parameters.  (See RasDial)

      tagRASDIALPARAMSW = record
        dwSize:DWORD;
        szEntryName:array[0..RAS_MaxEntryName] of WCHAR;
        szPhoneNumber:array[0..RAS_MaxPhoneNumber] of WCHAR;
        szCallbackNumber:array[0..RAS_MaxCallbackNumber] of WCHAR;
        szUserName:array[0..UNLEN] of WCHAR;
        szPassword:array[0..PWLEN] of WCHAR;
        szDomain:array[0..DNLEN] of WCHAR;
      end;
      RASDIALPARAMSW = tagRASDIALPARAMSW;
      RASDIALPARAMS = RASDIALPARAMSW;

      LPRASDIALPARAMSW = ^RASDIALPARAMSW;
      LPRASDIALPARAMS  = ^RASDIALPARAMS;

// Describes extended connection establishment options.  (See RasDial)

      tagRASDIALEXTENSIONS = record
        dwSize:DWORD;
        dwfOptions:DWORD;
        hwndParent:HWND;
        reserved:DWORD;
      end;
      RASDIALEXTENSIONS = tagRASDIALEXTENSIONS;
      LPRASDIALEXTENSIONS = ^RASDIALEXTENSIONS;

// 'dwfOptions' bit flags.
const
      RDEOPT_UsePrefixSuffix           = $00000001;
      RDEOPT_PausedStates              = $00000002;
      RDEOPT_IgnoreModemSpeaker        = $00000004;
      RDEOPT_SetModemSpeaker           = $00000008;
      RDEOPT_IgnoreSoftwareCompression = $00000010;
      RDEOPT_SetSoftwareCompression    = $00000020;


// Describes an enumerated RAS phone book entry name.  (See RasEntryEnum)
type
     tagRASENTRYNAMEW = record
       dwSize:DWORD;
       szEntryName:array[0..RAS_MaxEntryName] of WCHAR;
     end;
     RASENTRYNAMEW = tagRASENTRYNAMEW;
     RASENTRYNAME = RASENTRYNAMEW;
     LPRASENTRYNAMEW = ^RASENTRYNAMEW;
     LPRASENTRYNAME  = ^RASENTRYNAME;

//
// Custom Script Dll Support
//
type
     tagRASCOMMSETTINGS = record
       dwSize:DWORD;
       bParity:byte;
       bStop:byte;
       wAlign:word;
     end;
     RASCOMMSETTINGS = tagRASCOMMSETTINGS;
     LPRASCOMMSETTINGS = ^tagRASCOMMSETTINGS;

type
      PFNRASSETCOMMSETTINGS = function(hPort:HANDLE;
                                       pRasCommSettings:LPRASCOMMSETTINGS;
                                       pvReserved:PVOID):DWORD; cdecl;

type
     RASCUSTOMSCRIPTEXTENSIONS = record
       dwSize:DWORD;
       pfRasSetCommSettings:PFNRASSETCOMMSETTINGS;
     end;

type
     PFNRASGETBUFFER = function(ppBuffer:LPBYTE;
                                pdwSize:LPDWORD):DWORD; cdecl;

     PFNRASFREEBUFFER = function (pBuffer:LPBYTE):DWORD; cdecl;

     PFNRASSENDBUFFER = function(hPort:HANDLE;
                                 pBuffer:LPBYTE;
                                 dwSize:DWORD):DWORD; cdecl;

     PFNRASRECEIVEBUFFER = function(hPort:HANDLE;
                                    pBuffer:LPBYTE;
                                    pdwSize:LPDWORD;
                                    dwTimeoutMilliseconds:DWORD;
                                    hEvent:HANDLE):DWORD; cdecl;

    PFNRASRETRIEVEBUFFER = function(hPort:HANDLE;
                                    pBuffer:LPBYTE;
                                    pdwSize:LPDWORD):DWORD; cdecl;

// Protocol code to projection data structure mapping.

type
     tagRASPROJECTION = (RASP_PppIp := $8021,
                         RASP_PppIpx := $802B,
                         RASP_PppNbf := $803F,
                         RASP_PppIpV6 := $8057,
                         RASP_PppLcp := $C021,
                         RASP_Amb := $10000,
                         RASP_Slip := $20000);
     RASPROJECTION = tagRASPROJECTION;
     LPRASPROJECTION = ^RASPROJECTION;

{*
** Describes the result of a RAS AMB (Authentication Message Block)
** projection.  This protocol is used with NT 3.1 and OS/2 1.3 downlevel
** RAS servers.
*}

type
     tagRASAMBA = record
       dwSize:DWORD;
       dwError:DWORD;
       szNetBiosError:array[0..NETBIOS_NAME_LEN] of TCHAR;
       bLana:byte;
     end;
     LPRASAMB = ^RASAMB;

// Describes the result of a PPP NBF (NetBEUI) projection.

type
     tagRASPPPNBFW = record
       dwSize:DWORD;
       dwError:DWORD;
       dwNetBiosError:DWORD;
       szNetBiosError:array[0..NETBIOS_NAME_LEN] of TCHAR;
       szWorkstationName:array[0..NETBIOS_NAME_LEN] of TCHAR;
       bLana:byte;
     end;

     RASPPPNBF = tagRASPPPNBFW;
     LPRASPPPNBF = ^tagRASPPPNBFW;


// Describes the results of a PPP IPX (Internetwork Packet Exchange)
// projection.

type
     tagRASIPX = record
       dwSize:DWORD;
       dwError:DWORD;
       szIpxAddress:array[0..RAS_MaxIpxAddress] of TCHAR;
     end;
     RASPPPIPX = tagRASIPX;
     LPRASPPPIPX = ^tagRASIPX;

// Describes the results of an PPP IP (Internet) projection.

     tagRASPPPIPW = record
       dwSize:DWORD;
       dwError:DWORD;
       szIpAddress:array[0..RAS_MaxIpAddress] of WCHAR;
       szServerIpAddress:array[0..RAS_MaxIpAddress] of WCHAR;
	      dwOptions:DWORD;
	      dwServerOptions:DWORD;
     end;
     RASPPPIPW = tagRASPPPIPW;
     RASPPPIP = RASPPPIPW;

     LPRASPPPIPW = ^RASPPPIPW;
     LPRASPPPIP  = ^RASPPPIP;

//
// RASPPPIP 'dwOptions' and 'dwServerOptions' flags.
//
const
      RASIPO_VJ       = $00000001;		// Indicates that VJ compression is on

// Describes the results of an PPP IPV6 (Internet) projection.
type
     tagRASPPPIPV6 = record
       dwSize:DWORD;
       dwError:DWORD;
       LocalInterfaceIdentifier:array[0..7]of byte;
       PeerInterfaceIdentifier:array[0..7] of byte;
	      LocalCompressionProtocol:array[0..1] of byte;
	      PeerCompressionProtocol:array[0..1] of byte;
     end;
     RASPPPIPV6 = tagRASPPPIPV6;
     LPRASPPPIPV6 = ^RASPPPIPV6;

// Describes results of a GET_WINS/DNS I/O control. If
// the requested address has been negotiated the boolean
// valid is TRUE and the IpAddress contains the address of
// the requested server.  If Valid is false IpAddress is
// zero.

     tagRASPPPAddr = record
	      dwSize:DWORD;
	      dwError:DWORD;
	      Valid:BOOL;
	      IpAddress:DWORD;
     end;
     RASPPPADDR = tagRASPPPAddr;

// Describes the results of a SLIP (Serial Line IP) projection.
type
     tagRASSLIPW = record
       dwSize:DWORD;
       dwError:DWORD;
       szIpAddress:array[0..RAS_MaxIpAddress] of WCHAR;
     end;
     RASSLIPW = tagRASSLIPW;
     RASSLIP = RASSLIPW;
     LPRASSLIPW = ^RASSLIPW;
     LPRASSLIP = ^RASSLIP;


// 	RAS Control Structures
//
// 	Control Request Enumeration
type
     tagRasCntlEnum = (RASCNTL_SET_DEBUG,							// obsolete
	                      RASCNTL_LOCK_STATUS,						// obsolete
                       RASCNTL_PRINT_CS,							// obsolete
                       RASCNTL_STATISTICS,							// Get statistics
                       RASCNTL_ENUMDEV,							// Enum Devices
                       RASCNTL_GETPROJINFO,						// RasGetProjectionInfoW
                       RASCNTL_GETDISPPHONE,						// RasGetDispPhoneNumW
                       RASCNTL_DEVCONFIGDIALOGEDIT,				// RasDevConfigDialogEditW
                       RASCNTL_SERVER_GET_STATUS,					// Get status of PPP server and lines               NULL                   RASCNTL_SERVERSTATUS + dwNumLines * RASCNTL_SERVERLINE
                       RASCNTL_SERVER_ENABLE,					    // Turn the PPP server on                           NULL                   NULL
                       RASCNTL_SERVER_DISABLE,					    // Turn the PPP server off                          NULL                   NULL
                       RASCNTL_SERVER_GET_PARAMETERS,				// Get global server parameters                     NULL                   RASCNTL_SERVERSTATUS
                       RASCNTL_SERVER_SET_PARAMETERS,				// Set global server parameters                     RASCNTL_SERVERSTATUS   NULL
                       RASCNTL_SERVER_LINE_ADD,					// Add a line to be managed by the PPP server       RASCNTL_SERVERLINE     NULL
                       RASCNTL_SERVER_LINE_REMOVE,					// Remove a line being managed by the PPP server    RASCNTL_SERVERLINE     NULL
                       RASCNTL_SERVER_LINE_ENABLE,				    // Enable management of a line                      RASCNTL_SERVERLINE     NULL
                       RASCNTL_SERVER_LINE_DISABLE,			    // Disable management of a line                     RASCNTL_SERVERLINE     NULL
                       RASCNTL_SERVER_LINE_GET_PARAMETERS,		    // Get line parameters                              RASCNTL_SERVERLINE     RASCNTL_SERVERLINE
                       RASCNTL_SERVER_LINE_SET_PARAMETERS,		    // Set line parameters                              RASCNTL_SERVERLINE     NULL
                       RASCNTL_SERVER_USER_SET_CREDENTIALS,		// Allow a username/password                        RASCNTL_SERVERUSERCREDENTIALS NULL
                       RASCNTL_SERVER_USER_DELETE_CREDENTIALS,		// Remove a username                                RASCNTL_SERVERUSERCREDENTIALS NULL
                       RASCNTL_EAP_GET_USER_DATA,					// Get a ras entry's EAP user data
                       RASCNTL_EAP_SET_USER_DATA,					// Set a ras entry's EAP user data
                       RASCNTL_EAP_GET_CONNECTION_DATA,			// Get a ras entry's EAP conn data
                       RASCNTL_EAP_SET_CONNECTION_DATA,			// Set a ras entry's EAP conn data
                       RASCNTL_ENABLE_LOGGING,					    // Load logging extension dll if present
                       RASCNTL_DISABLE_LOGGING,					// Unload logging extension dll if loaded, stop logging
                       RASCNTL_SERVER_LINE_GET_CONNECTION_INFO,    // Get state info on a server lines                 RASCNTL_SERVERLINE               RASCNTL_SERVERCONNECTION
                       RASCNTL_SERVER_GET_IPV6_NET_PREFIX,         // Get IPV6 Network prefix pool                     NULL                             RASCNTL_SERVER_IPV6_NET_PREFIX
                       RASCNTL_SERVER_SET_IPV6_NET_PREFIX,         // Set IPV6 Network prefix pool                     PRASCNTL_SERVER_IPV6_NET_PREFIX  NULL
                       RASCNTL_LAYER_OPEN,                         // Open LCP/Auth/CCP/IPCP                           DWORD (Layer Id)                 NULL
                       RASCNTL_LAYER_CLOSE,                        // Close LCP/Auth/CCP/IPCP                          DWORD (Layer Id)                 NULL
                       RASCNTL_LAYER_RENEGOTIATE,                  // Renegotiate LCP/Auth/CCP/IPCP                    DWORD (Layer Id)                 NULL
                       RASCNTL_LAYER_PARAMETER_GET,                // Get LCP/Auth/CCP/IPCP value                      RASCNTL_LAYER_PARAMETER          RASCNTL_LAYER_PARAMETER
                       RASCNTL_LAYER_PARAMETER_SET                 // Set LCP/Auth/CCP/IPCP value                      RASCNTL_LAYER_PARAMETER          NULL
                       // Add others here
                      );
     RasCntlEnum_t = tagRasCntlEnum;

const
      RASCNTL_LAYER_PARAMETER_TYPE_NONE	 = 0;
      RASCNTL_LAYER_PARAMETER_TYPE_DWORD	= 1;
      RASCNTL_LAYER_PARAMETER_TYPE_BYTES	= 2;

type
     _RASCNTL_LAYER_PARAMETER = record
	      dwProtocolType:DWORD;
	      dwParameterId:DWORD;
	      dwValueType:DWORD;
	      dwValueSize:DWORD;
       case DWORD of
         0: (dwValue:DWORD);       // for most (simple integer) values
         1: (bytesValue:array[0..0] of byte); // array of bytes (size 1 is placeholder)
     end;
     RASCNTL_LAYER_PARAMETER = _RASCNTL_LAYER_PARAMETER;
     PRASCNTL_LAYER_PARAMETER = ^_RASCNTL_LAYER_PARAMETER;

type
     tagRasCntlDevConfigDlgEdit = record
	      szDeviceName:array[0..RAS_MaxDeviceName] of WCHAR;
	      szDeviceType:array[0..RAS_MaxDeviceType] of WCHAR;
	      hWndOwner:HWND;
	      dwSize:DWORD;
	      DataBuf:array[0..0] of byte;
     end;
     RASCNTL_DEVCFGDLGED = tagRasCntlDevConfigDlgEdit;
     PRASCNTL_DEVCFGDLGED = ^tagRasCntlDevConfigDlgEdit;

const
      MAX_IF_NAME_LEN	= 256;

//
//	RASCNTL_SERVERSTATUS is...
//
//	..Returned by:
//		RASCNTL_SERVER_GET_STATUS
//		RASCNTL_SERVER_GET_PARAMETERS
//  ..Passed to:
//		RASCNTL_SERVER_SET_PARAMETERS
//
const
      PPPSRV_FLAG_REQUIRE_DATA_ENCRYPTION			   = 1 shl 1;	// Require encryption on this connection.
      PPPSRV_FLAG_ALLOW_UNAUTHENTICATED_ACCESS	= 1 shl 2;	// Do not require authentication on the connection
      PPPSRV_FLAG_NO_VJ_HEADER_COMPRESSION		   = 1 shl 3;	// Prevent VJ TCP/IP header compression
      PPPSRV_FLAG_NO_DATA_COMPRESSION				      = 1 shl 4;	// Prevent MS data compression
      PPPSRV_FLAG_ADD_CLIENT_SUBNET				        = 1 shl 5;	// Add a subnet route for a client connection

type
     tagRasCntlServerStatus = record
	      bEnable:BOOL;
	      bmFlags:DWORD;
	      bUseDhcpAddresses:BOOL;		// Obtain addresses from DHCP server rather than static pool
	      dwStaticIpAddrStart:DWORD;	// If using static IP address pool, this is the first address
	      dwStaticIpAddrCount:DWORD;	// Number of static IP addresses following IpAddrStart in pool
	      bmAuthenticationMethods:DWORD;// Bitmask of authentication methods to be disallowed, see
      // RASEO_ProhibitXxx in ras.h

	      dwNumLines:DWORD;

	      bUseAutoIpAddresses:BOOL;    // TRUE if IP addresses for clients should be generated from AutoIp pool
	      dwAutoIpSubnet:DWORD;         // Defines AutoIP address pool
	      dwAutoIpSubnetMask:DWORD;

	      wszDhcpInterface:array[0..MAX_IF_NAME_LEN] of WCHAR;
     end;
     RASCNTL_SERVERSTATUS = tagRasCntlServerStatus;
     PRASCNTL_SERVERSTATUS = ^tagRasCntlServerStatus;

//
//	Structure passed to the following IOCTLs
//		 RASCNTL_SERVER_LINE_GET_PARAMETERS
//		 RASCNTL_SERVER_LINE_SET_PARAMETERS
//		 RASCNTL_SERVER_LINE_ENABLE
//		 RASCNTL_SERVER_LINE_DISABLE
//
type
     tagRasCntlServerLine = record
		     rasDevInfo:RASDEVINFO;				// szDeviceType and szDeviceName of the line
	      bEnable:BOOL;
	      bmFlags:DWORD;
	      DisconnectIdleSeconds:UINT;
	      dwDevConfigSize:DWORD;
	      DevConfig:array[0..0] of byte;			// Variable size (dwDevConfigSize bytes) array of device config info
     end;
     RASCNTL_SERVERLINE = tagRasCntlServerLine;
     PRASCNTL_SERVERLINE = ^tagRasCntlServerLine;

//
//	Structure passed to the following IOCTLs
//		RASCNTL_SERVER_USER_SET_CREDENTIALS			 Allow a username/password
//		RASCNTL_SERVER_USER_DELETE_CREDENTIALS		 Remove a username
//
type
     tagRasCntlServerUser = record
	      tszUserName:array[0..UNLEN] of TCHAR;
	      tszDomainName:array[0..DNLEN] of TCHAR;			// may be null
	      password:array[0..PWLEN-1] of byte;
			    cbPassword:DWORD;
     end;
     RASCNTL_SERVERUSERCREDENTIALS = tagRasCntlServerUser;
     PRASCNTL_SERVERUSERCREDENTIALS = ^tagRasCntlServerUser;

//
//  Structure containing info describing an enabled line's connection status
//
//  Used with the IOCTLS:
//	    RASCNTL_SERVER_LINE_GET_CONNECTION_INFO
//
type
     tagRasCntlServerConnection = record
       rasDevInfo:RASDEVINFO;		                 // szDeviceType and szDeviceName of the line
       hrasconn:HRASCONN;                           // handle that can be used in RasGetLinkStatistics (NULL if line is not enabled)
       dwServerIpAddress:DWORD;                  // IP Address for server line IP interface
       dwClientIpAddress:DWORD;                  // IP Address that will be assigned to client connecting to this line
       RasConnState:RASCONNSTATE;                       // RASCS_Disconnected, etc.
       tszUserName:array[0..DNLEN+1+UNLEN] of TCHAR; // Name of user logged in to the port
     end;
     RASCNTL_SERVERCONNECTION = tagRasCntlServerConnection;
     PRASCNTL_SERVERCONNECTION = ^tagRasCntlServerConnection;

//
//  Structure containing info describing the pool of IPV6 network prefixes that
//  can be assigned by the server.
//  Used with the IOCTLS:
//	    RASCNTL_SERVER_GET_IPV6_NET_PREFIX
//	    RASCNTL_SERVER_SET_IPV6_NET_PREFIX
//
type
     tagRasCntlServerIPV6NetPrefix = record
       IPV6NetPrefix:array[0..15] of byte;
       IPV6NetPrefixBitLength:DWORD;
       IPV6NetPrefixCount:DWORD;
     end;
     RASCNTL_SERVER_IPV6_NET_PREFIX = tagRasCntlServerIPV6NetPrefix;
     PRASCNTL_SERVER_IPV6_NET_PREFIX = ^tagRasCntlServerIPV6NetPrefix;

//
// RasDial message notifications are sent with Message ID set to
// WM_RASDIALEVENT
//
const
      RASDIALEVENT    	= 'RasDialEvent';

      WM_RASDIALEVENT 	= $CCCD;


type
     _RAS_STATS = record
       dwSize:DWORD;
       dwBytesXmited:DWORD;
       dwBytesRcved:DWORD;
       dwFramesXmited:DWORD;
       dwFramesRcved:DWORD;
       dwCrcErr:DWORD;
       dwTimeoutErr:DWORD;
       dwAlignmentErr:DWORD;
       dwHardwareOverrunErr:DWORD;
       dwFramingErr:DWORD;
       dwBufferOverrunErr:DWORD;
       dwCompressionRatioIn:DWORD;
       dwCompressionRatioOut:DWORD;
       dwBps:DWORD;
       dwConnectDuration:DWORD;
     end;

     RAS_STATS = _RAS_STATS;
     PRAS_STATS = ^_RAS_STATS;


{$IFDEF WINCE} // #ifdef UNDER_CE

// This structure is used by the RnaApp application on WINCE
// to signal when a connection has occured.

type
     tagRNAAppInfo = record
	      dwSize:DWORD;					// The size of this structure
	      hWndRNAApp:DWORD;				// The handle of the RNAApp window
	      Context:DWORD;				// Context value specified on CmdLine
	      ErrorCode:DWORD;				// Last error code
	      RasEntryName:array[0..RAS_MaxEntryName] of TCHAR;
     end;
     RNAAPP_INFO = tagRNAAppInfo;
     PRNAAPP_INFO = ^tagRNAAppInfo;

const
      RNA_RASCMD		= WM_USER+1;
      RNA_ADDREF		= 1;
      RNA_DELREF		= 2;
{$ENDIF WINCE}

{$PACKRECORDS DEFAULT} // #include "poppack.h"

{$IFNDEF RASAPI}
{$DEFINE RASAPI}
{$ENDIF RASAPI}

function RasDial(dialExtensions:LPRASDIALEXTENSIONS;
	                phoneBookPath:LPTSTR;
	                rasDialParam:LPRASDIALPARAMS;
	                NotifierType:DWORD;
	                notifier:LPVOID;
	                pRasConn:LPHRASCONN):DWORD; external KernelDLL name 'RasDial'; // index 1F3

function RasDialW(dialExtensions:LPRASDIALEXTENSIONS;
 	                phoneBookPath:LPTSTR;
	                 rasDialParam:LPRASDIALPARAMS;
	                 NotifierType:DWORD;
	                 notifier:LPVOID;
	                 pRasConn:LPHRASCONN):DWORD; external KernelDLL name 'RasDial'; // index 1F3

function RasHangUp(Session:HRASCONN):DWORD; external KernelDLL name 'RasHangUp'; // index 1F4, 1F5
function RasHangUpW(Session:HRASCONN):DWORD; external KernelDLL name 'RasHangUp'; // index 1F4, 1F5

function RasEnumEntries(Reserved:LPWSTR;
		                      lpszPhoneBookPath:LPWSTR;
		                      lprasentryname:LPRASENTRYNAME;
		                      lpcb:LPDWORD;
		                      lpcEntries:LPDWORD):DWORD; external KernelDLL name 'RasEnumEntries'; // index 1F6
function RasEnumEntriesW(Reserved:LPWSTR;
		                       lpszPhoneBookPath:LPWSTR;
		                       lprasentryname:LPRASENTRYNAME;
		                       lpcb:LPDWORD;
		                       lpcEntries:LPDWORD):DWORD; external KernelDLL name 'RasEnumEntries'; // index 1F6

function RasGetEntryDialParams(lpszPhoneBook:LPWSTR;
			                            lpRasDialParams:LPRASDIALPARAMS;
			                            lpfPassword:LPBOOL):DWORD; external KernelDLL name 'RasGetEntryDialParams'; // index 1F7 
function RasGetEntryDialParamsW(lpszPhoneBook:LPWSTR;
			                             lpRasDialParams:LPRASDIALPARAMS;
			                             lpfPassword:LPBOOL):DWORD; external KernelDLL name 'RasGetEntryDialParams'; // index 1F7

function RasSetEntryDialParams(lpszPhoneBook:LPWSTR;
			                            lpRasDialParams:LPRASDIALPARAMS;
			                            fRemovePassword:BOOL):DWORD; external KernelDLL name 'RasSetEntryDialParams'; // index 1F8
function RasSetEntryDialParamsW(lpszPhoneBook:LPWSTR;
			                             lpRasDialParams:LPRASDIALPARAMS;
			                             fRemovePassword:BOOL):DWORD; external KernelDLL name 'RasSetEntryDialParams'; // index 1F8

function RasGetEntryProperties(lpszPhoneBook:LPWSTR;
			                            szEntry:LPWSTR;
			                            lpEntry:LPRASENTRY;
			                            lpdwEntrySize:LPDWORD;
			                            lpb:LPBYTE;
			                            lpdwSize:LPDWORD):DWORD; external KernelDLL name 'RasGetEntryProperties'; // index 1F9
function RasGetEntryPropertiesW(lpszPhoneBook:LPWSTR;
			                             szEntry:LPWSTR;
			                             lpEntry:LPRASENTRY;
			                             lpdwEntrySize:LPDWORD;
			                             lpb:LPBYTE;
			                             lpdwSize:LPDWORD):DWORD; external KernelDLL name 'RasGetEntryProperties'; // index 1F9


function RasSetEntryProperties(lpszPhoneBook:LPWSTR;
			                            szEntry:LPWSTR;
			                            lpEntry:LPRASENTRY;
			                            dwEntrySize:DWORD;
			                            lpb:LPBYTE;
			                            dwSize:DWORD):DWORD; external KernelDLL name 'RasSetEntryProperties'; // index 1FA
function RasSetEntryPropertiesW(lpszPhoneBook:LPWSTR;
			                             szEntry:LPWSTR;
			                             lpEntry:LPRASENTRY;
			                             dwEntrySize:DWORD;
			                             lpb:LPBYTE;
			                             dwSize:DWORD):DWORD; external KernelDLL name 'RasSetEntryProperties'; // index 1FA

function RasValidateEntryName(lpszPhonebook:LPCWSTR;
			                           lpszEntry:LPCWSTR):DWORD; external KernelDLL name 'RasValidateEntryName'; // index 1FB
function RasValidateEntryNameW(lpszPhonebook:LPCWSTR;
			                            lpszEntry:LPCWSTR):DWORD; external KernelDLL name 'RasValidateEntryName'; // index 1FB

function RasDeleteEntry(lpszPhonebook:LPWSTR;
		                      lpszEntry:LPWSTR):DWORD; external KernelDLL name 'RasDeleteEntry'; // index 1FC
function RasDeleteEntryW(lpszPhonebook:LPWSTR;
		                      lpszEntry:LPWSTR):DWORD; external KernelDLL name 'RasDeleteEntry'; // index 1FC

function RasRenameEntry(lpszPhonebook:LPWSTR;
		                      lpszOldEntry:LPWSTR;
		                      lpszNewEntry:LPWSTR):DWORD; external KernelDLL name 'RasRenameEntry'; // index 1FD
function RasRenameEntryW(lpszPhonebook:LPWSTR;
		                       lpszOldEntry:LPWSTR;
		                       lpszNewEntry:LPWSTR):DWORD; external KernelDLL name 'RasRenameEntry'; // index 1FD

function RasEnumConnections(lprasconn:LPRASCONN;
			                         lpcb:LPDWORD;
			                         lpcConnections:LPDWORD):DWORD; external KernelDLL name 'RasEnumConnections'; // index 1FE
function RasEnumConnectionsW(lprasconn:LPRASCONN;
			                          lpcb:LPDWORD;
			                          lpcConnections:LPDWORD):DWORD; external KernelDLL name 'RasEnumConnections'; // index 1FE

function RasGetConnectStatus(rasconn:HRASCONN;
			                          lprasconnstatus:LPRASCONNSTATUS):DWORD; external KernelDLL name 'RasGetConnectStatus'; // index 1FF
function RasGetConnectStatusW(rasconn:HRASCONN;
			                           lprasconnstatus:LPRASCONNSTATUS):DWORD; external KernelDLL name 'RasGetConnectStatus'; // index 1FF

function RasGetEntryDevConfig(szPhonebook:LPCTSTR;
			                           szEntry:LPCTSTR;
			                           pdwDeviceID:LPDWORD;
			                           pdwSize:LPDWORD;
			                           pDeviceConfig:LPVARSTRING):DWORD; external KernelDLL name 'RasGetEntryDevConfig'; // index 200
function RasGetEntryDevConfigW(szPhonebook:LPCTSTR;
			                            szEntry:LPCTSTR;
			                            pdwDeviceID:LPDWORD;
			                            pdwSize:LPDWORD;
			                            pDeviceConfig:LPVARSTRING):DWORD; external KernelDLL name 'RasGetEntryDevConfig'; // index 200

function RasSetEntryDevConfig(szPhonebook:LPCTSTR;
			                           szEntry:LPCTSTR;
			                           dwDeviceID:DWORD;
				                          lpDeviceConfig:LPVARSTRING):DWORD; external KernelDLL name 'RasSetEntryDevConfig'; // index 201
function RasSetEntryDevConfigW(szPhonebook:LPCTSTR;
			                            szEntry:LPCTSTR;
			                            dwDeviceID:DWORD;
				                           lpDeviceConfig:LPVARSTRING):DWORD; external KernelDLL name 'RasSetEntryDevConfig'; // index 201


function RasEnumDevicesW(lpRasDevinfo:LPRASDEVINFOW;
                         lpcb:LPDWORD;
                         lpcDevices:LPDWORD):DWORD; external KernelDLL name 'RasEnumDevicesW'; // index 203
function RasEnumDevices(lpRasDevinfo:LPRASDEVINFOW;
                        lpcb:LPDWORD;
                        lpcDevices:LPDWORD):DWORD; external KernelDLL name 'RasEnumDevicesW'; // index 203

function RasGetProjectionInfoW(hrasconn:HRASCONN;
                               rasprojection:RASPROJECTION;
                               lpprojection:LPVOID;
                               lpcb:LPDWORD):DWORD; external KernelDLL name 'RasGetProjectionInfoW'; // index 204
function RasGetProjectionInfo(hrasconn:HRASCONN;
                              rasprojection:RASPROJECTION;
                              lpprojection:LPVOID;
                              lpcb:LPDWORD):DWORD; external KernelDLL name 'RasGetProjectionInfoW'; // index 204

function RasGetLinkStatistics(hRasConn:HRASCONN;
                              dwSubEntry:DWORD;
                              lpStatistics:PRAS_STATS):DWORD; external KernelDLL name 'RasGetLinkStatistics'; // index 205

function RasGetDispPhoneNumW(szPhonebook:LPCWSTR;
								                     szEntry:LPCWSTR;
								                     szPhoneNum:LPWSTR;
								                     dwPhoneNumLen:DWORD):DWORD; external KernelDLL name 'RasGetDispPhoneNumW'; // index 206
function RasGetDispPhoneNum(szPhonebook:LPCWSTR;
								                    szEntry:LPCWSTR;
								                    szPhoneNum:LPWSTR;
								                    dwPhoneNumLen:DWORD):DWORD; external KernelDLL name 'RasGetDispPhoneNumW'; // index 206

function RasDevConfigDialogEditW(szDeviceName:LPCWSTR;
                                 szDeviceType:LPCWSTR;
										                       hWndOwner:HWND;
                                 lpDeviceConfigIn:LPVOID;
										                       dwSize:DWORD;
                                 lpDeviceConfigOut:LPVARSTRING):DWORD; external KernelDLL name 'RasDevConfigDialogEditW'; // index 207
function RasDevConfigDialogEdit(szDeviceName:LPCWSTR;
                                szDeviceType:LPCWSTR;
										                      hWndOwner:HWND;
                                lpDeviceConfigIn:LPVOID;
										                      dwSize:DWORD;
                                lpDeviceConfigOut:LPVARSTRING):DWORD; external KernelDLL name 'RasDevConfigDialogEditW'; // index 207

function RasIOControl(hRasConn:LPVOID;
                      dwCode:DWORD;
                      pBufIn:LPBYTE;
                      dwLenIn:DWORD;
                      pBufOut:LPBYTE;
                      dwLenOut:DWORD;
                      pdwActualOut:LPDWORD):DWORD; external KernelDLL name 'RasIOControl'; // index 202

function RasGetEapUserData(hToken:HANDLE;           // access token for user
	                          pszPhonebook:LPCTSTR;     // path to phone book to use
	                          pszEntry:LPCTSTR;         // name of entry in phone book
	                          pbEapData:LPBYTE;        // retrieved data for the user
	                          pdwSizeofEapData:LPDWORD // size of retrieved data
                          ):DWORD; external KernelDLL name 'RasGetEapUserData'; // index 208

function RasSetEapUserData(hToken:HANDLE;           // access token for user
	                          pszPhonebook:LPCTSTR;     // path to phone book to use
	                          pszEntry:LPCTSTR;         // name of entry in phone book
	                          pbEapData:LPBYTE;        // data to store for the user
	                          dwSizeofEapData:DWORD   // size of data
                          ):DWORD; external KernelDLL name 'RasSetEapUserData'; // index 209

function RasGetEapConnectionData(pszPhonebook:LPCTSTR;     // path to phone book to use
	                                pszEntry:LPCTSTR;         // name of entry in phone book
	                                pbEapData:LPBYTE;        // retrieved data for the user
	                                pdwSizeofEapData:LPDWORD // size of retrieved data
                                ):DWORD; external KernelDLL name 'RasGetEapConnectionData'; // index 20A

function RasSetEapConnectionData(pszPhonebook:LPCTSTR;    // path to phone book to use
	                                pszEntry:LPCTSTR;        // name of entry in phone book
	                                pbEapData:LPBYTE;       // data to store for the connection
	                                dwSizeofEapData:DWORD  // size of data
                                ):DWORD; external KernelDLL name 'RasSetEapConnectionData'; // index 20B

implementation

end.