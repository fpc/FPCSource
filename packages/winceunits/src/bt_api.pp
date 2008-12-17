{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }
//------------------------------------------------------------------------------
//
//      Bluetooth client API declarations
//
//
// Module Name:
//
//      bt_api.h
//
// Abstract:
//
//      This file defines client-level APIs to Bluetooth stack
//
//
//------------------------------------------------------------------------------
//
//	Bluetooth client API
//

//
//  Microsoft Windows Mobile 6.0 for PocketPC SDK.
//

unit bt_api;

{$CALLING cdecl}

interface

uses Windows, WinSock2, WinIOCtl, ws2bth;

//
//		COD classes
//
const
      BTH_COD_MAJOR_SERVICE_CLASS_INFORMATION		= $800000;
      BTH_COD_MAJOR_SERVICE_CLASS_TELEPHONY		  = $400000;
      BTH_COD_MAJOR_SERVICE_CLASS_AUDIO			     = $200000;
      BTH_COD_MAJOR_SERVICE_CLASS_OBEX			      = $100000;
      BTH_COD_MAJOR_SERVICE_CLASS_CAPTURE			   = $080000;
      BTH_COD_MAJOR_SERVICE_CLASS_RENDERING		  = $040000;
      BTH_COD_MAJOR_SERVICE_CLASS_NETWORK			   = $020000;
      BTH_COD_MAJOR_SERVICE_CLASS_LIMITED_DISC	= $002000;

      BTH_COD_MAJOR_DEVICE_CLASS_MISC				      = $000000;
      BTH_COD_MAJOR_DEVICE_CLASS_COMPUTER			   = $000100;
      BTH_COD_MAJOR_DEVICE_CLASS_PHONE			      = $000200;
      BTH_COD_MAJOR_DEVICE_CLASS_LAP				       = $000300;
      BTH_COD_MAJOR_DEVICE_CLASS_AUDIO			      = $000400;
      BTH_COD_MAJOR_DEVICE_CLASS_PERIPHERAL		  = $000500;
      BTH_COD_MAJOR_DEVICE_CLASS_UNCLASSIFIED		= $001f00;

      BTH_COD_MINOR_COMPUTER_UNCLASSIFIED			   = $000000;
      BTH_COD_MINOR_COMPUTER_DESKTOP				       = $000004;
      BTH_COD_MINOR_COMPUTER_SERVER				        = $000008;
      BTH_COD_MINOR_COMPUTER_LAPTOP				        = $00000c;
      BTH_COD_MINOR_COMPUTER_HANDHELD				      = $000010;
      BTH_COD_MINOR_COMPUTER_PDA					          = $000014;

      BTH_COD_MINOR_PHONE_UNCLASSIFIED			      = $000000;
      BTH_COD_MINOR_PHONE_CELL					            = $000004;
      BTH_COD_MINOR_PHONE_CORDLESS				         = $000008;
      BTH_COD_MINOR_PHONE_SMART					           = $00000c;
      BTH_COD_MINOR_PHONE_WIRED					           = $000010;

      BTH_COD_MINOR_LAP_AVAILABLE					         = $000000;
      BTH_COD_MINOR_LAP_1_17						             = $000004;
      BTH_COD_MINOR_LAP_17_33						            = $000008;
      BTH_COD_MINOR_LAP_33_50						            = $00000c;
      BTH_COD_MINOR_LAP_50_67						            = $000010;
      BTH_COD_MINOR_LAP_67_83						            = $000014;
      BTH_COD_MINOR_LAP_83_99						            = $000018;
      BTH_COD_MINOR_LAP_NO_SERVICE				         = $00001c;

      BTH_COD_MINOR_AUDIO_UNCLASSIFIED			      = $000000;
      BTH_COD_MINOR_AUDIO_HEADSET					         = $000004;

      BTH_NAMEDEVENT_PAIRING_CHANGED           = 'system/events/bluetooth/PairingChange';
      BTH_NAMEDEVENT_HARDWARE_CHANGED          = 'system/events/bluetooth/HardwareChange';
      BTH_NAMEDEVENT_DEVICEID_CHANGED          = 'system/events/bluetooth/DeviceIdChange';
      BTH_NAMEDEVENT_CONNECTIVITY_CHANGED      = 'system/events/bluetooth/ConnectivityChange';
      BTH_NAMEDEVENT_SECURITY_CHANGED          = 'system/events/bluetooth/SecurityChange';
      BTH_NAMEDEVENT_CONNECTIONS_CHANGED       = 'system/events/bluetooth/ConnectionsChange';
      BTH_NAMEDEVENT_BASEBAND_CHANGED          = 'system/events/bluetooth/BasebandChange';
      BTH_NAMEDEVENT_STACK_INITED              = 'system/events/bluetooth/StackInitialized';
      BTH_NAMEDEVENT_PAN_REFRESH               = 'system/events/bluetooth/pan/refresh';


{$IFNDEF __bt_ddi_H__}
//
//  Attention: also defined in bt_ddi.h! Keep in sync!
//
//	Hardware Status
//
const
      HCI_HARDWARE_UNKNOWN							= 0;
      HCI_HARDWARE_NOT_PRESENT			=	1;
      HCI_HARDWARE_INITIALIZING		=	2;
      HCI_HARDWARE_RUNNING							= 3;
      HCI_HARDWARE_SHUTDOWN						=	4;
      HCI_HARDWARE_ERROR								 = 5;
{$ENDIF __bt_ddi_H__}

const
      BTH_GET_BASEBAND_CONNECTIONS_EX_API_VERSION      = 1;

type
     _BASEBAND_CONNECTION = record
	    		hConnection:USHORT;
	 	    baAddress:BT_ADDR;
	      cDataPacketsPending:longint;
       _flags:UINT;
{
	      UINT		fLinkType		: 1;     // The type of link. Zero (0) indicates SCO, and 1 indicates ACL.
	      UINT		fEncrypted		: 1;    // Encryption is enabled.
	      UINT		fAuthenticated	: 1; // Indicates whether the connection has been authenticated.
	      UINT		fMode			: 3;        // The connection mode.
}
     end;
     BASEBAND_CONNECTION = _BASEBAND_CONNECTION;
     PBASEBAND_CONNECTION = ^_BASEBAND_CONNECTION;

type
     _BASEBAND_CONNECTION_EX = record
	    		hConnection:USHORT;
	 	    baAddress:BT_ADDR;
	      cDataPacketsPending:longint;
       link_type:UCHAR;
       mode:UCHAR;
       _flags:UINT;
{
       UINT        fEncrypted      : 1;
       BOOL        fAuthenticated  : 1;
}
     end;
     BASEBAND_CONNECTION_EX = _BASEBAND_CONNECTION_EX;
     PBASEBAND_CONNECTION_EX = ^_BASEBAND_CONNECTION_EX;

const
      HCI_PAGE_SCAN_TYPE_STANDARD         = $00;
      HCI_PAGE_SCAN_TYPE_INTERLACED       = $01;

      HCI_INQUIRY_SCAN_TYPE_STANDARD      = $00;
      HCI_INQUIRY_SCAN_TYPE_INTERLACED    = $01;


//
// Warning: To use the following Bth* APIs you must link to btdrt.lib.
//          This library may or may not be available in your SDK.
//
// To preserve Win32 compatibility, consider using the Winsock equivalent
// of these functions.
//



const
      BtdrtDll = 'btdrt.dll';

//
//	Management APIs
//
function BthWriteScanEnableMask(mask:byte):longint; external BtdrtDll name 'BthWriteScanEnableMask';

function BthReadScanEnableMask(pmask:LPBYTE):longint; external BtdrtDll name 'BthReadScanEnableMask';

function BthWritePageTimeout(timeout:word):longint; external BtdrtDll name 'BthWritePageTimeout';

function BthReadPageTimeout(ptimeout:LPWORD):longint; external BtdrtDll name 'BthReadPageTimeout';

function BthWriteCOD(cod:dword):longint; external BtdrtDll name 'BthWriteCOD';

function BthReadCOD(pcod:LPDWORD):longint; external BtdrtDll name 'BthReadCOD';

function BthGetRemoteCOD(pbt:PBT_ADDR; pcod:LPDWORD):longint; external BtdrtDll name 'BthGetRemoteCOD';

function BthWriteAuthenticationEnable(ae:byte):longint; external BtdrtDll name 'BthWriteAuthenticationEnable';

function BthReadAuthenticationEnable(pae:LPBYTE):longint; external BtdrtDll name 'BthReadAuthenticationEnable';

function BthWriteLinkPolicySettings(pba:PBT_ADDR; lps:word):longint; external BtdrtDll name 'BthWriteLinkPolicySettings';

function BthReadLinkPolicySettings(pba:PBT_ADDR; plps:LPWORD):longint; external BtdrtDll name 'BthReadLinkPolicySettings';

function BthEnterHoldMode(pba:PBT_ADDR; hold_mode_max:word; hold_mode_min:word; pinterval:LPWORD):longint; external BtdrtDll name 'BthEnterHoldMode';

function BthEnterSniffMode(pba:PBT_ADDR;
                           sniff_mode_max:word;
                           sniff_mode_min:word;
                           sniff_attempt:word;
                           sniff_timeout:word;
                           pinterval:LPWORD):longint; external BtdrtDll name 'BthEnterSniffMode';

function BthExitSniffMode(pba:PBT_ADDR):longint; external BtdrtDll name 'BthExitSniffMode';

function BthEnterParkMode(pba:PBT_ADDR; beacon_max:word; beacon_min:word; pinterval:LPWORD):longint; external BtdrtDll name 'BthEnterParkMode';

function BthExitParkMode(pba:PBT_ADDR):longint; external BtdrtDll name 'BthExitParkMode';

function BthGetCurrentMode(pba:PBT_ADDR; pmode:LPBYTE):longint; external BtdrtDll name 'BthGetCurrentMode';

function BthGetBasebandHandles(cHandles:longint; pHandles:LPWORD; pcHandlesReturned:PLongint):longint; external BtdrtDll name 'BthGetBasebandHandles';

function BthGetBasebandConnections(cConnections:longint; pConnections:PBASEBAND_CONNECTION; pcConnectionsReturned:PLongint):longint; external BtdrtDll name 'BthGetBasebandConnections';

function BthGetBasebandConnectionsEx(dwApiVersion:DWORD;
                                     cConnections:longint;
                                     pConnections:PBASEBAND_CONNECTION_EX;
                                     pcConnectionsReturned:PLongint):longint; external BtdrtDll name 'BthGetBasebandConnectionsEx';

function BthGetAddress(_handle:word; pba:PBT_ADDR):longint; external BtdrtDll name 'BthGetAddress';

function BthReadLocalAddr(pba:PBT_ADDR):longint; external BtdrtDll name 'BthReadLocalAddr';

function BthGetHardwareStatus(pistatus:PLongint):longint; external BtdrtDll name 'BthGetHardwareStatus';

function BthReadLocalVersion(phci_version:LPBYTE;
                             phci_revision:LPWORD;
                             plmp_version:LPBYTE;
                             plmp_subversion:LPWORD;
                             pmanufacturer:LPWORD;
                             plmp_features:LPBYTE):longint; external BtdrtDll name 'BthReadLocalVersion';

function BthReadRemoteVersion(pba:PBT_ADDR;
                              plmp_version:LPBYTE;
                              plmp_subversion:LPWORD;
                              pmanufacturer:LPWORD;
                              plmp_features:LPBYTE):longint; external BtdrtDll name 'BthReadRemoteVersion';

function BthPerformInquiry(LAP:LongWord;
                           _length:byte;
                           num_responses:byte;
                           cBuffer:LongWord;
                           pcDiscoveredDevices:LPDWORD;
                           InquiryList:PBthInquiryResult):longint; external BtdrtDll name 'BthPerformInquiry';

function BthCancelInquiry:longint; external BtdrtDll name 'BthCancelInquiry';

function BthRemoteNameQuery(pba:PBT_ADDR;	cBuffer:LongWord; pcRequired:LPDWORD; szString:PWCHAR):longint; external BtdrtDll name 'BthRemoteNameQuery';

function BthTerminateIdleConnections:longint; external BtdrtDll name 'BthTerminateIdleConnections';

function BthSetInquiryFilter(pba:PBT_ADDR):longint; external BtdrtDll name 'BthSetInquiryFilter';

function BthSetCODInquiryFilter(cod:LongWord; codMask:LongWord):longint; external BtdrtDll name 'BthSetCODInquiryFilter';

function BthClearInquiryFilter:longint; external BtdrtDll name 'BthClearInquiryFilter';

function BthSwitchRole(pbt:PBT_ADDR; usRole:USHORT):longint; external BtdrtDll name 'BthSwitchRole';

function BthGetRole(pbt:PBT_ADDR; pusRole:PUSHORT):longint; external BtdrtDll name 'BthGetRole';

function BthReadRSSI(pbt:PBT_ADDR; pbRSSI:LPBYTE):longint; external BtdrtDll name 'BthReadRSSI';

//
//   Security manager APIs
//
function BthSetPIN(pba:PBT_ADDR; cPinLength:longint; ppin:LPBYTE):longint; external BtdrtDll name 'BthSetPIN';

function BthRevokePIN(pba:PBT_ADDR):longint; external BtdrtDll name 'BthRevokePIN';

type
     TBthLinkKey = packed array[0..15] of byte;
     PBthLinkKey = ^TBthLinkKey;


function BthSetLinkKey(pba:PBT_ADDR; key:TBthLinkKey):longint; external BtdrtDll name 'BthSetLinkKey';

function BthGetLinkKey(pba:PBT_ADDR; key:TBthLinkKey):longint; external BtdrtDll name 'BthGetLinkKey';

function BthRevokeLinkKey(pba:PBT_ADDR):longint; external BtdrtDll name 'BthRevokeLinkKey';

function BthAuthenticate(pba:PBT_ADDR):longint; external BtdrtDll name 'BthAuthenticate';

function BthSetEncryption(pba:PBT_ADDR; fOn:longint):longint; external BtdrtDll name 'BthSetEncryption';

function BthSetSecurityUI(_hEvent:HANDLE; dwStoreTimeout:DWORD; dwProcTimeout:DWORD):longint; external BtdrtDll name 'BthSetSecurityUI';

function BthGetPINRequest(pbt:PBT_ADDR):longint; external BtdrtDll name 'BthGetPINRequest';

function BthRefusePINRequest(pbt:PBT_ADDR):longint; external BtdrtDll name 'BthRefusePINRequest';

function BthAnswerPairRequest(pba:PBT_ADDR; cPinLength:longint; ppin:LPBYTE):longint; external BtdrtDll name 'BthAnswerPairRequest';

function BthPairRequest(pba:PBT_ADDR; cPinLength:longint; ppin:LPBYTE):longint; external BtdrtDll name 'BthAnswerPairRequest';

//
//  Connection APIs
//

function BthCreateACLConnection(pbt:PBT_ADDR; _phandle:LPWORD):longint; external BtdrtDll name 'BthCreateACLConnection';

function BthCreateSCOConnection(pbt:PBT_ADDR; _phandle:LPWORD):longint; external BtdrtDll name 'BthCreateSCOConnection';

function BthCloseConnection(_handle:word):longint; external BtdrtDll name 'BthCloseConnection';

function BthAcceptSCOConnections(fAccept:BOOL):longint; external BtdrtDll name 'BthAcceptSCOConnections';

function BthWritePageScanActivity(pageScanInterval:word; pageScanWindow:word):longint; external BtdrtDll name 'BthWritePageScanActivity';

function BthWriteInquiryScanActivity(inquiryScanInterval:word; inquiryScanWindow:word):longint; external BtdrtDll name 'BthWriteInquiryScanActivity';

function BthReadPageScanActivity(pPageScanInterval:LPWORD; pPageScanWindow:LPWORD):longint; external BtdrtDll name 'BthReadPageScanActivity';

function BthReadInquiryScanActivity(pInquiryScanInterval:LPWORD; pInquiryScanWindow:LPWORD):longint; external BtdrtDll name 'BthReadInquiryScanActivity';

function BthWritePageScanType(pageScanType:byte):longint; external BtdrtDll name 'BthWritePageScanType';

function BthWriteInquiryScanType(inquiryScanType:byte):longint; external BtdrtDll name 'BthWriteInquiryScanType';

function BthReadPageScanType(pPageScanType:LPBYTE):longint; external BtdrtDll name 'BthReadPageScanType';

function BthReadInquiryScanType(pInquiryScanType:LPBYTE):longint; external BtdrtDll name 'BthReadInquiryScanType';

function BthCreateSynchronousConnection(pbt:PBT_ADDR;
                                        pHandle:LPWORD;
                                        txBandwidth:LongWord;
                                        rxBandwidth:LongWord;
                                        maxLatency:word;
                                        voiceSetting:word;
                                        retransmit:byte):longint; external BtdrtDll name 'BthCreateSynchronousConnection';

function BthAcceptSynchronousConnections(fAccept:BOOL):longint; external BtdrtDll name 'BthAcceptSynchronousConnections';


//
//	SDP Name Service APIs
//
type
{
     SdpAttributeRange = _SdpAttributeRange;
     SdpQueryUuid = _SdpQueryUuid;
}

     _WSAQuerySetW = TWSAQuerySetW;

     _WSAESETSERVICEOP = TWSAeSetServiceOp;
     WSAESETSERVICEOP = _WSAESETSERVICEOP;

function BthNsSetService(pSet:LPWSAQUERYSET; op:WSAESETSERVICEOP; dwFlags:DWORD):longint; external BtdrtDll name 'BthNsSetService';

function BthNsLookupServiceBegin(pQuerySet:LPWSAQUERYSET; dwFlags:DWORD; lphLookup:LPHANDLE):longint; external BtdrtDll name 'BthNsLookupServiceBegin';

function BthNsLookupServiceNext(hLookup:HANDLE; dwFlags:DWORD; lpdwBufferLength:LPDWORD; pResults:LPWSAQUERYSET):longint; external BtdrtDll name 'BthNsLookupServiceNext';

function BthNsLookupServiceEnd(hLookup:HANDLE):longint; external BtdrtDll name 'BthNsLookupServiceEnd';

//
//	RFCOMM Apis
//
const
      RFCOMM_PORT_FLAGS_REMOTE_DCB    = $00000001;
      RFCOMM_PORT_FLAGS_KEEP_DCD      = $00000002;
      RFCOMM_PORT_FLAGS_AUTHENTICATE	 = $00000004;
      RFCOMM_PORT_FLAGS_ENCRYPT       = $00000008;

{$IFNDEF __bt_ddi_H__}
//
//  Attention: also defined in bt_ddi.h! Keep in sync!
//
//	channel:
//		RFCOMM_CHANNEL_ALL			accept connection on all channels (default upper layer)
//		RFCOMM_CHANNEL_CLIENT_ONLY	do not accept connections at all (client only)
//		...or channel to restrict connections on
//
const
      RFCOMM_CHANNEL_ALL			      = $00;
      RFCOMM_CHANNEL_MULTIPLE		  = $fe;
      RFCOMM_CHANNEL_CLIENT_ONLY	= $ff;
{$ENDIF __bt_ddi_H__}

type
     _portemu_port_params = record
	      channel:longint;
	      flocal:longint;
       device:BT_ADDR;
	      imtu:longint;
	      iminmtu:longint;
	      imaxmtu:longint;
	      isendquota:longint;
	      irecvquota:longint;
				   uuidService:GUID;
	      uiportflags:LongWord;
     end;
     PORTEMUPortParams = _portemu_port_params;

//	Bluetooth serial IOCTLs are cross-defined in pegdser.h to reserve spot there.
const
      IOCTL_BLUETOOTH_GET_RFCOMM_CHANNEL	= (FILE_DEVICE_SERIAL_PORT shl 16) or
                                           (FILE_ANY_ACCESS shl 14) or
                                           (24 shl 2) or
                                           METHOD_BUFFERED;
// #define IOCTL_BLUETOOTH_GET_RFCOMM_CHANNEL	CTL_CODE(FILE_DEVICE_SERIAL_PORT,24,METHOD_BUFFERED,FILE_ANY_ACCESS)

      IOCTL_BLUETOOTH_GET_PEER_DEVICE    = (FILE_DEVICE_SERIAL_PORT shl 16) or
                                           (FILE_ANY_ACCESS shl 14) or
                                           (25 shl 2) or
                                           METHOD_BUFFERED;
// #define IOCTL_BLUETOOTH_GET_PEER_DEVICE 	CTL_CODE(FILE_DEVICE_SERIAL_PORT,25,METHOD_BUFFERED,FILE_ANY_ACCESS)

//
// Bluetooth notification system
//
function RequestBluetoothNotifications(
                                       dwClass:DWORD; // class of notifications to register for
                                       hMsgQ:HANDLE   // message queue created by caller
                                      ):HANDLE; external BtdrtDll name 'RequestBluetoothNotifications';

function StopBluetoothNotifications(
                                    h:HANDLE // Handle returned from RequestBluetoothNotifications
                                   ):BOOL; external BtdrtDll name 'StopBluetoothNotifications';

type
     _BTEVENT = record
       dwEventId:DWORD;        // Event ID
       dwReserved:DWORD;       // Reserved
       baEventData:array[0..63] of byte;   // Event Data
     end;
     BTEVENT = _BTEVENT;
     PBTEVENT = ^_BTEVENT;

//
// Class of events to notify connections going up/down
// and other connection-related changes (role, mode).
//
const
      BTE_CLASS_CONNECTIONS	= 1;

      BTE_CONNECTION			     = 100;
      BTE_DISCONNECTION		   = 101;
      BTE_ROLE_SWITCH			    = 102;
      BTE_MODE_CHANGE			    = 103;
      BTE_PAGE_TIMEOUT		    = 104;

type
     BT_CONNECT_EVENT = record
       dwSize:DWORD;         // To keep track of version
       hConnection:USHORT;   // Baseband connection handle
       bta:BT_ADDR;          // Address of remote device
       ucLinkType:UCHAR;     // Link Type (ACL/SCO)
       ucEncryptMode:UCHAR;  // Encryption mode
     end;
     PBT_CONNECT_EVENT = ^BT_CONNECT_EVENT;

type
     BT_DISCONNECT_EVENT = record
       dwSize:DWORD;         // To keep track of version
       hConnection:USHORT;   // Baseband connection handle
       ucReason:UCHAR;       // Reason for disconnection
     end;
     PBT_DISCONNECT_EVENT = ^BT_DISCONNECT_EVENT;

type
     BT_ROLE_SWITCH_EVENT = record
       dwSize:DWORD;         // To keep track of version
       bta:BT_ADDR;          // Address of remote device
       _flags:UINT;
{
       UINT fRole : 1;       // New Role (master/slave)
}
     end;
     PBT_ROLE_SWITCH_EVENT = ^BT_ROLE_SWITCH_EVENT;

type
     BT_MODE_CHANGE_EVENT = record
       dwSize:DWORD;         // To keep track of version
       hConnection:USHORT;   // Baseband connection handle
       bta:BT_ADDR;          // Address of remote device
       bMode:byte;           // Power mode (sniff, etc)
       usInterval:USHORT;    // Power mode interval
     end;
     PBT_MODE_CHANGE_EVENT = ^BT_MODE_CHANGE_EVENT;

//
// Class of events to notify changes in pairing.
//
const
      BTE_CLASS_PAIRING		= 2;

      BTE_KEY_NOTIFY			  = 200;
      BTE_KEY_REVOKED			 = 201;

type
     BT_LINK_KEY_EVENT = record
       dwSize:DWORD;        // To keep track of version
       bta:BT_ADDR;         // Address of remote device
       link_key:array[0..15] of UCHAR;  // Link key data
       key_type:UCHAR;      // Link key type
     end;
     PBT_LINK_KEY_EVENT = ^BT_LINK_KEY_EVENT;

//
// Class of events to notify changes specific to the 
// local device (cod, name)
//
const
      BTE_CLASS_DEVICE		= 4;

      BTE_LOCAL_NAME			 = 300;
      BTE_COD					      = 301;

//
// Class of events to notify change of state of the
// core stack.
//
const
      BTE_CLASS_STACK			= 8;

      BTE_STACK_UP			   = 400;
      BTE_STACK_DOWN			 = 401;

//
// Class of events to notify change of state of AVDTP
//
const
      BTE_CLASS_AVDTP			= 16;

      BTE_AVDTP_STATE			= 500;

      BT_AVDTP_STATE_DISCONNECTED     = 0;
      BT_AVDTP_STATE_SUSPENDED        = 1;
      BT_AVDTP_STATE_STREAMING        = 2;

type
     BT_AVDTP_STATE_CHANGE = record
       dwSize:DWORD;       // To keep track of version
       bta:BT_ADDR;        // Address of remote device
       dwState:DWORD;      // New state of the AVDTP stream
     end;
     PBT_AVDTP_STATE_CHANGE = ^BT_AVDTP_STATE_CHANGE;

//
// Class of events to notify change of PAN state
//
const
      BTE_CLASS_PAN           = 32;

      BTE_PAN_CONNECTIONS     = 600;

type
     BT_PAN_NUM_CONNECTIONS = record
       dwSize:DWORD;       // To keep track of version
       NumConnections:DWORD; // Number of peers connected
     end;
     PBT_PAN_NUM_CONNECTIONS = ^BT_PAN_NUM_CONNECTIONS;

//
// PAN APIs
//
function BthActivatePAN(fActivate:BOOL):longint; external BtdrtDll name 'BthActivatePAN';


function rfRegisterDevice(lpszType:LPCWSTR; dwIndex:DWORD; lpszLib:LPCWSTR; dwInfo:DWORD):HANDLE; external KernelDLL name 'RegisterDevice'; // index 14F

function rfDeregisterDevice(hDevice:HANDLE):BOOL; external KernelDLL name 'DeregisterDevice';

function rfCreateFile(lpFileName:LPCTSTR;
                      dwDesiredAccess:DWORD;
                      dwShareMode:DWORD;
                      lpSecurityAttributes:LPSECURITY_ATTRIBUTES;
                      dwCreationDisposition:DWORD;
                      dwFlagsAndAttributes:DWORD;
                      hTemplateFile:HANDLE):HANDLE; external KernelDLL name 'CreateFileW';

function rfReadFile(_hFile:HANDLE;                // handle to file
                    lpBuffer:LPVOID;             // data buffer
                    nNumberOfBytesToRead:DWORD;  // number of bytes to read
                    lpNumberOfBytesRead:LPDWORD; // number of bytes read
                    lpOverlapped:LPOVERLAPPED    // overlapped buffer
                   ):BOOL; external KernelDLL name 'ReadFile'; // index F4

function rfWriteFile(_hFile:HANDLE;                    // handle to file
                     lpBuffer:LPCVOID;                // data buffer
                     nNumberOfBytesToWrite:DWORD;     // number of bytes to write
                     lpNumberOfBytesWritten:LPDWORD;  // number of bytes written
                     lpOverlapped:LPOVERLAPPED        // overlapped buffer
                    ):BOOL; external KernelDLL name 'WriteFile';

function rfCloseHandle(_hFile:HANDLE):BOOL; external KernelDLL name 'CloseHandle';

implementation

end.