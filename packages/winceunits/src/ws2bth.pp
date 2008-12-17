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
//        ws2bth.h
//
// Abstract:
//
//        Winsock 2 Bluetooth Annex definitions.
//

//
//  Microsoft Windows Mobile 6.0 for PocketPC SDK.
//

unit ws2bth;

{$CALLING cdecl}

interface

uses Windows;

const
      BTH_MAX_NAME_SIZE          = 248;    // max length of device friendly name.

// Some explanations of the BT_ADDR structure.
// Taken from module bt_ddi.h which is a part of MS WinCE 6.0 Platform Builder
// and defines interfaces between layers of Bluetooth device driver.

//  ------------------------------------ BD_ADDR------------------------------------
//
{$PACKRECORDS 1} // #pragma pack(push, 1)
type
     __bd_addr = record
       SAP:Cardinal; // Significant address part.
                     // The lower 24 bits are the 'Lower address part' and
                     // the higher 8 bits are the 'Upper address part'.
       NAP:word;     // Non-significant address part.
     end;
     BD_ADDR = __bd_addr;
(* Declared as:
typedef struct  __bd_addr {
    union {
        struct {
            unsigned int LAP : 24;  // Lower address part
            unsigned int UAP : 8;   // Upper address part
        };
        unsigned int SAP;           // Significant address part
    };

    unsigned short NAP;             // Non-significant address part
} BD_ADDR;
*)
{$PACKRECORDS DEFAULT} // #pragma pack(pop)

const
      BLUETOOTH_TRANSPORT_NAME = 'MSBT';

// End of BT_ADDR explanation.

type
     BT_ADDR = ULONGLONG;
     PBT_ADDR = ^BT_ADDR;
     TBtAddr = BT_ADDR;
     PBtAddr = ^TBtAddr;

type
     BT_COD = ULONG;
     TBtCod = BT_COD;

type
     BT_LAP = ULONG;
     TBtLap = BT_LAP;

const
      NAP_MASK                = ULONGLONG($FFFF00000000);
      SAP_MASK                = ULONGLONG($0000FFFFFFFF);

      NAP_BIT_OFFSET          = 8 * 4;
      SAP_BIT_OFFSET          = 0;

// Macro functions.      
function GET_NAP(const _bt_addr:BT_ADDR):USHORT;

function GET_SAP(const _bt_addr:BT_ADDR):ULONG;

function SET_NAP(const _nap:USHORT):ULONGLONG;

function SET_SAP(const _sap:ULONG):ULONGLONG;

function SET_NAP_SAP(const _nap:USHORT; const _sap:ULONG):ULONGLONG; 


// Turn 1 byte packing of structures on
const
      L2CAP_MAX_MTU  = 65535;

const
      BT_ADDR_NULL       = $000000000000;
      BT_ADDR_IAC_FIRST  = $9E8B00;
      BT_ADDR_IAC_LAST   = $9E8B3f;
      BT_ADDR_LIAC       = $9E8B00;
      BT_ADDR_GIAC       = $9E8B33;

      BT_PORT_NULL       = 0;
      BT_PORT_WILDCARD   = 0;
      BT_PORT_ANY        = -1;
      BT_PORT_MIN        = $1;
      BT_PORT_MAX        = $ffff;
      BT_PORT_DYN_FIRST  = $1001;
      BT_PORT_EXCL_BIT   = $0100;

const
      AF_BTH        = 32;
      AF_BT         = AF_BTH;
      WINDOWS_AF_BT = AF_BTH;
      AF_MAX        = 33;

      PF_BTH        = AF_BTH;
      PF_MAX        = AF_BTH;


      NS_BTH  = 16;

const
      SVCID_BTH_PROVIDER:GUID = (D1: $06AA63E0; D2: $7D60; D3: $41FF; D4: ($AF, $B2, $3E, $E6, $D2, $D9, $39, $2D));
//      SVCID_BTH_PROVIDER:GUID = '{06AA63E0-7D60-41FF-AFB2-3EE6D2D9392D}';


// Bluetooth protocol #s are assigned according to the Bluetooth
// Assigned Numbers portion of the Bluetooth Specification
const
      BTHPROTO_RFCOMM     = $0003;
      BTHPROTO_L2CAP      = $0100;

      SDP_ENUMDEVICES     = $00000010;
      SDP_SET             = $00000011;
      SDP_QUERY           = $00000012;

      RFCOMM_FLOW_CONTROL = $00000013;

      SOL_RFCOMM          = $03;
      SOL_BTHTDI          = $100;
      SOL_SDP             = $0101;

type
     _BTH_SOCKOPT_SECURITY = record
       iLength:longint;  // == 0 or 16 for link key, 0<=..<=16 for PIN. 0 = revoke
       btAddr:BT_ADDR;
       caData:array[0..15] of byte;
     end;
     BTH_SOCKOPT_SECURITY = _BTH_SOCKOPT_SECURITY;
     PBTH_SOCKOPT_SECURITY = ^_BTH_SOCKOPT_SECURITY;

type
     _BTH_LOCAL_VERSION = record
       hci_version:byte;
       hci_revision:word;
       lmp_version:byte;
       lmp_subversion:word;
       manufacturer:word;
       lmp_features:array[0..7] of byte;
     end;
     BTH_LOCAL_VERSION = _BTH_LOCAL_VERSION;
     PBTH_LOCAL_VERSION = ^_BTH_LOCAL_VERSION;

type
     _BTH_REMOTE_VERSION = record
       lmp_version:byte;
       lmp_subversion:word;
       manufacturer:word;
       lmp_features:array[0..7] of byte;
     end;
     BTH_REMOTE_VERSION = _BTH_REMOTE_VERSION;
     PBTH_REMOTE_VERSION = ^_BTH_REMOTE_VERSION;

type
     _BTH_REMOTE_NAME = record
       bt:BT_ADDR;
       szNameBuffer:array[0..BTH_MAX_NAME_SIZE-1] of WideChar;
     end;
     BTH_REMOTE_NAME = _BTH_REMOTE_NAME;
     PBTH_REMOTE_NAME = ^_BTH_REMOTE_NAME;

type
     _BTH_HOLD_MODE = record
       hold_mode_max:word;
       hold_mode_min:word;
       interval:word;    // out
     end;
     BTH_HOLD_MODE = _BTH_HOLD_MODE;
     PBTH_HOLD_MODE = ^_BTH_HOLD_MODE;

type
     _BTH_SNIFF_MODE = record
       sniff_mode_max:word;
       sniff_mode_min:word;
       sniff_attempt:word;
       sniff_timeout:word;
       interval:word;    // out
     end;
     BTH_SNIFF_MODE = _BTH_SNIFF_MODE;
     PBTH_SNIFF_MODE = ^_BTH_SNIFF_MODE;

type
     _BTH_PARK_MODE = record
       beacon_max:word;
       beacon_min:word;
       interval:word;    // out
     end;
     BTH_PARK_MODE = _BTH_PARK_MODE;
     PBTH_PARK_MODE = ^_BTH_PARK_MODE;

const
      SO_BTH_AUTHENTICATE             = $00000001;  // optlen=0, optval ignored
      SO_BTH_ENCRYPT                      = $00000002;  // optlen=sizeof(unsigned int), optval = &(unsigned int)TRUE/FALSE
      SO_BTH_SET_PIN                      = $00000003;  // bound only! survives socket! optlen=sizeof(BTH_SOCKOPT_SECURITY), optval=&BTH_SOCKOPT_SECURITY
      SO_BTH_SET_LINK                    = $00000004;   // bound only! survives socket! optlen=sizeof(BTH_SOCKOPT_SECURITY), optval=&BTH_SOCKOPT_SECURITY
      SO_BTH_GET_LINK                    = $00000005;   // bound only! optlen=sizeof(BTH_SOCKOPT_SECURITY), optval=&BTH_SOCKOPT_SECURITY
      SO_BTH_SET_MTU                      = $00000006;  // unconnected only! optlen=sizeof(unsigned int), optval = &mtu
      SO_BTH_GET_MTU                      = $00000007;  // optlen=sizeof(unsigned int), optval = &mtu
      SO_BTH_SET_MTU_MAX               = $00000008; // unconnected only! optlen=sizeof(unsigned int), optval = &max. mtu
      SO_BTH_GET_MTU_MAX               = $00000009; // bound only! optlen=sizeof(unsigned int), optval = &max. mtu
      SO_BTH_SET_MTU_MIN               = $0000000a; // unconnected only! optlen=sizeof(unsigned int), optval = &min. mtu
      SO_BTH_GET_MTU_MIN               = $0000000b; // bound only! optlen=sizeof(unsigned int), optval = &min. mtu
      SO_BTH_SET_XON_LIM               = $0000000c; // optlen=sizeof(unsigned int), optval = &xon limit (set flow off)
      SO_BTH_GET_XON_LIM               = $0000000d; // optlen=sizeof(unsigned int), optval = &xon
      SO_BTH_SET_XOFF_LIM             = $0000000e;  // optlen=sizeof(unsigned int), optval = &xoff limit (set flow on)
      SO_BTH_GET_XOFF_LIM             = $0000000f;  // optlen=sizeof(unsigned int), optval = &xoff
      SO_BTH_SET_SEND_BUFFER        = $00000010;    // optlen=sizeof(unsigned int), optval = &max buffered size for send
      SO_BTH_GET_SEND_BUFFER        = $00000011;    // optlen=sizeof(unsigned int), optval = &max buffered size for send
      SO_BTH_SET_RECV_BUFFER        = $00000012;    // optlen=sizeof(unsigned int), optval = &max buffered size for recv
      SO_BTH_GET_RECV_BUFFER        = $00000013;    // optlen=sizeof(unsigned int), optval = &max buffered size for recv
      SO_BTH_GET_V24_BR             = $00000014;    // connected only! optlen=2*sizeof(unsigned int), optval = &{v24 , br}
      SO_BTH_GET_RLS                      = $00000015;  // connected only! optlen=sizeof(unsigned int), optval = &rls
      SO_BTH_SEND_MSC                    = $00000016;   // connected only! optlen=2*sizeof(unsigned int), optval = &{v24, br}
      SO_BTH_SEND_RLS                    = $00000017;   // connected only! optlen=sizeof(unsigned int), optval = &rls
      SO_BTH_GET_FLOW_TYPE        = $00000018;  // connected only! optlen=sizeof(unsigned int), optval=&1=credit-based, 0=legacy
      SO_BTH_SET_PAGE_TO               = $00000019; // no restrictions. optlen=sizeof(unsigned int), optval = &page timeout
      SO_BTH_GET_PAGE_TO               = $0000001a; // no restrictions. optlen=sizeof(unsigned int), optval = &page timeout
      SO_BTH_SET_SCAN                    = $0000001b;   // no restrictions. optlen=sizeof(unsigned int), optval = &scan mode
      SO_BTH_GET_SCAN                    = $0000001c;   // no restrictions. optlen=sizeof(unsigned int), optval = &scan mode
      SO_BTH_SET_COD                      = $0000001d;  // no restrictions. optlen=sizeof(unsigned int), optval = &cod
      SO_BTH_GET_COD                      = $0000001e;  // no restrictions. optlen=sizeof(unsigned int), optval = &cod
      SO_BTH_GET_LOCAL_VER        = $0000001f;  // no restrictions. optlen=sizeof(BTH_LOCAL_VERSION), optval = &BTH_LOCAL_VERSION
      SO_BTH_GET_REMOTE_VER      = $00000020;   // connected only! optlen=sizeof(BTH_REMOTE_VERSION), optval = &BTH_REMOTE_VERSION
      SO_BTH_GET_AUTHN_ENABLE   = $00000021;    // no restrictions. optlen=sizeof(unsigned int), optval = &authentication enable
      SO_BTH_SET_AUTHN_ENABLE   = $00000022;    // no restrictions. optlen=sizeof(unsigned int), optval = &authentication enable
      SO_BTH_SET_READ_REMOTE_NAME   = $00000023;    // no restrictions. optlen=sizeof(BTH_REMOTE_NAME), optval=&BTH_REMOTE_NAME
      SO_BTH_GET_LINK_POLICY            = $00000024;    // connected only! optlen=sizeof(unsigned int), optval = &link policy
      SO_BTH_SET_LINK_POLICY            = $00000025;    // connected only! optlen=sizeof(unsigned int), optval = &link policy
      SO_BTH_ENTER_HOLD_MODE            = $00000026; // connected only! optlen=sizeof(BTH_HOLD_MODE), optval = &BTH_HOLD_MODE
      SO_BTH_ENTER_SNIFF_MODE          = $00000027; // connected only! optlen=sizeof(BTH_SNIFF_MODE), optval = &BTH_SNIFF_MODE
      SO_BTH_EXIT_SNIFF_MODE            = $00000028; // connected only! optlen=0, optval - ignored
      SO_BTH_ENTER_PARK_MODE            = $00000029; // connected only! optlen=sizeof(BTH_PARK_MODE), optval = &BTH_PARK_MODE
      SO_BTH_EXIT_PARK_MODE          = $0000002a; // connected only! optlen=0, optval - ignored
      SO_BTH_GET_MODE                        = $0000002b;   // connected only! optlen=sizeof(int), optval = &mode

type
     _SOCKADDR_BTH = record
       addressFamily:USHORT;
       btAddr:BT_ADDR;
       serviceClassId:GUID;
       port:ULONG;
     end;
     SOCKADDR_BTH = _SOCKADDR_BTH;
     PSOCKADDR_BTH = ^_SOCKADDR_BTH;

type
     __bth_inquiry_result = record
       ba:BT_ADDR;
       cod:LongWord;
       clock_offset:word;
       page_scan_mode:byte;
       page_scan_period_mode:byte;
       page_scan_repetition_mode:byte;
     end;
     BthInquiryResult = __bth_inquiry_result;
     PBthInquiryResult = ^BthInquiryResult;

implementation

function GET_NAP(const _bt_addr:BT_ADDR):USHORT; inline;
begin
  GET_NAP:=USHORT((_bt_addr and NAP_MASK) shr NAP_BIT_OFFSET);
end;

function GET_SAP(const _bt_addr:BT_ADDR):ULONG; inline;
begin
  GET_SAP:=ULONG((_bt_addr and SAP_MASK) shr SAP_BIT_OFFSET);
end;

function SET_NAP(const _nap:USHORT):ULONGLONG; inline;
begin
  SET_NAP:=ULONGLONG(_nap shl NAP_BIT_OFFSET);
end;

function SET_SAP(const _sap:ULONG):ULONGLONG; inline;
begin
  SET_SAP:=ULONGLONG(_sap shr SAP_BIT_OFFSET);
end;

function SET_NAP_SAP(const _nap:USHORT; const _sap:ULONG):ULONGLONG; inline;
begin
  SET_NAP_SAP:=SET_NAP(_nap) or SET_SAP(_sap);
end;

end.