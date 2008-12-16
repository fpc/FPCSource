{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }

unit wap;

interface

uses
  Windows;
  
{$calling cdecl}

//*********************************************************
//
// Constants
//
//*********************************************************

const
  wapdll                 = 'wap.dll';
  //*******************************************************
  FACILITY_WAP           = $500;
  //WTLS errors
  WAP_E_WTLS             = (SEVERITY_ERROR shl 31) or (FACILITY_WAP shl 16) or $0001;
  WAP_E_WTLS_CERTIFICATE = (SEVERITY_ERROR shl 31) or (FACILITY_WAP shl 16) or $0002;
  WAP_E_WTLS_HANDSHAKE   = (SEVERITY_ERROR shl 31) or (FACILITY_WAP shl 16) or $0003;
  WAP_E_WTLS_CRYPTERROR  = (SEVERITY_ERROR shl 31) or (FACILITY_WAP shl 16) or $0004;
  WAP_E_WTLS_BADMAC      = (SEVERITY_ERROR shl 31) or (FACILITY_WAP shl 16) or $0005;
  WAP_E_NO_UDP_ACCESS    = (SEVERITY_ERROR shl 31) or (FACILITY_WAP shl 16) or $00100;

//Maximum sizes
const
  MAX_WAP_ADDRESS_LENGTH             = 64;
//WDP field identifiers
  WDP_FIELD_NONE                     = $00000000;
  WDP_FIELD_SOURCEADDRESS            = $00000001;
  WDP_FIELD_SOURCEPORT               = $00000002;
  WDP_FIELD_DESTINATIONADDRESS       = $00000004;
  WDP_FIELD_DESTINATIONPORT          = $00000008;
  WDP_FIELD_USERDATA                 = $00000010;
  WDP_FIELD_ERRORCODE                = $00000020;
//WTP field identifiers
  WTP_FIELD_NONE                     = $00000000;
  WTP_FIELD_SOURCEADDRESS            = $00000001;
  WTP_FIELD_SOURCEPORT               = $00000002;
  WTP_FIELD_DESTINATIONADDRESS       = $00000004;
  WTP_FIELD_DESTINATIONPORT          = $00000008;
  WTP_FIELD_USERACKNOWLEDGEMENT      = $00000010;
  WTP_FIELD_WANTSECURETRANSACTION    = $00000020;
  WTP_FIELD_USERDATA                 = $00000040;
  WTP_FIELD_CLASSTYPE                = $00000080;
  WTP_FIELD_EXITINFO                 = $00000100;
  WTP_FIELD_HANDLE                   = $00000200;
  WTP_FIELD_ABORTCODE                = $00000400;
  WTP_FIELD_TRANSACTIONSECURITYLEVEL = $00000800;
  INVALID_WTP_TRANSACTION_HANDLE     = 0;

//****************************************************************
//
// Types
//
//****************************************************************

type
  WAP_LAYER = (
    WAP_LAYER_WDP = 1,
    WAP_LAYER_WTLS, 
    WAP_LAYER_WTP, 
    WAP_LAYER_WSP, 
    WAP_LAYER_WAE);
  PWAP_LAYER = ^WAP_LAYER;

//WAP primitives
  WAP_PRIMITIVE_ID = (
    WAP_PRIMITIVE_ID_T_DUNITDATA = 1);
  PWAP_PRIMITIVE_ID = ^WAP_PRIMITIVE_ID;

//WAP primitive type
  WAP_PRIMITIVE_TYPE = (
    WAP_PRIMITIVE_TYPE_REQUEST   = 1,
    WAP_PRIMITIVE_TYPE_INDICATION, 
    WAP_PRIMITIVE_TYPE_RESPONSE, 
    WAP_PRIMITIVE_TYPE_CONFIRM);
  PWAP_PRIMITIVE_TYPE = ^WAP_PRIMITIVE_TYPE;

//WAP address types
  WAP_ADDRESS_TYPE = (
    WAP_ADDRESS_TYPE_UDP = 1,
    WAP_ADDRESS_TYPE_GSM_SMS);
  PWAP_ADDRESS_TYPE = ^WAP_ADDRESS_TYPE;

//WAP address structure
  WAP_ADDRESS_tag = record
    watAddressType: WAP_ADDRESS_TYPE;
    ptsAddress    : Array[0..MAX_WAP_ADDRESS_LENGTH-1] of TCHAR;
  end {WAP_ADDRESS_tag};
  WAP_ADDRESS  = WAP_ADDRESS_tag;
  PWAP_ADDRESS = ^WAP_ADDRESS_tag;

  WAP_HANDLE  = LongInt;
  PWAP_HANDLE = ^WAP_HANDLE;

//WTP transaction class types
  WTP_TRANSACTION_CLASS_TYPE = (
    WTP_TRANSACTION_CLASS_TYPE_0 = 1,
    WTP_TRANSACTION_CLASS_TYPE_1, 
    WTP_TRANSACTION_CLASS_TYPE_2);
  PWTP_TRANSACTION_CLASS_TYPE = ^WTP_TRANSACTION_CLASS_TYPE;

//WTP secure transaction (WTLS) security levels
  WTP_TRANSACTION_SECURITY_LEVEL = (
    WTP_TRANSACTION_SECURITY_LEVEL_NOTSECURE   = 1,
    WTP_TRANSACTION_SECURITY_LEVEL_SECURENOAUTH, 
    WTP_TRANSACTION_SECURITY_LEVEL_GATEWAYAUTH);
  PWTP_TRANSACTION_SECURITY_LEVEL = ^WTP_TRANSACTION_SECURITY_LEVEL;

  WTP_TRANSACTION_HANDLE = LongInt;

//WAP primitives
//Base primitive type - all other primitive types derive from this one
  wap_primitive_base_tag = record
    wpiPrimitiveID  : WAP_PRIMITIVE_ID;
    wptPrimitiveType: WAP_PRIMITIVE_TYPE;
    dwValidFields   : LongInt;
  end {wap_primitive_base_tag};
  WAP_PRIMITIVE_BASE  = wap_primitive_base_tag;
  PWAP_PRIMITIVE_BASE = ^wap_primitive_base_tag;

//T-DUnitdata primitive (WDP)
  wdp_unitdata_tag = record
    wpiPrimitiveID      : WAP_PRIMITIVE_ID;
    wptPrimitiveType    : WAP_PRIMITIVE_TYPE;
    dwValidFields       : LongInt;
    waSourceAddress     : WAP_ADDRESS;
    dwSourcePort        : LongInt;
    waDestinationAddress: WAP_ADDRESS;
    dwDestinationPort   : LongInt;
    pbUserData          : PBYTE;
    dwUserDataSize      : LongInt;
  end {wdp_unitdata_tag};
  WDP_INITDATA  = wdp_unitdata_tag;
  PWDP_INITDATA = ^wdp_unitdata_tag;

//T-DError primitive (WDP)
//typedef struct wap_primitive_t_derror_tag
//WAP_PRIMITIVE_ID wpiPrimitiveID;
//WAP_PRIMITIVE_TYPE wptPrimitiveType;
//DWORD dwValidFields;
//WAP_ADDRESS waSourceAddress;
//DWORD dwSourcePort;
//WAP_ADDRESS waDestinationAddress;
//DWORD dwDestinationPort;
//HRESULT hrErrorCode;
//WAP_PRIMITIVE_T_DERROR;
//TR-Invoke primitive (WTP)

  wtp_invoke_tag = record
    wpiPrimitiveID        : WAP_PRIMITIVE_ID;
    wptPrimitiveType      : WAP_PRIMITIVE_TYPE;
    dwValidFields         : LongInt;
    waSourceAddress       : WAP_ADDRESS;
    dwSourcePort          : LongInt;
    waDestinationAddress  : WAP_ADDRESS;
    dwDestinationPort     : LongInt;
    bUserAcknowledgement  : Bool;
    bWantSecureTransaction: Bool;
    pbUserData            : PBYTE;
    dwUserDataSize        : LongInt;
    wtctClassType         : WTP_TRANSACTION_CLASS_TYPE;
    pbExitInfo            : PBYTE;
    dwExitInfoSize        : LongInt;
    wthTransactionHandle  : WTP_TRANSACTION_HANDLE;
  end {wtp_invoke_tag};
  WTP_INVOKE  = wtp_invoke_tag;
  PWTP_INVOKE = ^wtp_invoke_tag;

//TR-Result primitive (WTP)
  wtp_result_tag = record
    wpiPrimitiveID              : WAP_PRIMITIVE_ID;
    wptPrimitiveType            : WAP_PRIMITIVE_TYPE;
    dwValidFields               : LongInt;
    pbUserData                  : PBYTE;
    dwUserDataSize              : LongInt;
    pbExitInfo                  : PBYTE;
    dwExitInfoSize              : LongInt;
    wthTransactionHandle        : WTP_TRANSACTION_HANDLE ;
    wtslTransactionSecurityLevel: WTP_TRANSACTION_SECURITY_LEVEL;
  end {wtp_result_tag};
  WTP_RESULT  = wtp_result_tag;
  PWTP_RESULT = ^wtp_result_tag;

//TR-Abort primitive (WTP)
  wtp_abort_tag = record
    wpiPrimitiveID      : WAP_PRIMITIVE_ID;
    wptPrimitiveType    : WAP_PRIMITIVE_TYPE;
    dwValidFields       : LongInt;
    bAbortCode          : BYTE;
    wthTransactionHandle: WTP_TRANSACTION_HANDLE ;
  end {wtp_abort_tag};
  WTP_ABORT  = wtp_abort_tag;
  PWTP_ABORT = ^wtp_abort_tag;

//************************************************************
//
// WinAPI functions
//
//************************************************************

function WapOpen(const wlLayer: WAP_LAYER;
                 const dwLocalPort: LongInt;
                 const pwhHandle: PWAP_HANDLE;
                 const phMessageAvailableEvent: PHandle): HRESULT; external wapdll name 'WapOpen';
function WapClose(const whHandle: WAP_HANDLE): HRESULT; external wapdll name 'WapClose';
function WapSend(const whHandle: WAP_HANDLE;
                 const pwpbPrimitive: PWAP_PRIMITIVE_BASE): HRESULT; external wapdll name 'WapSend';
function WapGetNextPrimitiveSize(const whHandle: WAP_HANDLE;
                                 const pdwNextPrimitiveSize: PLongInt): HRESULT; external wapdll name 'WapGetNextPrimitiveSize';
function WapRead(const whHandle: WAP_HANDLE;
                 const pwpbPrimitiveBuffer: WAP_PRIMITIVE_BASE;
                 const dwPrimitiveBufferSize: LongInt): HRESULT; external wapdll name 'WapRead';
function WapPing(const pwaAddress: PWAP_ADDRESS;
                 const wIdentifier: Word;
                 const dwSendDataSize: LongInt;
                 const pdwReceiveDataSize: PLongInt;
                 const dwTimeout: LongInt): HRESULT; external wapdll name 'WapPing';

implementation

end.
