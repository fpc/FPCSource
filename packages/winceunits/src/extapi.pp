{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2009 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

********************************************************************* }

//**********************************************************************/
//*                                                                    */
//* EXTAPI.H - Extended TAPI header file.                              */
//* Extended TAPI extends wireless functionality to include such       */
//* things as asking for signal strength, choosing the cellular        */
//* network, and more. ExTAPI works with Telephony API (TAPI) and uses */
//* all of the TAPI line devices. ExTAPI operations are available only */
//* if the application has successfully negotiated a device-specific   */
//* extension version with lineNegotiateExtVersion and successfully    */
//* obtained a line device handle with lineOpen.                       */
//*                                                                    */
//**********************************************************************/

//
//  Microsoft Windows Mobile 6.0 for PocketPC SDK.
//


unit extapi;

{$CALLING cdecl}

interface

uses Windows, TAPI;

const
// ExTAPI LINEERR_ constants
      LINEERR_INCORRECTPASSWORD               = $80010001;

// Line barring modes
      LINEBARRMODE_OUT                        = $00000001;
      LINEBARRMODE_OUT_INT                    = $00000002;
      LINEBARRMODE_OUT_INTEXTOHOME            = $00000004;
      LINEBARRMODE_IN                         = $00000008;
      LINEBARRMODE_IN_ROAM                    = $00000010;
      LINEBARRMODE_IN_NOTINSIM                = $00000020;
      LINEBARRMODE_ALL                        = $00000040;
      LINEBARRMODE_ALL_OUT                    = $00000080;
      LINEBARRMODE_ALL_IN                     = $00000100;

// Line call-waiting states
      LINECALLWAITING_ENABLED                 = $00000001;
      LINECALLWAITING_DISABLED                = $00000002;

// Line capability classes
      LINECAPSCLASS_VOICE                     = $00000001;
      LINECAPSCLASS_DATA                      = $00000002;
      LINECAPSCLASS_FAX                       = $00000004;
      LINECAPSCLASS_SMS                       = $00000008;
      LINECAPSCLASS_SYNCDATA                  = $00000010;
      LINECAPSCLASS_ASYNCDATA                 = $00000020;
      LINECAPSCLASS_PACKET                    = $00000040;
      LINECAPSCLASS_PAD                       = $00000080;
      LINECAPSCLASS_ALL                       = $000000ff;

// Line equipment states
      LINEEQUIPSTATE_MINIMUM                  = $00000001;
      LINEEQUIPSTATE_RXONLY                   = $00000002;
      LINEEQUIPSTATE_TXONLY                   = $00000003;
      LINEEQUIPSTATE_NOTXRX                   = $00000004;
      LINEEQUIPSTATE_FULL                     = $00000005;

// Line GPRS class types
      LINEGPRSCLASS_GSMANDGPRS                = $00000001;
      LINEGPRSCLASS_GSMORGPRS                 = $00000002;
      LINEGPRSCLASS_GSMORGPRS_EXCLUSIVE       = $00000003;
      LINEGPRSCLASS_GPRSONLY                  = $00000004;
      LINEGPRSCLASS_GSMONLY                   = $00000005;

// Line GPRS class changed types
      LINEGPRSCLASSCHANGED_NETWORK            = $00000001;
      LINEGPRSCLASSCHANGED_RADIO              = $00000002;

// Line HSCSD air-rates
      LINEHSCSDAIRRATE_9600                   = $00000001;
      LINEHSCSDAIRRATE_14400                  = $00000002;
      LINEHSCSDAIRRATE_19200                  = $00000003;
      LINEHSCSDAIRRATE_28800                  = $00000004;
      LINEHSCSDAIRRATE_38400                  = $00000005;
      LINEHSCSDAIRRATE_43200                  = $00000006;
      LINEHSCSDAIRRATE_57600                  = $00000007;

// Line HSCSD codings
      LINEHSCSDCODING_4800                    = $00000001;
      LINEHSCSDCODING_9600                    = $00000002;
      LINEHSCSDCODING_14400                   = $00000004;

// Line mute states
      LINEMUTESTATE_MUTEENABLED               = $00000001;
      LINEMUTESTATE_MUTEDISABLED              = $00000002;

// Line operator formats
      LINEOPFORMAT_NONE                       = $00000000;
      LINEOPFORMAT_ALPHASHORT                 = $00000001;
      LINEOPFORMAT_ALPHALONG                  = $00000002;
      LINEOPFORMAT_NUMERIC                    = $00000004;
      LINEOPFORMAT_ACCESS_TYPE                = $00000008;

// Line operator statuses
      LINEOPSTATUS_UNKNOWN                    = $00000000;
      LINEOPSTATUS_AVAILABLE                  = $00000001;
      LINEOPSTATUS_CURRENT                    = $00000002;
      LINEOPSTATUS_FORBIDDEN                  = $00000003;

// Line radio presence states
      LINERADIOPRESENCE_PRESENT               = $00000001;
      LINERADIOPRESENCE_NOTPRESENT            = $00000002;

// Line radio support states
      LINERADIOSUPPORT_OFF                    = $00000001;
      LINERADIOSUPPORT_ON                     = $00000002;
      LINERADIOSUPPORT_UNKNOWN                = $00000003;

// Line register modes
      LINEREGMODE_AUTOMATIC                   = $00000001;
      LINEREGMODE_MANUAL                      = $00000002;
      LINEREGMODE_MANAUTO                     = $00000003;

// Line register status
      LINEREGSTATUS_UNKNOWN                   = $00000001;
      LINEREGSTATUS_DENIED                    = $00000002;
      LINEREGSTATUS_UNREGISTERED              = $00000003;
      LINEREGSTATUS_ATTEMPTING                = $00000004;
      LINEREGSTATUS_HOME                      = $00000005;
      LINEREGSTATUS_ROAM                      = $00000006;
      LINEREGSTATUS_DIGITAL                   = $00000007;
      LINEREGSTATUS_ANALOG                    = $00000008;

// Line send caller-ID states
      LINESENDCALLERID_ENABLED                = $00000001;
      LINESENDCALLERID_DISABLED               = $00000002;

// Line system types, CDMA
      LINESYSTEMTYPE_NONE                     = $00000000;
      LINESYSTEMTYPE_IS95A                    = $00000001;
      LINESYSTEMTYPE_IS95B                    = $00000002;
      LINESYSTEMTYPE_1XRTTPACKET              = $00000004;
// Line system types, GSM
      LINESYSTEMTYPE_GSM                      = $00000008;
      LINESYSTEMTYPE_GPRS                     = $00000010;
// Line system types, GSM, CDMA, UMTS, HSDPA
      LINESYSTEMTYPE_EDGE                     = $00000020;
      LINESYSTEMTYPE_1XEVDOPACKET             = $00000040;
      LINESYSTEMTYPE_1XEVDVPACKET             = $00000080;
      LINESYSTEMTYPE_UMTS                     = $00000100;
      LINESYSTEMTYPE_HSDPA                    = $00000200;

// Line USSD flags
      LINEUSSDFLAG_ACTIONREQUIRED             = $00000001;
      LINEUSSDFLAG_ACTIONNOTNEEDED            = $00000002;
      LINEUSSDFLAG_TERMINATED                 = $00000004;
      LINEUSSDFLAG_OTHERCLIENTRESPONDED       = $00000008;
      LINEUSSDFLAG_UNSUPPORTED                = $00000010;
      LINEUSSDFLAG_TIMEOUT                    = $00000020;
      LINEUSSDFLAG_ENDSESSION                 = $00000040;

// Special LINEOPERATOR index values
      LINEOPERATOR_USEFIRSTAVAILABLEINDEX     = -1;

// Line operator statuses
      LINEACCESSTYPE_UNKNOWN                  = $00000000;
      LINEACCESSTYPE_UMTS                     = $00000001;
      LINEACCESSTYPE_GSM                      = $00000002;
      LINEACCESSTYPE_GSM_COMPACT              = $00000003;

// Maximum string lengths
      MAX_LENGTH_OPERATOR_LONG                = 32;
      MAX_LENGTH_OPERATOR_SHORT               = 16;
      MAX_LENGTH_OPERATOR_NUMERIC             = 16;


// LINE_DEVSPECIFIC message types
      LINE_EQUIPSTATECHANGE                   = $00000100;
    // dwParam1 = LINE_EQUIPSTATECHANGE
    // dwParam2 = One of the LINEEQUIPSTATE_* constants
    // dwParam3 = One of the LINERADIOSUPPORT_* constants

      LINE_GPRSCLASS                          = $00000101;
    // dwParam1 = LINE_GPRSCLASS
    // dwParam2 = One of the LINEGPRSCLASS_* constants
    // dwParam3 = One of the LINEGPRSCLASSCHANGED_* constants

      LINE_GPRSREGISTERSTATE                  = $00000102;
    // dwParam1 = LINE_GPRSREGISTERSTATE
    // dwParam2 = One of the LINEREGSTATUS_* constants
    // dwParam3 Unused

      LINE_RADIOPRESENCE                      = $00000103;
    // dwParam1 = LINE_RADIOPRESENCE
    // dwParam2 = One of the LINERADIOPRESENCE_* constants
    // dwParam3 Unused

      LINE_REGISTERSTATE                      = $00000104;
    // dwParam1 = LINE_REGISTERSTATE
    // dwParam2 = One of the LINEREGSTATUS_* constants
    // dwParam3 Unused

      LINE_USSD                               = $00000105;
    // dwParam1 = LINE_USSD
    // dwParam2 = Message identifier
    // dwParam3 = Size in bytes of message

      LINE_CURRENTLINECHANGE                  = $00000106;
    // dwParam1 = LINE_CURRENTLINECHANGE
    // dwParam2 = New line identifier
    // dwParam3 = New address ID

      LINE_CURRENTSYSTEMCHANGE                = $00000107;
    // dwParam1 = LINE_CURRENTSYSTEMCHANGE
    // dwParam2 = New system coverage (LINESYSTEMTYPE_*)
    // dwParam3 = unused

// Structures
type
     lineoperator_tag = record
       dwIndex:DWORD;
       dwValidFields:DWORD;
       dwStatus:DWORD;
       lpszLongName:array[0..MAX_LENGTH_OPERATOR_LONG-1] of TCHAR;
       lpszShortName:array[0..MAX_LENGTH_OPERATOR_SHORT-1] of TCHAR;
       lpszNumName:array[0..MAX_LENGTH_OPERATOR_NUMERIC-1] of TCHAR;
     end;
     LINEOPERATOR = lineoperator_tag;
     LPLINEOPERATOR = ^lineoperator_tag;

     lineoperatorex_tag = record
       cbSize:DWORD;
       dwIndex:DWORD;
       dwValidFields:DWORD;
       dwStatus:DWORD;
       lpszLongName:array[0..MAX_LENGTH_OPERATOR_LONG-1] of TCHAR;
       lpszShortName:array[0..MAX_LENGTH_OPERATOR_SHORT-1] of TCHAR;
       lpszNumName:array[0..MAX_LENGTH_OPERATOR_NUMERIC-1] of TCHAR;
       dwAccessType:DWORD;
     end;
     LINEOPERATOREX = lineoperatorex_tag;
     LPLINEOPERATOREX = ^lineoperatorex_tag;

     lineoperatorstatus_tag = record
       dwTotalSize:DWORD;
       dwNeededSize:DWORD;
       dwUsedSize:DWORD;
       dwPreferredCount:DWORD;
       dwPreferredSize:DWORD;
       dwPreferredOffset:DWORD;
       dwAvailableCount:DWORD;
       dwAvailableSize:DWORD;
       dwAvailableOffset:DWORD;
     end;
     LINEOPERATORSTATUS = lineoperatorstatus_tag;
     LPLINEOPERATORSTATUS = ^lineoperatorstatus_tag;

     linegeneralinfo_tag = record
       dwTotalSize:DWORD;
       dwNeededSize:DWORD;
       dwUsedSize:DWORD;
       dwManufacturerSize:DWORD;
       dwManufacturerOffset:DWORD;
       dwModelSize:DWORD;
       dwModelOffset:DWORD;
       dwRevisionSize:DWORD;
       dwRevisionOffset:DWORD;
       dwSerialNumberSize:DWORD;
       dwSerialNumberOffset:DWORD;
       dwSubscriberNumberSize:DWORD;
       dwSubscriberNumberOffset:DWORD;
     end;
     LINEGENERALINFO = linegeneralinfo_tag;
     LPLINEGENERALINFO = ^linegeneralinfo_tag;

// Functions
const
      ExTapiDLL = 'cellcore.dll';

function lineGetCallBarringCaps(hLine:HLINE;
                                lpdwModes:LPDWORD;
                                lpdwClasses:LPDWORD):LONG; external ExTapiDLL name 'lineGetCallBarringCaps';

function lineGetCallBarringState(hLine:HLINE;
                                 dwMode:DWORD;
                                 lpdwClasses:LPDWORD;
                                 lpszPassword:LPCTSTR):LONG; external ExTapiDLL name 'lineGetCallBarringState';

function lineGetCallWaitingCaps(hLine:HLINE;
                                lpdwClasses:LPDWORD):LONG; external ExTapiDLL name 'lineGetCallWaitingCaps';

function lineGetCallWaitingState(hLine:HLINE;
                                 lpdwClasses:LPDWORD):LONG; external ExTapiDLL name 'lineGetCallWaitingState';

function lineGetCurrentAddressID(hLine:HLINE;
                                 lpdwAddressID:LPDWORD):LONG; external ExTapiDLL name 'lineGetCurrentAddressID';

function lineGetCurrentHSCSDStatus(hLine:HLINE;
                                   lpdwChannelsIn:LPDWORD;
                                   lpdwChannelsOut:LPDWORD;
                                   lpdwChannelCoding:LPDWORD;
                                   lpdwAirInterfaceRate:LPDWORD):LONG; external ExTapiDLL name 'lineGetCurrentHSCSDStatus';

function lineGetCurrentOperator(hLine:HLINE;
                                lpCurrentOperator:LPLINEOPERATOR):LONG; external ExTapiDLL name 'lineGetCurrentOperator';

function lineGetCurrentOperatorEx(hLine:HLINE;
                                  lpCurrentOperatorEx:LPLINEOPERATOREX):LONG; external ExTapiDLL name 'lineGetCurrentOperatorEx';

function lineGetCurrentSystemType(hLine:HLINE;
                                  lpdwCurrentSystemType:LPDWORD):LONG; external ExTapiDLL name 'lineGetCurrentSystemType';

function lineGetEquipmentState(hLine:HLINE;
                               lpdwState:LPDWORD;
                               lpdwRadioSupport:LPDWORD):LONG; external ExTapiDLL name 'lineGetEquipmentState';

function lineGetGeneralInfo(hLine:HLINE;
                            lpLineGeneralInfo:LPLINEGENERALINFO):LONG; external ExTapiDLL name 'lineGetGeneralInfo';

function lineGetGPRSClass(hLine:HLINE;
                          lpdwClass:LPDWORD):LONG; external ExTapiDLL name 'lineGetGPRSClass';

function lineGetHSCSDCaps(hLine:HLINE;
                          lpdwClass:LPDWORD;
                          lpdwChannelsIn:LPDWORD;
                          lpdwChannelsOut:LPDWORD;
                          lpdwChannelsSum:LPDWORD;
                          lpdwChannelCodings:LPDWORD):LONG; external ExTapiDLL name 'lineGetHSCSDCaps';

function lineGetHSCSDState(hLine:HLINE;
                           lpdwChannelsIn:LPDWORD;
                           lpdwMaxChannelsIn:LPDWORD;
                           lpdwChannelCodings:LPDWORD;
                           lpdwAirInterfaceRate:LPDWORD):LONG; external ExTapiDLL name 'lineGetHSCSDState';

function lineGetMuteState(hLine:HLINE;
                          lpdwState:LPDWORD):LONG; external ExTapiDLL name 'lineGetMuteState';

function lineGetNumberCalls(hLine:HLINE;
                            lpdwNumActiveCalls:LPDWORD;
                            lpdwNumOnHoldCalls:LPDWORD;
                            lpdwNumOnHoldPendCalls:LPDWORD):LONG; external ExTapiDLL name 'lineGetNumberCalls';

function lineGetOperatorStatus(hLine:HLINE;
                               lpOperatorStatus:LPLINEOPERATORSTATUS):LONG; external ExTapiDLL name 'lineGetOperatorStatus';

function lineGetOperatorStatusEx(hLine:HLINE;
                                 lpOperatorStatus:LPLINEOPERATORSTATUS):LONG; external ExTapiDLL name 'lineGetOperatorStatusEx';

function lineGetRadioPresence(hLine:HLINE;
                              lpdwRadioPresence:LPDWORD):LONG; external ExTapiDLL name 'lineGetRadioPresence';

function lineGetRegisterStatus(hLine:HLINE;
                               lpdwRegisterStatus:LPDWORD):LONG; external ExTapiDLL name 'lineGetRegisterStatus';

function lineGetSendCallerIDState(hLine:HLINE;
                                  lpdwState:LPDWORD):LONG; external ExTapiDLL name 'lineGetSendCallerIDState';

function lineGetUSSD(hLine:HLINE;
                     dwID:DWORD;
                     lpbUSSD:LPBYTE;
                     dwUSSDSize:DWORD;
                     lpdwFlags:LPDWORD):LONG; external ExTapiDLL name 'lineGetUSSD';

function lineRegister(hLine:HLINE;
                      dwRegisterMode:DWORD;
                      lpszOperator:LPCTSTR;
                      dwOperatorFormat:DWORD):LONG; external ExTapiDLL name 'lineRegister';

function lineRegisterEx(hLine:HLINE;
                        dwRegisterMode:DWORD;
                        lpOperatorEx:LPLINEOPERATOREX):LONG; external ExTapiDLL name 'lineRegisterEx';

function lineSendUSSD(hLine:HLINE;
                      lpbUSSD:LPBYTE;
                      dwUSSDSize:DWORD;
                      dwFlags:DWORD):LONG; external ExTapiDLL name 'lineSendUSSD';

function lineSetCallBarringPassword(hLine:HLINE;
                                    dwMode:DWORD;
                                    lpszOldPassword:LPCTSTR;
                                    lpszNewPassword:LPCTSTR):LONG; external ExTapiDLL name 'lineSetCallBarringPassword';

function lineSetCallBarringState(hLine:HLINE;
                                 dwMode:DWORD;
                                 dwClasses:DWORD;
                                 lpszPassword:LPCTSTR):LONG; external ExTapiDLL name 'lineSetCallBarringState';

function lineSetCallWaitingState(hLine:HLINE;
                                 dwClasses:DWORD;
                                 dwState:DWORD):LONG; external ExTapiDLL name 'lineSetCallWaitingState';

function lineSetCurrentAddressID(hLine:HLINE;
                                 dwAddressID:DWORD):LONG; external ExTapiDLL name 'lineSetCurrentAddressID';

function lineSetEquipmentState(hLine:HLINE;
                               dwState:DWORD):LONG; external ExTapiDLL name 'lineSetEquipmentState';

function lineSetGPRSClass(hLine:HLINE;
                          dwClass:DWORD):LONG; external ExTapiDLL name 'lineSetGPRSClass';

function lineSetHSCSDState(hLine:HLINE;
                           dwChannelsIn:DWORD;
                           dwMaxChannelsIn:DWORD;
                           dwChannelCodings:DWORD;
                           dwAirInterfaceRate:DWORD):LONG; external ExTapiDLL name 'lineSetHSCSDState';

function lineSetMuteState(hLine:HLINE;
                          dwState:DWORD):LONG; external ExTapiDLL name 'lineSetMuteState';

function lineSetSendCallerIDState(hLine:HLINE;
                                  dwState:DWORD):LONG; external ExTapiDLL name 'lineSetSendCallerIDState';

function lineSetPreferredOperator(hLine:HLINE;
                                  lpOperator:LPLINEOPERATOR):LONG; external ExTapiDLL name 'lineSetPreferredOperator';

function lineSetPreferredOperatorEx(hLine:HLINE;
                                    lpOperatorEx:LPLINEOPERATOREX):LONG; external ExTapiDLL name 'lineSetPreferredOperatorEx';

function lineUnregister(hLine:HLINE):LONG; external ExTapiDLL name 'lineUnregister';

implementation

end.